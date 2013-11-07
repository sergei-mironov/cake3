{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Development.Cake3.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Loc
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import Data.List as L
import Data.Either
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import Development.Cake3.Types
import qualified System.IO as IO
import Text.Printf

import System.FilePath.Wrapper


type Location = String

data MakeState = MS {
    srecipes :: Map Target (Set Recipe1)
  , sloc :: Location
  , makeDeps :: Set File
  , placement :: [Target]
  , subprojects :: Map File (Set File)
  } deriving(Show)

addPlacement :: RecipeT x -> Make ()
addPlacement r = modify $ \ms -> ms { placement = (placement ms) ++ [rtgt r] }

modifyRecipes f = modify $ \ms -> ms { srecipes = f (srecipes ms) }

-- State that contents of the target Makefile depends on a File specified.
addMakeDep :: File -> Make ()
addMakeDep f = modify (\ms -> ms { makeDeps = S.insert f (makeDeps ms) })

-- State that subproject (a directory with it's own Makefile) manages the file f
addSubproject :: File -> File -> Make ()
addSubproject f p = modify (\ms -> ms { subprojects = M.insertWith mappend p (S.singleton f) (subprojects ms) })

-- Default Make state
defMS = MS mempty mempty mempty mempty mempty

-- | The File Alias records the file which may be referenced from other rules,
-- it's "Brothers", and the recipes required to build this file.
newtype Alias = Alias (File, [File], Make Recipe)

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO, MonadFix)

makefileT :: (FileLike x) => (FileT x)
makefileT = fromFilePath "Makefile"

addRebuildDeps :: Set File -> Map Target Recipe2 -> Map Target Recipe2
addRebuildDeps md rs = M.map mkd rs where
  mkd r | makefileT `S.member` (rtgt r) = r{ rsrc = ((rsrc r) `mappend` md) }
        | otherwise = r

isRequiredFor :: Map Target (RecipeT x) -> (RecipeT x) -> File -> Bool
isRequiredFor rs r f = if f`S.member`(rtgt r) then True else godeeper where
  godeeper = or $ map (\tgt -> or $ map (\r -> isRequiredFor rs r f) (selectBySrc tgt)) (S.toList $ rtgt r)
  selectBySrc f = map snd . M.toList . fst $ M.partition (\r -> f`S.member`(rsrc r)) rs

-- | There are only 2 kind of rules: 1) ones that depend on a Makefile, and 2) ones
-- that Makefile depends on. Case-2 is known in advance (for example, when the
-- the contents of a file is required to build a Makefile then Makefile depends
-- on this file). This function adds the case-1 dependencies.
addMakeDeps :: Map Target Recipe2 -> Map Target Recipe2
addMakeDeps rs
  | M.null makeRules = rs
  | otherwise = M.map addMakeDeps_ rs
  where
    makeRules = M.filter (\r -> makefileT `S.member` (rtgt r)) rs
    isRequiredFor_ = isRequiredFor rs
    addMakeDeps_ r | not (r `isRequiredFor_` makefileT) = r{ rsrc = (S.insert makefileT (rsrc r)) }
                   | otherwise = r

flattern :: (Ord x, Ord y, Show y) => Map x (Set y) -> Either String (Map x y)
flattern m = mapM check1 (M.toList m) >>= \m -> return (M.fromList m) where
  check1 (k,s) = do
    case S.size s of
      1 -> return (k, S.findMin s)
      _ -> fail $ printf "More than 1 value describes single entity: %s" (show s)

flattern' :: (Ord x, Ord y, Show y) => Map x (Set y) -> Map x y
flattern' m = M.map check1 m where
  check1 s = do
    case S.size s of
      1 -> S.findMin s
      _ -> error $ printf "More than 1 value describes single entity: %s" (show s)

check :: Map Target (Set Recipe1) -> Either String (Map String Variable, Map Target Recipe2)
check rs1 = do
  rs1' <- flattern rs1
  let vs = F.foldr (\b a -> M.unionWith mappend a (rvars b)) mempty rs1'
  vs' <- flattern vs
  let rs2 = M.map (\(Recipe a b c d e f) -> let d' = flattern' d in (Recipe a b c d' e f)) rs1'
  return (vs',rs2)

-- | Returns either error or a tuple containing: 1) the set of all variables 2) the set of recipes
-- 3) the user-defined order of targets in the final Makefile
evalMake :: Make () -> IO (Either String (Map String Variable, Map Target Recipe2, [Target]))
evalMake mk = do
  flip evalStateT defMS $ unMake $ mk >> do
    md <- makeDeps <$> get
    rs <- srecipes <$> get
    p <- placement <$> get
    sp <- subprojects <$> get
    return $ case check rs of
      Left err -> Left err
      Right (v,r) -> Right (v, addMakeDeps $ addRebuildDeps md $ r, p)

modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

addRecipe :: Recipe1 -> Make ()
addRecipe r = modify $ \ms -> 
  let rs = srecipes ms ; k = rtgt r
  in ms { srecipes = (M.unionWith mappend (M.singleton k (S.singleton r)) rs) }

getLoc :: Make String
getLoc = sloc <$> get

instance MonadLoc Make where
  withLoc l' (Make um) = Make $ do
    modifyLoc (\l -> l') >> um

newtype A a = A { unA :: StateT Recipe1 Make a }
  deriving(Monad, Functor, Applicative, MonadState Recipe1, MonadIO,MonadFix)

addVariable :: Variable -> A ()
addVariable v = modify $ \r -> r { rvars = M.insertWith mappend (vname v) (S.singleton v) (rvars r) }

targets :: A (Set File)
targets = rtgt <$> get

prerequisites :: A (Set File)
prerequisites = rsrc <$> get

runA :: Recipe1 -> A a -> Make Recipe1
runA r a = do
  r' <- snd <$> runStateT (unA a) r
  addRecipe r'
  return r'

readFile :: File -> A String
readFile f = do
  A (lift $ addMakeDep f)
  liftIO (IO.readFile (unpack f))

class Placable a where
  place :: a -> Make ()

instance Placable Alias where
  place (Alias (_,_,x)) = do
    x >>= addPlacement

instance Placable (Alias,Alias) where
  place (a,b) = place a >> place b

instance Placable (Alias,Alias,Alias) where
  place (a,b,c) = place a >> place b >> place c

instance Placable (Alias,Alias,Alias,Alias) where
  place (a,b,c,d) = place a >> place b >> place c >> place d

instance Placable x => Placable (Make x) where
  place mk = mk >>= place

instance Placable x => Placable [x] where
  place xs = sequence_ (map place xs)

