{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Development.Cake3.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Loc
import Data.Monoid
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import Data.List as L
import Data.Either
import qualified Data.Foldable as F
import Development.Cake3.Types
import Text.Printf

import System.FilePath.Wrapper

type Recipe = Recipe1

type Recipe1 = RecipeT (Map String [Variable]) File1

type Recipe2 = RecipeT (Map String [Variable]) File2

type Recipe3 = RecipeT [Variable] File2

type File = File1

type File1 = FileT (ReactFile Make)

type File2 = FileT FilePath

type Location = String

type Recipes = [Recipe1]

data MakeState = MS {
    srecipes :: Recipes
  , sloc :: Location
  , spos :: Int
  , fileCache :: FileCache
  , makeDeps :: Set File2
  } deriving(Show)

defMS = MS mempty mempty 0 emptyFileCache S.empty

-- | The File Alias records the file which may be referenced from other rules,
-- it's "Brothers", and the recipes required to build this file.
newtype Alias = Alias (File1, [File1], Make ())

unalias :: [Alias] -> Make ()
unalias as = F.sequence_ $ map (\(Alias (_,_,x)) -> x) as

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO)

instance FileCacheMonad Make Make where
  cache (FileT (ReactFile act)) = FileT <$> act

  readCachedFile f = do
    fp <- cache f
    modify $ \s -> s { makeDeps = S.insert fp (makeDeps s) }
    -- FIXME: has to catch exceptions
    Just <$> liftIO (readFile (unpack fp))

makefileT :: (FileLike x) => (FileT x)
makefileT= fromFilePath "Makefile"

-- | Adds Makefile dependencies to the set of rules. Take into account possible
-- explicit Make-dependencies defined by the user
addMakeDeps :: [File2] -> [Recipe3] -> [Recipe3]
addMakeDeps md rs_ = reverse $ L.foldl adder [] rs  where
  rs = L.foldl mkd [] rs_ where
  mkd rs r | makefileT`L.elem`(rtgt r) = r{ rsrc = ((rsrc r) ++ md) }:rs
           | otherwise = r:rs
  adder rs r | not(makefileT`dependsOn`r) = r{ rsrc = (makefileT:(rsrc r)) }:rs
             | otherwise = r:rs
  dependsOn :: File2 -> Recipe3 -> Bool
  dependsOn f r = if f`L.elem`(rtgt r) then True else godeeper where
    godeeper = or $ map (\tgt -> or $ map (dependsOn f) (selectBySrc tgt)) (rtgt r)
  selectBySrc f = fst $ partition (\r -> f`elem`(rsrc r)) rs

flatternBy :: (Eq y, Ord x, Show y) => (y -> y -> Bool) -> Map x [y] -> ([String], [y])
flatternBy cmp m = F.foldr check1 mempty m where
  check1 s_ (errs,ss) =
    let s = L.nubBy cmp s_ in
    case L.length s of
      1 -> (errs, (head s):ss)
      _ -> let e = printf "More than 1 value describes single entity: %s" (show s)
               in (e:errs, ss)

type ErrorMsg = String

check :: (Map [File2] [Recipe2]) -> ([ErrorMsg], [Variable], [Recipe3])
check rs = (es1 ++ es2, vs', sorted_rs) where
  (es1, rs') = flatternBy (\a b -> a{pos=0}==b{pos=0}) rs
  vs = F.foldr (\rs acc -> M.unionWith mappend (rvars rs) acc) mempty rs'
  (es2, vs') = flatternBy (==) vs
  rs'' = map (\(Recipe p a b c d e f) -> Recipe p a b c (snd (flatternBy (==) d)) e f) rs'
  sorted_rs = sortBy (\a b -> (pos b)`compare`(pos a)) rs''

unfiles :: [Recipe1] -> Make (Map [File2] [Recipe2])
unfiles r2s = F.foldlM adder mempty r2s where
  adder m (Recipe p as bs cs d e f) = do
    as' <- mapM cache as
    bs' <- mapM cache bs
    cs' <- mapM (mapM (either (\x -> Left <$> return x) (\x -> Right <$> cache x))) cs
    return $ M.insertWith mappend as' [Recipe p as' bs' cs' d e f] m

evalMake :: [Alias] -> IO ([ErrorMsg], ([Variable], [Recipe3]))
evalMake aliases = do
  flip evalStateT defMS $ unMake $ do
    unalias aliases
    md <- S.toList <$> makeDeps <$> get
    r2 <- ((srecipes <$> get) >>= unfiles)
    let (e,v,r3) = check r2
    return (e,(v,addMakeDeps md r3))

modifyRecipes f = modify $ \ms -> ms { srecipes = f (srecipes ms) }

modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

getPos :: Make Int
getPos = do
  p <- spos <$> get
  modify $ \ms -> ms { spos = p + 1 }
  return p

addRecipe :: Recipe1 -> Make ()
addRecipe r = modifyRecipes (r:)

getLoc :: Make String
getLoc = sloc <$> get

instance MonadLoc Make where
  withLoc l' (Make um) = Make $ do
    modifyLoc (\l -> l') >> um

newtype A a = A { unA :: StateT Recipe1 Make a }
  deriving(Monad, Functor, Applicative, MonadState Recipe1, MonadIO)

instance FileCacheMonad A Make where
  cache f = A (lift (cache f))
  readCachedFile f = A (lift (readCachedFile f))

targets :: A [File1]
targets = rtgt <$> get

prerequisites :: A [File1]
prerequisites = rsrc <$> get

runA :: Recipe1 -> A a -> Make ()
runA r a = runStateT (unA a) r >>= addRecipe . snd

