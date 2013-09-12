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

type Recipe1 = RecipeT (Map String (Set Variable)) File1

type Recipe2 = RecipeT [Variable] File1

type Recipe3 = RecipeT [Variable] File2

type File = File1

type File1 = FileT (ReactFile Make)

type File2 = FileT FilePath

type Location = String

type Recipes = Map [File1] (Set (Positioned Recipe1))

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
  cache (FileT (ReactFile key act)) = do
    c <- fileCache <$> get
    case M.lookup key c of
      Just f -> return f
      Nothing -> do
        f <- FileT <$> act
        modify (\s -> s { fileCache = M.insert key f (fileCache s) } )
        return f

  readCachedFile f = do
    fp <- unpack <$> cache f
    modify $ \s -> s { makeDeps = S.insert (fromFilePath fp) (makeDeps s) }
    -- FIXME: need to catch exceptions
    Just <$> liftIO (readFile fp)

makefileT :: (FileLike x) => (FileT x)
makefileT= fromFilePath "Makefile"

-- | Adds Makefile dependencies to the set of rules. Take into account possible
-- explicit Make-dependencies defined by the user
-- Note: addMakeDeps is effectively a reverse due to foldl
addMakeDeps :: [File2] -> [Recipe3] -> [Recipe3]
addMakeDeps md rs = L.foldl adder [] rs where
  adder rs r | makefileT`L.elem`(rtgt r) = r{ rsrc = ((rsrc r) ++ md) }:rs
             | not(makefileT`dependsOn`r) = r{ rsrc = (makefileT:(rsrc r)) }:rs
             | otherwise = r:rs
  dependsOn :: File2 -> Recipe3 -> Bool
  dependsOn f r = if f`L.elem`(rtgt r) then True else godeeper where
    godeeper = or $ map (\tgt -> or $ map (dependsOn f) (selectBySrc tgt)) (rtgt r)
  selectBySrc f = fst $ partition (\r -> f`elem`(rsrc r)) rs

evalMake :: [Alias] -> IO ([ErrorMsg], ([Variable], [Recipe3]))
evalMake aliases = do
  flip evalStateT defMS $ unMake $ do
    unalias aliases
    md <- S.toList <$> makeDeps <$> get
    (e,v,r2) <- check <$> srecipes <$> get
    r3 <- unfiles r2
    return (e,(v,addMakeDeps md r3))

modifyRecipes f = modify $ \ms -> ms { srecipes = f (srecipes ms) }

modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

addRecipe :: Recipe1 -> Make ()
addRecipe r = do
  p <- getPos
  modifyRecipes $ M.insertWith mappend (rtgt r) (S.singleton (Positioned p r))

getLoc :: Make String
getLoc = sloc <$> get

getPos :: Make Int
getPos = do
  p <- spos <$> get
  modify $ \ms -> ms { spos = p + 1 }
  return p

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

flattern :: (Ord y, Show y) => Map k (Set y) -> ([String], [y])
flattern m = F.foldr check1 mempty m where
  check1 s (errs,ss) =
    case (S.size s == 1) of
      True -> (errs, (S.toList s) ++ ss)
      False -> let e = printf "More than 1 value describes single entity: %s" (show s)
               in (e:errs, ss)

type ErrorMsg = String

check :: Recipes -> ([ErrorMsg], [Variable], [Recipe2])
check rs = (es1 ++ es2, vs', sorted_rs) where
  (es1, rs') = flattern rs
  vs = F.foldr (\(Positioned _ rs) acc -> M.unionWith mappend (rvars rs) acc) mempty rs'
  (es2, vs') = flattern vs
  rs'' = map (\(Positioned p (Recipe a b c d e f)) -> Positioned p (Recipe a b c (snd (flattern d)) e f)) rs'
  sorted_rs = map unposition (sortBy cmpPos rs'')

unfiles :: [Recipe2] -> Make [Recipe3]
unfiles r2s = do
  forM r2s $ \(Recipe as bs cs d e f) -> do
    as' <- mapM cache as
    bs' <- mapM cache bs
    cs' <- mapM (mapM (either (\x -> Left <$> return x) (\x -> Right <$> cache x))) cs
    return (Recipe as' bs' cs' d e f)

