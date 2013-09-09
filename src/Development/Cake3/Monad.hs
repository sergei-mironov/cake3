{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.List
import Data.Foldable as F
import Development.Cake3.Types
import Text.Printf

import System.FilePath.Wrapper

type Location = String

data MakeState = MS {
    srecipes :: Recipes
  , sloc :: Location
  , spos :: Int
  } deriving(Show)

defMS = MS mempty mempty 0

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO)

runMake :: [Alias] -> IO Recipes
runMake makes = srecipes <$> execStateT (unMake (unalias $ reverse makes)) defMS

addRecipe :: Recipe -> Make ()
addRecipe r = do
  p <- getPos
  let pr = Positioned p r
  F.forM_ (rtgt r) $ \tgt -> do
    modifyRecipes $ M.insertWith mappend tgt (S.singleton pr)

modifyRecipes f = modify $ \ms -> ms { srecipes = f (srecipes ms) }
modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

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

newtype A a = A { unA :: StateT Recipe Make a }
  deriving(Monad, Functor, Applicative, MonadState Recipe, MonadIO)

runA :: Recipe -> A a -> Make ()
runA r a = runStateT (unA a) r >>= addRecipe . snd


flattern :: (Ord y, Show y) => Map k (Set y) -> ([String], [y])
flattern m = F.foldr check1 mempty m where
  check1 s (errs,ss) =
    case (S.size s == 1) of
      True -> (errs, (S.toList s) ++ ss)
      False -> let e = printf "More than 1 value describes single entity: %s" (show s)
               in (e:errs, ss)

check :: Recipes -> ([String], [Variable], [CheckedRecipe])
check rs = (es1 ++ es2, vs', sorted_rs) where
  (es1, rs') = flattern rs
  vs = F.foldr (\(Positioned _ rs) acc -> M.unionWith mappend (rvars rs) acc) mempty rs'
  (es2, vs') = flattern vs
  rs'' = map (\(Positioned p (Recipe a b c d e f)) -> Positioned p (Recipe a b c (snd (flattern d)) e f)) rs'
  sorted_rs = map unposition (sortBy cmpPos rs'')

-- | The File Alias records the file which may be referenced from other rules,
-- it's "Brothers", and the recipes required to build this file.
newtype Alias = Alias (File, [File], Make ())

unalias :: [Alias] -> Make ()
unalias as = F.sequence_ $ map (\(Alias (_,_,x)) -> x) as

instance AsFile Alias where
  toFile (Alias (f,_,_) ) = f


