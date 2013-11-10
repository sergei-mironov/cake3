{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Development.Cake3.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Loc
import Data.Data
import Data.Typeable
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import Data.List as L hiding (foldl')
import Data.Either
import Data.Foldable (foldl')
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import Development.Cake3.Types
import qualified System.IO as IO
import Text.Printf

import System.FilePath.Wrapper


type Location = String

data MakeState = MS {
    recipes :: Set Recipe
  , sloc :: Location
  , makeDeps :: Set File
  , placement :: [File]
  , subprojects :: Map File (Set File)
  , includes :: Set File
  , errors :: String
  , warnings :: String
  } deriving(Show, Data, Typeable)

initialMakeState = MS mempty mempty mempty mempty mempty mempty mempty mempty

getPlacementPos :: Make Int
getPlacementPos = L.length <$> placement <$> get

addPlacement :: Int -> File -> Make ()
addPlacement pos r = modify $ \ms -> ms { placement = r`insertInto`(placement ms) } where
  insertInto x xs = let (h,t) = splitAt pos xs in h ++ (x:t)

-- State that target Makefile depends on a File f.
addMakeDep :: File -> Make ()
addMakeDep f = modify (\ms -> ms { makeDeps = S.insert f (makeDeps ms) })

-- State that subproject p (a directory with it's own Makefile) manages file f
addSubproject :: File -> File -> Make ()
addSubproject f p = modify (\ms -> ms { subprojects = M.insertWith mappend p (S.singleton f) (subprojects ms) })

queryVariables :: MakeState -> Set Variable
queryVariables ms = F.foldl' (\a r -> a`mappend`(rvars r)) mempty (recipes ms)

queryVariablesE :: MakeState -> Either String (Set Variable)
queryVariablesE ms = check where
  vs = queryVariables ms
  bads = M.filter (\s -> (S.size s) /= 1) (groupSet (\v -> S.singleton (vname v)) vs)
  check | (M.size bads) > 0 = Left "Some variables share same name"
        | otherwise = Right vs

checkForEmptyTarget :: MakeState -> String
checkForEmptyTarget ms = foldl' checker mempty (recipes ms) where
  checker es r | S.null (rtgt r) = es++e
               | otherwise = es where
    e = printf "Error: Recipe without targets\n\t%s\n" (show r)

checkForTargetConflicts :: MakeState -> String
checkForTargetConflicts ms = foldl' checker mempty (groupRecipes (recipes ms)) where
  checker es rs | S.size rs > 1 = es++e
                | otherwise = es where
    e = printf "Error: Recipes share one or more targets\n\t%s\n" (show rs)

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO, MonadFix)

-- | Returns either error or a tuple containing: 1) the set of all variables 2) the set of recipes
-- 3) the user-defined order of targets in the final Makefile
evalMake :: Make a -> IO MakeState
evalMake mk = do
  ms <- flip execStateT initialMakeState (unMake mk)
  return ms {
    errors = checkForEmptyTarget ms ++ checkForTargetConflicts ms
  }

modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

addRecipe :: Recipe -> Make ()
addRecipe r = modify $ \ms -> 
  let rs = recipes ms ; k = rtgt r
  in ms { recipes = (S.insert r (recipes ms)) }

getLoc :: Make String
getLoc = sloc <$> get

include :: File -> Make ()
include f = modify $ \ms -> ms {includes = S.insert f (includes ms)}

instance MonadLoc Make where
  withLoc l' (Make um) = Make $ do
    modifyLoc (\l -> l') >> um

newtype A a = A { unA :: StateT Recipe Make a }
  deriving(Monad, Functor, Applicative, MonadState Recipe, MonadIO,MonadFix)

addVariable :: Variable -> A ()
addVariable v = modify $ \r -> r { rvars = S.insert v (rvars r) }

targets :: A (Set File)
targets = rtgt <$> get

prerequisites :: A (Set File)
prerequisites = rsrc <$> get

runA :: A a -> Make (Recipe, a)
runA act = do
  loc <- getLoc
  let template = Recipe mempty mempty [] mempty loc False
  (a,r) <- runStateT (unA act) template
  return (r,a)

markPhony :: A ()
markPhony = modify $ \r -> r { rphony = True }

liftMake :: Make a -> A a
liftMake mk = A (lift mk)

readFile :: File -> A String
readFile f = do
  A (lift $ addMakeDep f)
  liftIO (IO.readFile (unpack f))

