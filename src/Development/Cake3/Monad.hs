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
    recipes :: Set Recipe
  , sloc :: Location
  , makeDeps :: Set File
  , placement :: [File]
  , subprojects :: Map File (Set File)
  } deriving(Show, Data, Typeable)

addPlacement :: File -> Make ()
addPlacement r = modify $ \ms -> ms { placement = (placement ms) ++ [r] }

-- State that target Makefile depends on a File f.
addMakeDep :: File -> Make ()
addMakeDep f = modify (\ms -> ms { makeDeps = S.insert f (makeDeps ms) })

-- State that subproject p (a directory with it's own Makefile) manages file f
addSubproject :: File -> File -> Make ()
addSubproject f p = modify (\ms -> ms { subprojects = M.insertWith mappend p (S.singleton f) (subprojects ms) })

initialMakeState = MS mempty mempty mempty mempty mempty

queryVariables :: MakeState -> Set Variable
queryVariables ms = F.foldl' (\a r -> a`mappend`(rvars r)) mempty (recipes ms)

queryVariablesE :: MakeState -> Either String (Set Variable)
queryVariablesE ms = check where
  vs = queryVariables ms
  bads = M.filter (\s -> (S.size s) /= 1) (groupSet (\v -> [vname v]) vs)
  check | (M.size bads) > 0 = Left "Some variables share same name"
        | otherwise = Right vs

-- | The File Alias represents 1) the file which may be referenced from other rules,
-- 2) it's "Brothers", and 3) the recipe required to build this file and it's
-- brothers.
newtype Alias = Alias (File, [File], Make Recipe)

newtype Make a = Make { unMake :: (StateT MakeState IO) a }
  deriving(Monad, Functor, Applicative, MonadState MakeState, MonadIO, MonadFix)

-- flattern :: (Ord x, Ord y, Show y) => Map x (Set y) -> Either String (Map x y)
-- flattern m = mapM check1 (M.toList m) >>= \m -> return (M.fromList m) where
--   check1 (k,s) = do
--     case S.size s of
--       1 -> return (k, S.findMin s)
--       _ -> fail $ printf "More than 1 value describes single entity: %s" (show s)

-- flattern' :: (Ord x, Ord y, Show y) => Map x (Set y) -> Map x y
-- flattern' m = M.map check1 m where
--   check1 s = do
--     case S.size s of
--       1 -> S.findMin s
--       _ -> error $ printf "More than 1 value describes single entity: %s" (show s)

-- check :: Map Target (Set Recipe) -> Either String (Map String Variable, Map Target Recipe)
-- check rs1 = do
--   rs1' <- flattern rs1
--   let vs = F.foldr (\b a -> M.unionWith mappend a (rvars b)) mempty rs1'
--   vs' <- flattern vs
--   let rs2 = M.map (\(Recipe a b c d e f) -> let d' = flattern' d in (Recipe a b c d' e f)) rs1'
--   return (vs',rs2)

-- | Returns either error or a tuple containing: 1) the set of all variables 2) the set of recipes
-- 3) the user-defined order of targets in the final Makefile
evalMake :: Make a -> IO MakeState
evalMake mk = flip execStateT initialMakeState (unMake mk)

modifyLoc f = modify $ \ms -> ms { sloc = f (sloc ms) }

addRecipe :: Recipe -> Make ()
addRecipe r = modify $ \ms -> 
  let rs = recipes ms ; k = rtgt r
  in ms { recipes = (S.insert r (recipes ms)) }

getLoc :: Make String
getLoc = sloc <$> get

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

runA :: (Location, Bool) -> A a -> Make (Recipe, a)
runA (loc,isPhony) act = do
  let template = Recipe mempty mempty [] mempty loc isPhony
  (a,r) <- runStateT (unA act) template
  addRecipe r
  return (r,a)

markPhony :: A ()
markPhony = modify $ \r -> r { rphony = True }

liftMake :: Make a -> A a
liftMake mk = A (lift mk)

readFile :: File -> A String
readFile f = do
  A (lift $ addMakeDep f)
  liftIO (IO.readFile (unpack f))

