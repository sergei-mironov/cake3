{-# LANGUAGE DeriveDataTypeable #-}
module Development.Cake3.Types where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Data
import Data.Typeable
import qualified Data.List as L
import Data.List hiding(foldr)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import System.FilePath.Wrapper


-- | Makefile variable
data Variable = Variable {
    vname :: String
  , vval :: Maybe String
  -- ^ Nothing means that variable is defined elsewhere (eg. borrowed from the
  -- environment)
  } deriving(Show, Eq, Ord, Data, Typeable)

type Vars = Map String (Set Variable)

-- | Command represents OS command line and consists of a list of fragments.
-- Each fragment is either text (may contain spaces) or FilePath (spaces should
-- be escaped)
type Command = [Either String File]

return_text x = return [Left x]
return_file x = return [Right x]

-- | Recipe answers to the question 'How to build the targets'
data Recipe = Recipe {
    rtgt :: Set File
  -- ^ Targets 
  , rsrc :: Set File
  -- ^ Prerequisites
  , rcmd :: [Command]
  -- ^ A list of shell commands
  , rvars :: Set Variable
  -- ^ Container of variables
  , rloc :: String
  -- FIXME: actually, PHONY is a file's attribute, not recipe's
  , rphony :: Bool
  } deriving(Show, Eq, Ord, Data, Typeable)

addPrerequisites :: Set File -> Recipe -> Recipe
addPrerequisites p r = r { rsrc = p`mappend`(rsrc r)}

addPrerequisite :: File -> Recipe -> Recipe
addPrerequisite f = addPrerequisites (S.singleton f)

type Target = Set File

groupSet :: (Ord k, Ord x) => (x -> [k]) -> Set x -> Map k (Set x)
groupSet keys s = S.foldl' f' mempty s where
  f' m x = L.foldl' ins m (keys x) where
    ins m k = M.insertWith mappend k (S.singleton x) m

groupRecipes = groupSet (S.toList . rtgt)

flattern :: [Set x] -> [x]
flattern = concat . map S.toList

applyPlacement :: [File] -> Set Recipe  -> [Recipe]
applyPlacement pl rs = 
  let placed = nub $ flattern $ catMaybes $ L.map (\k -> M.lookup k (groupRecipes rs)) pl
      all = S.toList rs
  in placed ++ (all \\ placed)
    

transformRecipes :: (Applicative m) => (Recipe -> m (Set Recipe)) -> Set Recipe -> m (Set Recipe)
transformRecipes f m = S.foldl' f' (pure mempty) m where
  f' a r = mappend <$> (f r) <*> a

