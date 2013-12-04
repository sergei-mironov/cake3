{-# LANGUAGE DeriveDataTypeable #-}
module Development.Cake3.Types where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Data
import Data.Typeable
import Data.Foldable (Foldable(..), foldl')
import qualified Data.List as L
import Data.List hiding(foldr, foldl')
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import System.FilePath.Wrapper

-- | The representation of Makefile variable
data Variable = Variable {
    vname :: String
  -- ^ The name of a variable
  , vval :: Maybe String
  -- ^ Nothing means that variable is defined elsewhere (eg. borrowed from the
  -- environment)
  } deriving(Show, Eq, Ord, Data, Typeable)

-- type Vars = Map String (Set Variable)

-- | Command represents OS command line and consists of a list of fragments.
-- Each fragment is either text (may contain spaces) or FilePath (spaces should
-- be escaped)
type Command = [CommandPiece]

data CommandPiece = CmdStr String | CmdFile File
  deriving (Show, Eq, Ord, Data, Typeable)

return_text x = return [CmdStr x]
return_file f = return [CmdFile f]

data Flag = Phony | Intermediate
  deriving(Show,Eq,Ord, Data, Typeable)

-- | Recipe answers to the question 'How to build the targets'. Internally, it
-- contains sets of targets and prerequisites, as well as shell commands
-- required to build former from latter
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
  , rflags :: Set Flag
  } deriving(Show, Eq, Ord, Data, Typeable)

emptyRecipe :: String -> Recipe
emptyRecipe loc = Recipe mempty mempty mempty mempty loc mempty

addPrerequisites :: Set File -> Recipe -> Recipe
addPrerequisites p r = r { rsrc = p`mappend`(rsrc r)}

addPrerequisite :: File -> Recipe -> Recipe
addPrerequisite f = addPrerequisites (S.singleton f)

type Target = Set File

groupSet :: (Ord k, Ord x, Foldable t) => (x -> Set k) -> t x -> Map k (Set x)
groupSet keys s = foldl' f' mempty s where
  f' m x = foldl' ins m (keys x) where
    ins m k = M.insertWith mappend k (S.singleton x) m

groupRecipes ::  (Foldable t) => t Recipe -> Map File (Set Recipe)
groupRecipes = groupSet rtgt

flattern :: [Set x] -> [x]
flattern = concat . map S.toList

applyPlacement' :: (Eq x) => [File] -> Map File x  -> [x]
applyPlacement' pl m = 
  let placed = nub $ catMaybes $ L.map (\k -> M.lookup k m) pl
      all = L.map snd $ M.toList m
  in placed ++ (all \\ placed)

applyPlacement :: (Foldable t) => [File] -> t Recipe  -> [Recipe]
applyPlacement pl rs = flattern $ applyPlacement' pl (groupRecipes rs)

-- applyPlacement2 :: (Foldable t) => [File] -> t File  -> [File]
-- applyPlacement2 pl fs = flattern $ applyPlacement' pl (groupSet S.singleton fs)
    

transformRecipes :: (Applicative m) => (Recipe -> m (Set Recipe)) -> Set Recipe -> m (Set Recipe)
transformRecipes f m = S.foldl' f' (pure mempty) m where
  f' a r = mappend <$> (f r) <*> a

transformRecipesM_ :: (Monad m, Foldable t) => (Recipe -> m ()) -> t Recipe -> m ()
transformRecipesM_ f rs = foldl' (\a r -> a >> f r) (return mempty) rs

queryVariables :: (Foldable t) => t Recipe -> Set Variable
queryVariables rs = foldl' (\a r -> a`mappend`(rvars r)) mempty rs

queryVariablesE :: (Foldable t) => t Recipe -> Either String (Set Variable)
queryVariablesE rs = check where
  vs = queryVariables rs
  bads = M.filter (\s -> (S.size s) /= 1) (groupSet (\v -> S.singleton (vname v)) vs)
  check | (M.size bads) > 0 = Left "Some variables share same name"
        | otherwise = Right vs

queryTargets :: (Foldable t) => t Recipe -> Set File
queryTargets rs = foldl' (\a r -> a`mappend`(rtgt r)) mempty rs

var :: String -> Maybe String -> Variable
var n v = Variable n v

-- | Declare the variable which is defined in the current Makefile and has it's
-- default value
makevar
  :: String -- ^ Variable name
  -> String -- ^ Default value
  -> Variable
makevar n v = var n (Just v)

-- | Declare the variable which is not defined in the target Makefile
extvar :: String -> Variable
extvar n = var n Nothing

-- | Special variable @$(MAKE)@
make = extvar "MAKE"

