module Development.Cake3.Types where

import Data.Maybe
import Data.Monoid
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
  } deriving(Show, Eq, Ord)

type Vars = Map String (Set Variable)

-- | Command represents OS command line and consists of a list of fragments.
-- Each fragment is either text (may contain spaces) or FilePath (spaces should
-- be escaped)
type Command = [Either String File]

return_text x = return [Left x]
return_file x = return [Right x]

-- | Recipe answers to the question 'How to build the targets'
data RecipeT v = Recipe {
    rtgt :: Set File
  -- ^ Targets 
  , rsrc :: Set File
  -- ^ Prerequisites
  , rcmd :: [Command]
  -- ^ A list of shell commands
  , rvars :: v
  -- ^ Container of variables
  , rloc :: String
  -- FIXME: actually, PHONY is a file's attribute, not recipe's
  , rphony :: Bool
  } deriving(Show, Eq, Ord)

type Recipe = Recipe1

type Recipe1 = RecipeT (Map String (Set Variable))

type Recipe2 = RecipeT (Map String Variable)

type Target = Set File

applyPlacement :: (Eq x) => Map Target (RecipeT x) -> [Target] -> [RecipeT x]
applyPlacement rs p = nub $ (mapMaybe id $ map (flip M.lookup rs) p) ++ (map snd $ M.toList rs)

mapRecipes :: [RecipeT x] -> Map Target (RecipeT x)
mapRecipes rs = M.fromList $ map (\r -> (rtgt r,r)) rs

traverseMap :: (Monad m) => ((RecipeT x) -> m [RecipeT x]) -> Map Target (RecipeT x) -> m (Map Target (RecipeT x))
traverseMap f m = M.foldl' f' (return mempty) m where
  f' a r = do
    l <- f r
    m <- a
    return (m`mappend`(mapRecipes l))

