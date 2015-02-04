{-# LANGUAGE DeriveDataTypeable #-}
module Development.Cake3.Types where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Writer (MonadWriter, WriterT(..), execWriterT, execWriter, tell)
import Data.Maybe
import Data.Monoid
import Data.Data
import Data.Typeable
import Data.Foldable (Foldable(..), foldl', forM_)
import qualified Data.List as L
import Data.List hiding(foldr, foldl')
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import System.FilePath.Wrapper

-- | The representation of Makefile variable.
data Variable = Variable {
    vname :: String
  -- ^ The name of a variable
  , vval :: Maybe String
  -- ^ Nothing means that variable is defined elsewhere (eg. borrowed from the
  -- environment)
  } deriving(Show, Eq, Ord, Data, Typeable)


-- | The representation a tool used by the Makefile's recipe. Typical example
-- are 'gcc' or 'bison'
data Tool = Tool {
    tname :: String
  -- ^ Name of tool.
  } deriving(Show, Eq, Ord, Data, Typeable)

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
  -- ^ A set of variables employed in the recipe. The target Makefile should
  -- notice changes in those variables and rebuild the targets
  , rtools :: Set Tool
  -- ^ A set of tools employed in the recipe. Make
  , rloc :: String
  -- ^ Location (probably, doesn't function)
  , rflags :: Set Flag
  -- ^ Set of flags (Makefile-specific)
  } deriving(Show, Eq, Ord, Data, Typeable)

emptyRecipe :: String -> Recipe
emptyRecipe loc = Recipe mempty mempty mempty mempty mempty loc mempty

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

filterRecipesByTools :: (Foldable t) => [Tool] -> t Recipe -> Set Recipe
filterRecipesByTools ts rs = foldMap mp rs where
  mp r = (\match -> if match then S.singleton r else S.empty) $ or $ map (\t -> S.member t (rtools r)) ts

filterRecipesByTargets :: (Foldable t, Foldable t2) => t2 File -> t Recipe -> Set Recipe
filterRecipesByTargets ts rs = foldMap mp rs where
  mp r = (\(Any match) -> if match then S.singleton r else S.empty) $ foldMap (\t -> Any $ S.member t (rtgt r)) ts

filterRecipesByToolsDeep :: [Tool] -> Set Recipe -> Set Recipe
filterRecipesByToolsDeep ts rs = fdeep (queryPrereq ry) rn ry where
  ry = filterRecipesByTools ts rs
  rn = rs `S.difference` ry

  fdeep ts rn ry =
    let
      ry' = filterRecipesByTargets ts rn
    in
      if not $ S.null ry' then
        fdeep (queryPrereq ry') (rn `S.difference` ry') (ry `S.union` ry')
      else
        ry

applyPlacement :: (Foldable t) => [File] -> t Recipe  -> [Recipe]
applyPlacement pl rs = flattern $ applyPlacement' pl (groupRecipes rs)

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

queryPrereq :: (Foldable t) => t Recipe -> Set File
queryPrereq rs = foldl' (\a r -> a`mappend`(rsrc r)) mempty rs

var :: String -> Maybe String -> Variable
var n v = Variable n v

intermediateFiles :: (Foldable t) => t Recipe -> Set File
intermediateFiles rs = 
  execWriter $ do
    forM_ rs $ \r -> do
      when (not $ Phony `S.member` (rflags r)) $ do
        tell (rtgt r)

tool :: String -> Tool
tool = Tool

-- | Define the Makefile-level variable. Rules, referring to a variable,
-- 'notice' it's changes.
makevar
  :: String -- ^ Variable name
  -> String -- ^ Default value
  -> Variable
makevar n v = var n (Just v)

-- | Declare the variable defined elsewhere. Typycally, environment variables
-- may be decalred with this functions. Variables are tracked by the cake3.
-- Rules, referring to a variable, 'notice' it's changes.
extvar :: String -> Variable
extvar n = var n Nothing

-- | Reref to special variable @$(MAKE)@
make = extvar "MAKE"

-- | Simple wrapper for FilePath.
type File = FileT FilePath

