{-# LANGUAGE OverloadedStrings #-}
module Development.Cake3.Types where

import Prelude hiding (FilePath)
import Data.Monoid
import qualified Data.List as L
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
-- import System.FilePath
import Filesystem.Path.CurrentOS
import Text.Printf
-- import Data.Text.Lazy as T (Text(..))

-- | Item wich have it's position in the Makefile. Positions are just a sequence
-- to order the items
data Positioned a = Positioned { ppos :: Int, pwhat :: a }
  deriving(Show,Eq,Ord)

instance Monoid a => Monoid (Positioned a) where
  mempty = Positioned 0 mempty
  mappend (Positioned a ad) (Positioned b bd) = Positioned (min a b) (mappend ad bd)

-- higher positions go first
cmpPos (Positioned a _) (Positioned b _) = b`compare`a
unposition (Positioned _ x) = x

-- | Makefile variable
data Variable = Variable {
    vname :: String
  , vval :: Maybe String
  -- ^ Nothing means that variable is defined elsewhere (eg. borrowed from the
  -- environment)
  } deriving(Show, Eq, Ord)

type Vars = Map String (Set Variable)

-- | For strings: tag marking escaped string, i.e. the one with ' ' replaced
-- with '\ '.
-- newtype Escaped = Escaped Text deriving(Show, Eq, Ord)

-- FIXME: What about '\' in string?
-- escape :: FilePath -> Escaped FilePath
-- escape = Escaped . escfile' where
--   escfile' [] = []
--   escfile' (' ':cs) = "\\\\ " ++ escfile' cs
--   escfile' (x:cs) = x:escfile' cs

-- | Command represents OS command line and consists of a list of fragments.
-- Each fragment is either text (may contain spaces) or FilePath (spaces should
-- be escaped)
type Command = [Either String FilePath]
return_text x = return [Left x]
return_file x = return [Right x]

-- | Recipe answers to the question 'How to build the targets'
data Recipe = Recipe {
    rtgt :: [FilePath]
  -- ^ Targets
  , rsrc :: [FilePath]
  -- ^ Prerequisites
  , rcmd :: [Command]
  -- ^ A list of shell commands
  , rvars :: Map String (Set Variable)
  , rloc :: String
  -- FIXME: actually, PHONY is a file's attribute, not recipe's
  , rphony :: Bool
  } deriving(Show, Eq, Ord)

-- | Collection of recipes.
type Recipes = Map [FilePath] (Positioned (Set Recipe))

-- | A list with uniq elements, i.e. set.
newtype Uniq s = Uniq [s] deriving(Show)

collapse :: (Ord y, Show k) => Map k (Set y) -> (Uniq y, [String])
collapse m = asUniq $ foldr check1 mempty $ M.toList m where
  check1 (k,s) (ss,es) | S.size s == 1 = (s`S.union`ss, es)
                       | otherwise = (ss, (printf "several values for key %s" (show k)):es)
  asUniq (s,e) = (Uniq (S.toList s), e)

-- FIXME: simplify the code. One needn't two almost similar collapses
collapseP :: (Ord y, Show k) => Map k (Positioned (Set y)) -> (Uniq y, [String])
collapseP m = asUniq $ foldr check1 mempty $ M.toList m where
  check1 (k,p@(Positioned pos s)) (ss,es) | S.size s == 1 = ((Positioned pos (head $ S.toList s)):ss, es)
                                        | otherwise = (ss, (printf "several values for key %s" (show k)):es)
  asUniq (ps,e) = (Uniq (map unposition $ sortBy cmpPos ps), e)

type Location = String

data MakeState = MS {
    srecipes :: Recipes
  , svars :: Vars
  , sloc :: Location
  , spos :: Int
  } deriving(Show)

defMS = MS mempty mempty mempty 0


class AsFile x where
  toFile :: x -> FilePath

instance AsFile FilePath where
  toFile = id

-- FIXME: make it change the suffix instead of just adding one to another
-- (.=) :: (AsFile x) => x -> String -> File
-- (.=) x newext = let (File (Escaped src)) = toFile x
--                 in (File (Escaped $ replaceExtension src newext))

