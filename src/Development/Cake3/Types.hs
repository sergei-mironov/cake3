module Development.Cake3.Types where

import Data.Monoid
import qualified Data.List as L
import Data.List hiding(foldr)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import System.FilePath.Wrapper

-- | Item wich have it's position in the Makefile. Positioned adds the metric to
-- the contained datatype. Note, that the metric is not the subject of Eq or
-- Ord. mappend-ing two metrics results in taking the minimal one.
data Positioned a = Positioned { ppos :: Int, pwhat :: a }
  deriving(Show)

instance Eq a => Eq (Positioned a) where
  (==) a b = (pwhat a) == (pwhat b)
instance Ord a => Ord (Positioned a) where
  compare a b = compare (pwhat a) (pwhat b)
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

-- | Command represents OS command line and consists of a list of fragments.
-- Each fragment is either text (may contain spaces) or FilePath (spaces should
-- be escaped)
type Command f = [Either String f]

return_text x = return [Left x]
return_file x = return [Right x]

-- | Recipe answers to the question 'How to build the targets'
data RecipeT v f = Recipe {
    pos :: Int
  , rtgt :: [f] -- FIXME: convert into Set
  -- ^ Targets 
  , rsrc :: [f] -- FIXME: convert into Set
  -- ^ Prerequisites
  , rcmd :: [Command f]
  -- ^ A list of shell commands
  , rvars :: v
  -- ^ Container of variables
  , rloc :: String
  -- FIXME: actually, PHONY is a file's attribute, not recipe's
  , rphony :: Bool
  } deriving(Show, Eq, Ord)

