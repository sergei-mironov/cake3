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
-- data Pos a = Pos { ppos :: Int, pwhat :: a }
--   deriving(Show, Eq)

-- instance Ord a => Ord (Pos a) where
--   compare (Pos p w) (Pos p2 w2) =
--     case p`compare`p2 of
--       EQ -> w`compare`w2
--       x -> x

-- instance Monoid a => Monoid (Pos a) where
--   mempty = Pos 0 mempty
--   mappend (Pos a ad) (Pos b bd) = Pos (min a b) (mappend ad bd)

-- -- higher positions go first
-- cmpPos (Pos a _) (Pos b _) = a`compare`b
-- unposition (Pos _ x) = x


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
