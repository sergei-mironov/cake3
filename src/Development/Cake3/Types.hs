{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Development.Cake3.Types where

import Data.Monoid
import qualified Data.List as L
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import System.FilePath
import Text.Printf

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

type Command = String

-- | Makefile variable
data Variable = Variable { vname :: String, vval :: Maybe String }
  deriving(Show, Eq, Ord)

type Vars = Map String (Set Variable)

-- | For strings: tag marking escaped string, i.e. the one with ' ' replaced
-- with '\ '.
newtype Escaped x = Escaped x deriving(Show, Eq, Ord)

-- FIXME: What about '\' in string?
escape :: FilePath -> Escaped FilePath
escape = Escaped . escfile' where
  escfile' [] = []
  escfile' (' ':cs) = "\\\\ " ++ escfile' cs
  escfile' (x:cs) = x:escfile' cs

newtype File = File (Escaped FilePath)
  deriving(Show,Eq,Ord)

unfile (File (Escaped x)) = x

data Recipe = 
  Recipe {
    rtgt' :: [File]
  , rsrc' :: [File]
  , rcmd :: [Command]
  , rvars :: Map String (Set Variable)
  , rloc :: String
  -- FIXME: actually, PHONY is a file's attribute, not recipe's
  , rphony :: Bool
  } deriving(Show, Eq, Ord)

rtgt = map unfile . rtgt'
rsrc = map unfile . rsrc'

-- | Collection of recipes.
type Recipes = Map [File] (Positioned (Set Recipe))

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

-- | Latest location in Cakefile known
type Location = String

data MakeState = MS {
    srecipes :: Recipes
  -- ^ Recipes collection
  , svars :: Vars
  -- ^ Variables collection
  , sloc :: Location
  -- ^ Latest location in Cakefile known
  , spos :: Int
  -- ^ Current position counter
  } deriving(Show)

defMS = MS mempty mempty mempty 0

class (Functor f, Monad f) => MakeTargets f where
  allTargets :: f File -> f [File]

instance MakeTargets [] where
  allTargets = return

instance MakeTargets IO where
  allTargets mx = mx >>= \x -> return [x]

