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

data Positioned a = Positioned { ppos :: Int, pwhat :: a }
  deriving(Show,Eq,Ord)

instance Monoid a => Monoid (Positioned a) where
  mempty = Positioned 0 mempty
  mappend (Positioned a ad) (Positioned b bd) = Positioned (min a b) (mappend ad bd)

cmpPos (Positioned a _) (Positioned b _) = a`compare`b
unposition (Positioned _ x) = x

type Command = String

data Variable = Variable { vname :: String, vval :: Maybe String }
  deriving(Show, Eq, Ord)

type Vars = Map String (Set Variable)

newtype Escaped x = Escaped x
  deriving(Show, Eq, Ord)

escape :: FilePath -> Escaped FilePath
escape = Escaped . escfile' where
  escfile' [] = []
  escfile' (' ':cs) = "\\\\ " ++ escfile' cs
  escfile' (x:cs) = x:escfile' cs

newtype File = File (Escaped FilePath)
  deriving(Show,Eq,Ord)

unfile (File (Escaped x)) = x

data Rule = 
  Rule {
    rtgt' :: [File]
  , rsrc' :: [File]
  , rcmd :: [Command]
  , rvars :: Map String (Set Variable)
  , rloc :: String
  -- FIXME: actually, PHONY is a file's attribute, not rule's
  , rphony :: Bool
  } deriving(Show, Eq, Ord)

rtgt = map unfile . rtgt'
rsrc = map unfile . rsrc'

type Rules = Map [File] (Positioned (Set Rule))

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
    srules :: Rules
  , svars :: Vars
  , sloc :: Location
  , spos :: Int
  } deriving(Show)

defMS = MS mempty mempty mempty 0

