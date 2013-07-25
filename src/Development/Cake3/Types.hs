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

type Command = String

data Variable = Variable { vname :: String, vval :: String }
  deriving(Show, Eq, Ord)

newtype Escaped x = Escaped x
  deriving(Show, Eq, Ord)

escaped :: Escaped x -> x
escaped (Escaped x) = x

escape :: FilePath -> Escaped FilePath
escape = Escaped . escfile' where
  escfile' [] = []
  escfile' (' ':cs) = "\\\\ " ++ escfile' cs
  escfile' (x:cs) = x:escfile' cs


data Rule = 
  Rule {
    rtgt' :: Escaped FilePath
  , rsrc' :: [Escaped FilePath]
  , rcmd :: [Command]
  , rvars :: Map String (Set Variable)
  , rloc :: String
  -- FIXME: actually, PHONY is a file's attribute, not rule's
  , rphony :: Bool
  } deriving(Show, Eq, Ord)

rtgt = escaped . rtgt'
rsrc = map escaped . rsrc'

type Rules = Map FilePath (Set Rule)

newtype Uniq s = Uniq [s] deriving(Show)

collapse' :: (Ord y) => Map String (Set y) -> (Uniq y, [String])
collapse' m = asUniq $ foldr check1 mempty $ M.toList m where
  check1 (k,s) (ss,es) | S.size s == 1 = (s`S.union`ss, es)
                       | otherwise = (ss, (printf "several values for key %s" k):es)
  asUniq (s,e) = (Uniq (S.toList s), e)

collapseRules :: Rules -> (Uniq Rule, [String])
collapseRules rs = collapse' rs

collapseVars :: Uniq Rule -> (Uniq Variable, [String])
collapseVars (Uniq rs) = collapse' $ (M.unionsWith mappend) $ map rvars rs

collapse :: Rules -> (Uniq Variable, Uniq Rule, [String])
collapse tree = (vs, rs, e1++e2) where
  (rs, e1) = collapseRules tree
  (vs, e2) = collapseVars rs


