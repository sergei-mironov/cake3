{-# LANGUAGE FlexibleInstances #-}
module Development.Cake3.Writer (toMake) where

import Prelude hiding (FilePath)
import Control.Monad (when)
import Control.Applicative
import Control.Monad.State (State(..), execState, runState, modify, get, put)
import Data.List as L
import Data.String
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Printf
import System.FilePath.Wrapper

import Development.Cake3.Types
import Development.Cake3.Monad

-- rule2vars r = let Uniq vs = fst $ C3.collapse (rvars r) in vs

gen :: MakeWriter Int
gen = do
  x <- head <$> cnt <$> get
  modify (\ws -> ws { cnt = tail (cnt ws) })
  return x

-- | Writer state
data WS = WS { cnt :: [Int] , ls :: String }
  deriving(Show)

type MakeWriter a = State WS a

class ToMakeText x where
  toMakeText :: x -> String
  
instance ToMakeText [Char] where
  toMakeText = id

instance ToMakeText [[Char]] where
  toMakeText = concat . map toMakeText

instance ToMakeText File where
  toMakeText (File f) = escape f where
    escape [] = []
    escape (' ':xs) = "\\ " ++ escape xs
    escape (x:xs) = (x:(escape xs))

spaceB (' ':xs) = True
spaceB _ = False
spaceE [] = False
spaceE x | last x == ' ' = True | otherwise = False

instance ToMakeText Command where
  -- Make sure that Files have spaces in between
  toMakeText [x] = either toMakeText toMakeText x
  toMakeText ((Left str):(Right f):cmd)
    | spaceE str = str ++ (toMakeText ((Right f):cmd))
    | otherwise = str ++ ' ':(toMakeText ((Right f):cmd))
  toMakeText ((Right f):(Left str):cmd)
    | spaceB str = (toMakeText f) ++ (toMakeText ((Left str):cmd))
    | otherwise = (toMakeText f) ++ ' ':(toMakeText ((Left str):cmd))
  toMakeText ((Right f):(Right f2):cmd) = (toMakeText f) ++ ' ':(toMakeText ((Right f2):cmd))
  toMakeText ((Left s1):(Left s2):cmd) = s1 ++ (toMakeText ((Left s2):cmd))

line :: String -> MakeWriter ()
line s = modify $ \ws -> ws { ls = concat [ls ws, s, "\n"] }

toMake :: Recipes -> String
toMake rs_ =
  let (errs, vs, rs) = check rs_
  in ls $ flip execState (WS [1..] "") $ do

  line "# This Makefile was generated by the ThirdCake"
  line "# https://github.com/grwlf/cake3"
  line ""

  line "GUARD = .GUARD_$(1)_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)"

  -- Variables
  forM_ vs $ \v -> case v of
    (Variable n (Just v)) -> line (printf "%s = %s" n v)
    (Variable n Nothing) -> return ()

  -- Rules
  forM_ rs $ \r -> do
    let varguard v = printf "$(call GUARD,%s)" (vname v)
    let deps = intercalate " " $ (map toMakeText (rsrc r)) ++ (map varguard (rvars r))
    let tgts = intercalate " " $ (map toMakeText (rtgt r))

    when (rphony r) $ do
      line (printf ".PHONY: %s" tgts)

    case (length (rtgt r)) of
      0 -> do
        return ()
      1 -> do
        let s = (head (rtgt r))
        line $ printf "%s : %s" (toMakeText s) deps
        forM_ (rcmd r) $ \c -> do
          line (printf "\t%s" (toMakeText c))
      _ -> do
        i <- gen
        let s = (printf "stamp%d" i :: String)
        line (printf ".INTERMEDIATE : %s" s)
        line (printf "%s : %s" s deps)
        forM_ (rcmd r) $ \c -> do
          line (printf "\t%s" (toMakeText c))
        line (printf "%s : %s" tgts s)

  -- Rules for variable's guards
  forM_ vs $ \v -> do
    line (printf "$(call GUARD,%s) :" (vname v))
    line (printf "\trm -f .GUARD_%s_*" (vname v))
    line "\ttouch $@"

