module Development.Cake3.Writer where

import Control.Applicative
import Control.Monad.Writer
import Data.List
import Development.Cake3.Types
import Text.Printf

rule2vars r = let Uniq vs = fst $ collapse' (rvars r) in vs

tell1 x = tell [x]

toMake tree = let (Uniq vs, Uniq rs, es) = collapse tree in unlines $ execWriter $ do

  tell1 "GUARD = $(1)_GUARD_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)"

  forM vs $ \(Variable n v) -> do
    tell1 $ printf "%s = %s" n v

  forM rs $ \r -> do
    let varguard v = printf "$(call GUARD,%s)" (vname v)
    let deps = (intercalate " " $ (rsrc r) ++ (map varguard (rule2vars r)))
    when (rphony r) $ do
      tell1 $ printf ".PHONY: %s" (rtgt r)
    tell1 $ printf "%s : %s # %s" (rtgt r) deps (rloc r)
    forM (rcmd r) $ \c -> tell1 ("\t" ++ c)

  forM vs $ \v -> do
    tell $ [
        printf "$(call GUARD,%s) :" (vname v)
      , printf "\trm -f %s_GUARD_*" (vname v)
      , printf "\ttouch $@"
      -- FIXME: why do we need this empty line?
      , ""
      ]
  
  
  
  
