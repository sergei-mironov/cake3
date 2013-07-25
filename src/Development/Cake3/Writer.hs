module Development.Cake3.Writer where

import Control.Applicative
import Control.Monad.Writer
import Data.List
import Development.Cake3.Types
import Text.Printf

toMake tree = unlines $ body where

  body = guard ++ (map varToken vs) ++ (concat $ map ruleToken rs) ++ (concat $ map guardRuleOf vs)

  (Uniq vs, Uniq rs, es) = collapse tree

  varToken (Variable n v) = printf "%s = %s" n v

  guard = ["GUARD = $(1)_GUARD_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)"]

  guardRuleOf v = [
      printf "$(call GUARD,%s) :" (vname v)
    , printf "\trm -f %s_GUARD_*" (vname v)
    , printf "\ttouch $@"
    , ""
    ]

  rule2vars r = let Uniq vs = fst $ collapse' (rvars r) in vs

  -- FIXME: implement http://stackoverflow.com/questions/11647859/make-targets-depend-on-variables
  -- FIXME: extra newline in commands
  ruleToken r = execWriter $ do
    when (rphony r) $ do
      tell [printf ".PHONY: %s" (rtgt r)]
    tell $ hdr : cmds
    where
      hdr = printf "%s : %s # %s" (rtgt r) deps (rloc r)
      cmds = map ("\t" ++) (rcmd r)
      varguard v = printf "$(call GUARD,%s)" (vname v)
      deps = (intercalate " " $ (rsrc r) ++ (map varguard (rule2vars r)))


