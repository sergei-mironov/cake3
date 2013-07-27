{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Cakefile where

import Control.Monad.Loc
import Development.Cake3
import Cakepath_Main (file)
import qualified CakeLib as L

-- Haskell-level variable
sound = "Yuupee" :: String

-- Makefile variable
cflags = makevar "CFLAGS" "-O0 -g3"

-- Makefile variable, defined elsewhere
shell = extvar "SHELL"

-- list of files. file function should be defined for each Cakefile to refer to
-- files relative to current directory. See Cakepath_Main.hs for definition
cfiles = map file [ "main.c"]

-- Rules uses [make| ... |] syntax for quasy-quoting the references to other
-- entities of Cakefiles. It makes automatic dependency tracking possible.

-- Rule for compiling files. Check resulting Makefile to see how variable guards
-- are inserted. ofiles will be rebult is someone changes the CFLAGS
ofiles = forM cfiles $ \c -> do
  rule [c .= "o"] $ do
    [make| gcc -I lib -c $cflags -o $dst $c |]

-- Bring together objects from this unit and from Library
allofiles = ofiles ++ (L.ofiles cflags)

-- Rule for linker
elf = rule [file "main.elf"] $ do
  [make| echo "SHELL is $shell" |]
  [make| gcc -o $dst $allofiles |]
  [make| echo $sound |]

-- Clean is a special rule in a sence that it doesn't use dependencies or
-- variable guards. clean should just do what user tolds it to do. Thus, unsafe
-- is used here.
clean = phony "clean" $ unsafe $ do
  [make| rm $elf ; rm GUARD_* ; rm $allofiles |]

-- All rule is just an alias for elf
all = phony "all" $ do
  depend (head elf)

-- Finally, Haskell main function collects all required rules and prints the
-- Makefile's contents on a standard output
main = do
  runMake (Cakefile.all ++ elf ++ clean) >>= putStrLn . toMake

