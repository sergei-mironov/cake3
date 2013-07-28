{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Cakefile where

import Development.Cake3
import Cakefile_P (file,cakefiles)
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
-- entities of Cakefiles. It makes automatic dependency tracking possible. rules
-- function has a signature of
--
-- newtype Alias = Alias (File, Make Rule)
--
-- rule :: [File] -> A () -> [Alias]
--
-- Basically, rule returns list of Files, each File may be created by
-- calculating a Rule in a Make monad.

-- Rule for compiling files. Check resulting Makefile to see how variable guards
-- are inserted. ofiles will be rebult is someone changes the CFLAGS. Note, that
-- this particluar rule is written as List monad.
--
-- $dst reference is a bit special. It is a function defined in
-- Development.Cake3 which expands into space-separated list of current targets
ofiles :: [Alias]
ofiles = do 
  c <- cfiles
  rule [c .= "o"] $ do
    [make| gcc -I lib -c $cflags -o $dst $c |]

-- Bring together objects from this unit and from the Lib library
allofiles :: [Alias]
allofiles = ofiles ++ (L.ofiles cflags)

-- Rule for linker, not how various entities are referenced.
elf :: [Alias]
elf = rule [file "main.elf"] $ do
  [make| echo "SHELL is $shell" |]
  [make| gcc -o $dst $allofiles |]
  [make| echo $sound |]

-- Clean is a special rule in a sence that it doesn't use dependencies or
-- variable guards. clean should just do what user tolds it to do. Thus, unsafe
-- is used here.
clean :: [Alias]
clean = phony "clean" $ unsafe $ do
    [make| rm $elf ; rm GUARD_* ; rm $allofiles ; rm $cakegen |]

-- Rule named 'all' is just an alias for elf
all = phony "all" $ do
  depend elf

-- Self-update rules: rebuild Makefile if Cakefiles changes
cakegen = rule [file "Cakegen" ] $ do
  depend cakefiles
  [make| cake3 |]

selfupdate = rule [file "Makefile"] $ do
  [make| $cakegen > $dst |]

-- Finally, default Haskell main function collects all required rules and prints the
-- Makefile's contents on a standard output
main = do
  runMake (Cakefile.all ++ elf ++ clean ++ selfupdate ) >>= putStrLn . toMake

