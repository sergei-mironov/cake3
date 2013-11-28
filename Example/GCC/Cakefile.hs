{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Cakefile where

import Development.Cake3
import Cakefile_P

main = writeMake "Makefile" $ do

  cs <- filterDirectoryContentsRecursive [".c"]

  d <- rule $ do
    shell [cmd|gcc -M $cs -MF @(file "depend.mk")|]

  os <- forM cs $ \c -> do
    rule $ do
      shell [cmd| gcc -c $(extvar "CFLAGS") -o @(c.="o") $c |]

  elf <- rule $ do
    shell [cmd| gcc -o @(file "main.elf") $os |]

  rule $ do
    phony "clean"
    unsafeShell [cmd|rm $elf $os $d|]

  rule $ do
    phony "all"
    depend elf

  includeMakefile d
