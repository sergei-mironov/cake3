{-# LANGUAGE QuasiQuotes #-}
module Cakefile where

import Development.Cake3
import Development.Cake3.Utils.Find
import Cakefile_P (file,projectroot)


main = runMake_ $ do

  cfiles <- filterExts [".c"] <$> getDirectoryContentsRecursive projectroot

  let obj c = c .= ".o"
  let elf = file "main.elf"
  let deps = file "depend.mk"

  rule $ do
    phony "all"
    depend elf

  rule $ do
    shell [cmd|gcc -M @cfiles -MF %deps|]

  forM cfiles $ \c -> do
    rule $ do
      shell [cmd| gcc -c $(extvar "CFLAGS") -o %(obj c) @c |]

  rule $ do
    shell [cmd| gcc -o %elf @(map obj cfiles) |]

  include deps

