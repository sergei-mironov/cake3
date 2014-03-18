module Cakefile where

import Development.Cake3
import Cakefile_P

main = writeMake (file "Makefile") $ do

  prebuild [cmd|@@echo "*****************"|]
  prebuild [cmd|@@echo "THE CAKE3 EXAMPLE"|]
  prebuild [cmd|@@echo "*****************"|]

  cs <- filterDirectoryContentsRecursive [".c"]

  d <- rule $ do
    shell [cmd|gcc -M $cs -MF @(file "depend.mk")|]

  os <- forM cs $ \c -> do
    rule $ do
      shell [cmd| gcc -c $(extvar "CFLAGS") -o @(c.="o") $c |]

  elf <- rule $ do
    shell [cmd| gcc -o @(file "main.elf") $os |]

  rule $ do
    phony "all"
    depend elf

  includeMakefile d

  selfUpdate
