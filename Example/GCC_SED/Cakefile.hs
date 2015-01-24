module Cakefile where

import Development.Cake3
import Development.Cake3.Utils.Slice
import Cakefile_P

-- | Let's imagine that sed is our build-only tool.
sed = tool "sed"


-- | Build the Makefile and Makefile.nosed. The latter assumes that all targets
-- depending on 'sed' rule are pre-built. In this case this should be a 'main.h'
-- target. 
main = writeSliced (file "Makefile.devel") [(file "Makefile", [cake3,cakegen,sed])] $ do

  prebuild [cmd|@@echo "*****************"|]
  prebuild [cmd|@@echo "THE CAKE3 EXAMPLE"|]
  prebuild [cmd|@@echo "*****************"|]

  cs <- filterDirectoryContentsRecursive [".c"]

  h <- rule $ do
    shell [cmd|$(sed) 's/HELLO/\"HELLO CAKE3!\"/' $(file "main.h.in") > @(file "main.h")|]

  d <- rule $ do
    depend h
    shell [cmd|gcc -M $cs -MF @(file "depend.mk")|]

  os <- forM cs $ \c -> do
    rule $ do
      depend h
      shell [cmd| gcc -c $(extvar "CFLAGS") -o @(c.="o") $c |]

  elf <- rule $ do
    shell [cmd| gcc -o @(file "main.elf") $os |]

  rule $ do
    phony "all"
    depend elf

  includeMakefile d

  selfUpdate
