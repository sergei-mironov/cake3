module Cakefile where

import Development.Cake3
import Cakefile_P

main = do

  writeMake (file "Makefile") $ do

    rule $ do
      phony "all"
      depend (genFile (file "out") (unlines [
        "line 1 1 1", "line 2 2 2", "line $ $ $",
        "line \" \" \"", "line ' ' '", "line \\ \\ \\"]))
