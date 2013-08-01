
module Cakefile_P where

import Development.Cake3

file :: FilePath -> File
file x = file' ("." </> x)

