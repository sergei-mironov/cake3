
module Cakepath_Lib where

import Development.Cake3

file :: FilePath -> File
file x = file' ("lib/" ++ x)
