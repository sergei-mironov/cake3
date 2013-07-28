module Main where

import System.Process
import Paths_cake3

main = getDataFileName "CakeScript.sh" >>= system . ("sh " ++ )

