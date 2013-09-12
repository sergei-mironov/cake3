{-# LANGUAGE QuasiQuotes #-}

module CakeLib where

import Development.Cake3
import CakeLib_P (file)

librules :: Variable -> [Alias]
librules cf = (ofiles cf) ++ [clean] where
  clean = phony "clean" $ unsafe $ do
    shell [cmd| rm $(ofiles cf) |]

ofiles :: Variable -> [Alias]
ofiles var = 
  let c = file "lib.c" in
  rule [c .= "o"] $ do
    shell [cmd| gcc -c -I lib $var -o $dst $c |]

main = runMake (librules (makevar "CFLAGS" ""))
