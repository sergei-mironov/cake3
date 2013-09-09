{-# LANGUAGE QuasiQuotes #-}

module CakeLib where

import Development.Cake3
import CakeLib_P (file)

librules :: Variable -> Rules
librules cf = clean : (ofiles cf) where
  clean = phony "clean" $ unsafe $ do
    shell [cmd| rm $(ofiles cf) |]

ofiles :: Variable -> Rules
ofiles cf = do
  c <- map file [ "lib.c" ]
  rule [c .= "o"] $ do
    shell [cmd| gcc -c -I lib $cf -o $dst $c |]

defcf = makevar "CFLAGS" ""

main = do
  runMake (librules defcf) >>= putStrLn . toMake
