{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module CakeLib where

import Control.Monad.Loc
import Development.Cake3

import CakeLib_P (file)

cfiles = map file [ "lib.c" ]

librules cf = (ofiles cf) ++ clean where
  clean = phony "clean" $ unsafe $ do
    [make| rm $(ofiles cf) |]

ofiles cf = do
  c <- cfiles
  rule [c .= "o"] $ do
    [make| gcc -c -I lib $cf -o $dst $c |]

defcf = makevar "CFLAGS" ""

main = do
  runMake (librules defcf) >>= putStrLn . toMake
