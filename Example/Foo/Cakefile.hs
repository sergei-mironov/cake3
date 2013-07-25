{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad.Loc
import Development.Cake3

import Cakepath_Main (file)
import qualified CakeLib as L

cflags = var "CFLAGS" "-O0 -g3"

cfiles = map file [ "main.c"]

ofiles = do
  forM cfiles $ \c -> do
    rule [c .= "o"] $ do
      [make| gcc -I lib -c $cflags -o $dst $c |]

elf = do
  rule [file "main.elf"] $ do
    [make| gcc -o $dst $ofiles $(L.ofiles cflags) |]

main = do
  runMake elf >>= putStrLn . toMake

