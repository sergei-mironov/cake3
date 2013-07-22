{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad.Loc
import Development.Cake3

import Cakepath_Main (top)
import qualified CakeLib as L

cflags = var "CFLAGS" "-O0 -g3"

cfiles = map (top </>) [ "main.c"]

ofiles = do
  forM cfiles $ \c -> do
    rule (c .= "o") $ do
      [make| gcc $cflags -o $dst $c |]

elf = do
  rule (top </> "main.elf") $ do
    [make| ld -o $dst $ofiles $(L.ofiles cflags) |]


main = do
  runMake elf >>= putStrLn . toMake
