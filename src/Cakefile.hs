{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad.Loc
import Development.Cake3

import Cakepath_Main (top)

cc = var "CROSS_COMPILE" "arm-none-eabi-"

cflags = var "CFLAGS" "-O0 -g3"

cfiles = map (top </>) [ "main.c", "second.c",  "space file.c"]

ofiles = do
  forM cfiles $ \c -> do
    rule (c .= "o") $ do
      [make| $(cc)gcc $cflags -o $dst $c |]

elf = do
  rule (top </> "main.elf") $ do
    [make| $(cc)ld -o $dst $ofiles |]


all = do
  phony "all" $ do
    depend elf
