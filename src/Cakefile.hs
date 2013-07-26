{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad.Loc
import Development.Cake3

import Cakepath_Main (file)

foodef = "-DFOO" :: String

cc = makevar "CROSS_COMPILE" "arm-none-eabi-"

shell = extvar "SHELL"

cflags = var "CFLAGS" "-O0 -g3"

cfiles = map file [ "main.c", "second.c",  "space file.c"]

ofiles = do
  forM cfiles $ \c -> do
    rule [c .= "o"] $ do
      [make| $(cc)gcc $foodef $cflags -o $dst $c |]

[elf,elf2] = rule [file "main.elf", file "main2.elf" ] $ do
    [make| echo "Shell value is $shell" |]
    [make| $(cc)ld -o $dst $ofiles |]

all = do
  phony "all" $ do
    depend elf

