{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import System.FilePath
import Control.Monad
import Development.Cake3
import Control.Monad.Loc

cc = var "CROSS_COMPILE" "arm-none-eabi-"

cflags = var "CFLAGS" "-O0 -g3"

cfiles = [ top "main.c", top "second.c" ]

ofiles = do
  forM cfiles $ \c -> do
    rule (c .= "o") $ do
      [make| $(cc)gcc $cflags -o $dst $c |]

elf = do
  rule (top "main.elf") $ do
    [make| $(cc)ld -o $dst $ofiles |]

------------------------------------

(.=) :: FilePath -> String -> FilePath
(.=) src newext = src ++ newext

top :: FilePath -> FilePath
top x = "/root/" </> x

