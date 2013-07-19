{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad
import Development.Cake3

cflags = var "CFLAGS" "-O0 -g3"

cfiles = [ top "main.c", top "second.c" ]

ofiles = do
  forM cfiles $ \c -> do
    rule (c .= "o") $ do
      [make| "gcc $cflags -o $dst $c" |]

elf = do
  rule (top "main.elf") $ do
    [make| "ld -o $dst $ofiles" |]

------------------------------------

(.=) :: FilePath -> String -> FilePath
(.=) src newext = src ++ newext

top :: FilePath -> FilePath
top x = x

