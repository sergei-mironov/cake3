{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad.Loc
import Development.Cake3

import Cakefile_P (file)

foodef = "-DFOO" :: String

cc = makevar "CROSS_COMPILE" "arm-none-eabi-"

shell = extvar "SHELL"

cflags = makevar "CFLAGS" "-O0 -g3"

cfiles = map file [ "main.c", "second.c",  "space file.c"]

ofiles = do
  forM cfiles $ \c -> do
    rule [c .= "o"] $ do
      [make| $(cc)gcc $foodef $cflags -o $dst $c |]

cfilesAutoSearch :: Make [File]
cfilesAutoSearch = do
  return $ map file ["mains1.c", "mains2.c" ]

ofilesM = do
  fs <- cfilesAutoSearch
  forM fs $ \c -> do
    return $ rule [c .= "o"] $ do
      [make| $(cc)gcc $foodef $cflags -o $dst $c |]

[elf,elf2] = rule [file "main.elf", file "main2.elf" ] $ do
    [make| echo "Shell value is $shell" |]
    [make| $(cc)ld -o $dst $ofilesM |]

all' = do
  phony "all" $ do
    depend elf

main = do
  runMake [all'] >>= putStrLn . toMake

