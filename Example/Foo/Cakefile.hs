{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad.Loc
import Development.Cake3

import Cakepath_Main (file)
import qualified CakeLib as L

sound = "Yuupee" :: String

cflags = makevar "CFLAGS" "-O0 -g3"

shell = extvar "SHELL"

cfiles = map file [ "main.c"]

ofiles = do
  forM cfiles $ \c -> do
    rule [c .= "o"] $ do
      [make| gcc -I lib -c $cflags -o $dst $c |]

allofiles = ofiles ++ (L.ofiles cflags)

elf = do
  rule [file "main.elf"] $ do
    [make| echo "SHELL is $shell" |]
    [make| gcc -o $dst $allofiles |]
    [make| echo $sound |]

clean = do
  phony "clean" $ do
    unsafe $ do
      [make| rm $elf ; rm GUARD_* ; rm $allofiles |]

all = do
  phony "all" $ depend (head elf)

main = do
  runMake (Main.all ++ elf ++ clean) >>= putStrLn . toMake

