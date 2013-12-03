{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module CakeLib(os) where

import Development.Cake3
import CakeLib_P

os (var::Variable) = do
  let c = file "lib.c"
  rule $ do
    shell [cmd| gcc -c -I lib $var -o @(c.="o") $c |]

defaultFlags = makevar "CFLAGS" ""

main = writeMake (file "Makefile") $ do
  fs <- os defaultFlags

  rule $ do
    phony "clean"
    unsafeShell [cmd| rm $(fs)|]

  rule $ do
    phony "all"
    depend fs

