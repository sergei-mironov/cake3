{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Cakefile where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cakefile_P (file)

main = writeMake "Makefile" $ do

  prebuild [cmd|urweb -print-cinclude >/dev/null|]

  u <- uwlib "Script.urp" $ do
    ffi "Script.urs"
    include "Script.h"
    link "Script.o"

  t1 <- uwapp "-dbms sqlite" "Test1.urp" $ do
    allow url "http://code.jquery.com/ui/1.10.3/jquery-ui.js";
    allow mime "text/javascript";
    library (internal u);
    debug
    module_ (single "Test1.ur")

  t2 <- uwapp "-dbms sqlite" "Test2.urp" $ do
    library (internal u);
    module_ (single "Test2.ur")

  rule $ do
    phony "all"
    depend u
    depend t1
    depend t2

  return ()
