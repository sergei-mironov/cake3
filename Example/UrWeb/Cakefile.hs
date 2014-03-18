module Cakefile where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cakefile_P

instance IsString File where fromString = file

project = do

  prebuild [cmd|urweb -print-cinclude >/dev/null|]

  u <- uwlib "Script.urp" $ do
    ffi "Script.urs"
    include "Script.h"
    link "Script.o"
    pkgconfig "jansson"

  t1 <- uwapp "-dbms sqlite" "Test1.urp" $ do
    allow url "http://code.jquery.com/ui/1.10.3/jquery-ui.js";
    allow mime "text/javascript";
    library u;
    debug
    ur (single "Test1.ur")

  t2 <- uwapp "-dbms sqlite" "Test2.urp" $ do
    library u;
    ur (single "Test2.ur")

  rule $ do
    phony "all"
    depend u
    depend t1
    depend t2

  return ()

main = do
  writeMake (file "Makefile") (project)
  writeMake (file "Makefile.devel") (selfUpdate >> project)

