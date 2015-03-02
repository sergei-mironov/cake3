module Cakefile where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cakefile_P

main = writeMake (file "Makefile") $ do

  prebuild [cmd|urweb -version|]

  u <- uwlib (file "Script.urp") $ do
    ffi (file "Script.urs")
    include (file "Script.h")
    src (file "Script.c")
    pkgconfig "jansson"

  t1 <- uwapp "-dbms sqlite" (file "Test1.urp") $ do
    allow url "http://code.jquery.com/ui/1.10.3/jquery-ui.js"
    allow mime "text/javascript"
    library u
    debug
    ur (file "Test1.ur")

  t2 <- uwapp "-dbms sqlite" (file "Test2.urp") $ do
    library u
    ur (file "Test2.ur")

  rule $ do
    phony "all"
    depend u
    depend t1
    depend t2

  return ()

