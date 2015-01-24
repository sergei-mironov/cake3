module Cakefile where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cakefile_P

instance IsString File where fromString = file

project = do

  prebuild [cmd|urweb -print-cinclude >/dev/null|]

  rule $ do
    phony "all"
    depend $ uwapp "-dbms sqlite" "Main.urp" $ do
      allow url "http://code.jquery.com/ui/1.10.3/jquery-ui.js";
      allow mime "text/javascript";
      allow mime "text/css";
      debug
      bin "bootstrap.css" [NoScan]
      ur (single "Main.ur")

  return ()

main = do
  writeMake (file "Makefile") (project)
  writeMake (file "Makefile.devel") (selfUpdate >> project)

