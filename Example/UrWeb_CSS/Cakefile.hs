module Cakefile where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cakefile_P

main = writeMake (file "Makefile") $ do

  prebuild [cmd|urweb -print-cinclude >/dev/null|]

  rule $ do
    phony "all"
    depend $ uwapp "-dbms sqlite" (file "Main.urp") $ do
      allow url "http://code.jquery.com/ui/1.10.3/jquery-ui.js";
      allow mime "text/javascript";
      allow mime "text/css";
      debug
      embed (file "bootstrap.css")
      ur (file "Main.ur")

  return ()

