Thirdcake
---------

Thirdcake is a Makefile DSL written in Haskell. It can help you to define clean
and correct Makefile for your project. Currenly, only GNU Make dialect is
supported.


Installing
==========

  1. Install (Haskell Platform)[http://www.haskell.org/platform/]

  2. Download the Thirdcake

         git clone http://github.com/grwlf/thirdcake

  3. Install dependencies
    
         cabal install haskell-src-meta monadloc QuasiText

  3. Build it using Cabal from Platform

         cd thirdcake
         cabal configure && cabal install


