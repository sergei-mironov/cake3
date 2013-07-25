{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module CakeLib where

import Control.Monad.Loc
import Development.Cake3

import Cakepath_Lib (file)

cfiles = map file [ "lib.c"]

ofiles cf = do
  forM cfiles $ \c -> do
    rule [c .= "o"] $ do
      [make| gcc -c -I lib $cf -o $dst $c |]

