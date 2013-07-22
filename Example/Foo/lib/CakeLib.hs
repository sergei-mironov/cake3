{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module CakeLib where

import Control.Monad.Loc
import Development.Cake3

import Cakepath_Lib (top)

cfiles = map (top </>) [ "lib.c"]

ofiles cf = do
  forM cfiles $ \c -> do
    rule (c .= "o") $ do
      [make| gcc $cf -o $dst $c |]

