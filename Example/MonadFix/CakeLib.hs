{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
module CakeLib where

import Control.Monad
import Development.Cake3
import CakeLib_P (file)

main = mdo

  inputs <- return (map file [ "file1.inp", "file2.inp" ])

  objs <- forM inputs $ \inp -> do
    let outp = (inp .= "outp")
    ruleM outp $ do
      shell [cmd| cat $inp > $outp |]

  hehe <- ruleM (file "file.hehe") $ do
    shell [cmd| cat $(head objs) > $hehe |]

  runMake $ do
    place hehe
    place objs

