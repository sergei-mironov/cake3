{-# LANGUAGE QuasiQuotes #-}
module CakeLib where

import Development.Cake3
import CakeLib_P (file)

[fa, fb] = (map file [ "file.a", "file.b" ]) `rule` do
  shell [cmd| echo aa > $(fa) |]
  shell [cmd| echo bb > $(fb) |]

fc = (file "file.c") `rule` shell [cmd| echo cc > $(fc) |]

hehe = rule (file "file.hehe") $ do
  shell [cmd|cat $fa > $hehe |]

main = runMake [fb,hehe]
