{-# LANGUAGE QuasiQuotes #-}
module CakeLib where

import Development.Cake3
import CakeLib_P (file)



-- [fa, fb] = (map file [ "file.a", "file.b" ]) `rule` do
--   shell [cmd| echo aa > $(fa) |]
--   shell [cmd| echo bb > $(fb) |]

fa = (file "file.a") `rule` do
  shell [cmd| echo aa > $(fa) |]

main = do
  runMake [fa] >>= putStrLn . toMake


-- type Recipe = [(String, IO ())]


-- rule :: [String] -> (IO ()) -> Recipe
-- rule s io = map (\x -> (x,io)) s

-- [a,b] = ["a","b"] `rule` do
--   putStrLn (fst a)
--   putStrLn (fst b)


-- main = do
--   snd a
--   snd b
