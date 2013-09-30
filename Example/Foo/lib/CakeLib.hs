{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}

module CakeLib where

import Development.Cake3
import CakeLib_P (file)

librules var = do
  o <- ofiles var
  c <- clean var
  return $ o ++ [c]

clean :: Variable -> Make Alias
clean var = phonyM "clean" $ do
  unsafe (shell [cmd| rm $(ofiles var) |])

ofiles :: Variable -> Make [Alias]
ofiles var = mdo
  let parseSomething = return (file "lib.c")
  c <- parseSomething
  o <- ruleM [c .= "o"] $ do
    shell [cmd| gcc -c -I lib $var -o $o $c |]
  return o

main = runMake_ $ do
  let var = makevar "CFLAGS" ""
  place $ ofiles var
  place $ clean var
