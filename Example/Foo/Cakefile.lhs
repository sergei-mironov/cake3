> {-# LANGUAGE QuasiQuotes, OverloadedStrings #-}


> module Cakefile where

Import tha Cake3 library

> import Development.Cake3

Import paths. This file will be generated by the cake3 script.

> import Cakefile_P

Foo uses a library with it's own cakefile in ./lib. So we import it as usual
haskell module

> import qualified CakeLib as L

> cflags = makevar "CFLAGS" "-O0 -g3"
> shellname = extvar "SHELL"
> greeting = "Yuupee" :: String
>
> cfiles = [file "main.c"]

> rule_for_each l a = concat <$> forM l a
>
> os_local = rule_for_each cfiles $ \c -> rule $ do
>   shell [cmd| gcc -I lib -c $cflags -o @(c.="o") $c |]

> os = do
>   o1 <- os_local 
>   o2 <- L.os cflags
>   return (o1++o2)

> elf = rule $ do
>   shell [cmd| echo $(string greeting) |]
>   shell [cmd| echo SHELL is $shellname |]
>   shell [cmd| echo CFLAGS is $cflags |]
>   shell [cmd| gcc $cflags -o @(file "main.elf") $os |]

> main = writeMake (file "Makefile") $ do
>   rule $ do
>     phony "clean"
>     unsafeShell [cmd| rm $elf ; rm $os |]
>   rule $ do
>     phony "all"
>     depend elf


