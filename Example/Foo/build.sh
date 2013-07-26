#!/bin/sh

set +x

ghc -ilib --make Cakefile.hs && ./Cakefile | tee Makefile

echo "Makefile created. Type make to compile"

