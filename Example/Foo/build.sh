#!/bin/sh

set +x

set -e

ghc --make Cakefile.hs -main-is Cakefile

./Cakefile > ./Makefile

cat Makefile

echo "Makefile created. Type make to compile"

