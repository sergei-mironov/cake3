#!/bin/sh

ghc -ilib --make Cakefile.hs && ./Cakefile | tee Makefile

