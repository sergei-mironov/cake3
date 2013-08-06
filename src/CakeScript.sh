#!/bin/sh

die() { echo "$@" >&2 ; exit 1 ; }
err() { echo "$@" >&2 ; }

cakepath() {
cat <<EOF
module ${1}_P where

import Development.Cake3

file :: FilePath -> File
file x = file' ("$2" </> x)

cakefiles :: [File]
cakefiles = case "$2" of
              "." -> map file' \$ $3
              _ -> error "cakefiles are defined for top-level cake only"

EOF
}

caketemplate() {
cat <<"EOF"
{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Cakefile where

import Control.Monad.Loc
import Development.Cake3

import Cakefile_P (file, cakefiles)

elf = rule [file "main.elf"] $ do
    [shell| echo "Your commands go here" ; exit 1 ; |]

all = do
  phony "all" $ do
    depend elf

main = do
  runMake [Cakefile.all, elf] >>= putStrLn . toMake

EOF
}

while test -n "$1" ; do
  case "$1" in
    --help|-h|help) 
      err "Cake3 the Makefile generator help"
      err "https://github.com/grwlf/cake3"
      err "Usage: cake3 [--help|-h] [init]"
      err "cake3 init"
      err "    - Create default Cakefile.hs"
      err "cake3"
      err "    - Build the Makefile"
      exit 1;
      ;;
    init)
      test -f Cakefile.hs &&
        die "Cakefile.hs already exists"
      caketemplate > Cakefile.hs
      echo "Cakefile.hs has been created"
      exit 0;
      ;;
  esac
  shift
done

CWD=`pwd`
T=`mktemp -d`

cakes() {
  find -type f '(' -name 'Cake*\.hs' -or -name 'Cake*\.lhs' ')' -and -not -name '*_P.hs' \
    | grep -v '^\.[a-zA-Z].*'
}

IFS=$'\n'
CAKES=`cakes`
CAKELIST="[]"
for f in $CAKES ; do
  CAKELIST="\"$f\" : $CAKELIST" 
done

MAIN_=
MAIN=
for f in $CAKES ; do
  fname_=$(basename "$f")
  tgt=$T/$fname_
  fname=$(echo "$fname_" | sed 's/\.l\?hs//')
  pname="${fname}_P.hs"
  fdir=$(dirname "$f")

  if test "$fdir" = "." ; then
    if test -n "$MAIN" ; then
      die 'More than one Cake* file in current dir'
    fi
    MAIN=$fname
    MAIN_=$fname_
  fi

  if test -f "$tgt" ; then
    die "More than one file named '${fname}.hs' in the filetree"
  fi

  cp "$f" "$tgt" ||
    die "cp $f $tgt failed. Duplicate names?"

  if cat "$f" | grep -q "import.*${fname}_P" ; then
    echo "Creating $fdir/${pname}" >&2
    cakepath "$fname" "$fdir" "$CAKELIST" > "$fdir/${pname}"

    cp "$fdir/${pname}" "$T/${pname}" ||
      die -n "cp $fdir/${pname} $T/${pname} failed"
  else
    echo "Skipping creating $fdir/${pname}" >&2
  fi
done

if test -z "$MAIN" ; then
  die "No Cake* file exist in the current directory. Consider running \`cake3 --help'."
fi

(
set -e
cd $T
ghc --make "$MAIN_" -main-is "$MAIN" -o Cakegen
cp -t "$CWD" Cakegen
"$CWD/Cakegen" > "$CWD/Makefile"
echo "Makefile created" >&2
)


