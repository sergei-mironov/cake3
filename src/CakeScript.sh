#!/bin/sh

die() { echo "$@" >&2 ; exit 1 ; }

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

CWD=`pwd`
T=`mktemp -d`

cakes() {
  find -type f -name 'Cake*\.hs' -and -not -name '*_P.hs' \
    | grep -v '^\.[a-zA-Z].*'
}

IFS=$'\n'
CAKES=`cakes`
CAKELIST="[]"
for f in $CAKES ; do
  CAKELIST="\"$f\" : $CAKELIST" 
done
MAIN=
for f in $CAKES ; do
  tgt=$T/`basename "$f"`
  fname=`basename "$f" .hs`
  fdir=`dirname "$f"`

  if test "$fdir" = "." ; then
    if test -n "$MAIN" ; then
      die 'More than one Cake* file in current dir'
    fi
    MAIN=$fname
  fi

  if test -f "$tgt" ; then
    die "More than one file named '${fname}.hs' in the filetree"
  fi

  cp "$f" "$tgt" ||
    die "cp $f $tgt failed. Duplicate names?"

  if cat $f | grep -q "^import.*${fname}_P" ; then
    echo "Creating $fdir/${fname}_P.hs" >&2
    cakepath "$fname" "$fdir" "$CAKELIST" > $fdir/${fname}_P.hs

    cp "$fdir/${fname}_P.hs" "$T/${fname}_P.hs" ||
      die -n "cp $fdir/${fname}_P.hs $T/${fname}_P.hs failed"
  else
    echo "Skipping creating $fdir/${fname}_P.hs" >&2
  fi
done

if test -z "$MAIN" ; then
  die "No Cake* file exist in the current directory"
fi

(
set -e
cd $T
ghc --make "${MAIN}.hs" -main-is $MAIN -o Cakegen
cp -t $CWD Cakegen
$CWD/Cakegen > $CWD/Makefile
echo "Makefile created" >&2
)


