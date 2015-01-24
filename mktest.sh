#!/bin/sh


if test -n "$1" ; then
  export FLT="grep -e $1"
else
  export FLT="cat"
fi

set -x
for e in `find ./Example -mindepth 1 -maxdepth 1 -type d | $FLT` ; do
  (
  set -e -x
  cd $e;
  rm -rf autogen || true
  cake3
  make
  ) || {
    echo "Error(s) detected in $e"
    break
  }
done
