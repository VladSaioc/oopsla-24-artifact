#!/bin/bash

TARGET=$1
ROOT=$2
RESULTS=$3

if [ -z $1 ]; then
  echo "Give me a target."
  exit 1
fi

if [ -z $2 ]; then
  ROOT="../benchmarks"
fi

if [ -z $3 ]; then
  RESULTS="$ROOT/results"
fi

ROOTREGEXP=$(echo "$ROOT" | sed "s+\.+\\\.+g")

rm -rf "$RESULTS/$TARGET"

go build github.com/nicolasdilley/gomela

shopt -s globstar
if [ -d $ROOT/$TARGET ]; then
  HASGO=$(ls $ROOT/$TARGET | grep ".go")
  # echo $HASGO
  if [ ! -z "$HASGO" ]; then
    echo "Now processing: $ROOT/$TARGET"
    # ./gomela -am -ginger -result_folder "$RESULTS/$TARGET" fs "$ROOT/$TARGET"
  fi
fi

case $(uname) in
  "Linux")
    P="P"
    ;;
  "Darwin")
    P="p"
    ;;
esac

traverseDir () {
  echo "Now processing: $ROOT/$1"
  if [ ! -d $ROOT/$1 ]; then
    echo "$ROOT/$1 is not a directory."
    return
  fi
  HASGO=$(ls $ROOT/$1 | grep ".go")
  # echo $HASGO
  if [ -z "$HASGO" ]; then
    for l in $(ls $ROOT/$1 \
      | sed "s+$ROOTREGEXP/$1/++g" \
      | sed "s+:++g" \
      | grep -v$P "(\.|vendor|benchmarks|examples|testdata|/test/)");
    do
      traverseDir $1/$l
    done
    return
  fi
  ./gomela -am -ginger -result_folder "$RESULTS/$1" fs "$ROOT/$1"

  for l in $(ls $ROOT/$1 \
    | sed "s+$ROOTREGEXP/$1/++g" \
    | sed "s+:++g" \
    | grep -v$P "(\.|vendor|benchmarks|examples|testdata|/test/)");
  do
    traverseDir $1/$l
  done
}

for l in $(ls $ROOT/$TARGET \
  | sed "s+$ROOTREGEXP/$TARGET/++g" \
  | sed "s+:++g" \
  | grep -v$P "(\.|vendor|benchmarks|examples|testdata|/test/)");
do
  echo $l
  traverseDir $TARGET/$l
done
