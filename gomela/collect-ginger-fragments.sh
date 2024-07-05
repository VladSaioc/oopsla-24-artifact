#!/bin/bash

TARGET=$1
RESULTS=$2

if [ -z $2 ]; then
  RESULTS="../benchmarks/results"
fi

if [ -z $3 ]; then
  REPO_STORAGE="../benchmarks"
fi

if [ ! -z $TARGET ]; then
  TARGET="/$TARGET"
fi

if [ ! -d $RESULTS ]; then
  echo "Results directory $RESULTS does not exist"
  exit 1
fi

REPO_STORAGE=$(cd $REPO_STORAGE; echo $PWD | sed "s+\.+\\\.+g")

CSV_HARD="$RESULTS$TARGET/ginger-fragments-hard.csv"
CSV_SOFT="$RESULTS$TARGET/ginger-fragments-soft.csv"
rm $CSV_HARD
rm $CSV_SOFT
touch $CSV_HARD
touch $CSV_SOFT

HARD_SCOPE_LOG="The following fragment is in scope (hard). Located at:"
SOFT_SCOPE_LOG="The following fragment is in scope (soft). Located at:"
HARD_SCOPE_GOMELA_FAIL_LOG="The following Gomela unparsable fragment is in scope (hard). Located at:"
SOFT_SCOPE_GOMELA_FAIL_LOG="The following Gomela unparsable fragment is in scope (soft). Located at:"

echo "Fragment,Gomela success" > $CSV_HARD
echo "Fragment,Gomela success" > $CSV_SOFT

shopt -s globstar
for l in $(ls $RESULTS$TARGET/**/log.csv);
do
  # echo $l
  GREPRES=$(grep -P "Fragments in " "$l")
  if [ ! -z "$GREPRES" ]; then
    echo "Now processing: $l"

    HAS_FRAGMENTS=$(grep "$HARD_SCOPE_LOG" $l)
    if [ ! -z "$HAS_FRAGMENTS" ]; then
      while read -r fragment
      do
        echo "$fragment,1" >> $CSV_HARD
      done <<< $(grep "$HARD_SCOPE_LOG" $l | sed "s+$HARD_SCOPE_LOG++g")
    fi
    
    HAS_FRAGMENTS=$(grep -P "$HARD_SCOPE_GOMELA_FAIL_LOG" $l)
    if [ ! -z "$HAS_FRAGMENTS" ]; then
      while read -r fragment
      do
        echo "$fragment,1" >> $CSV_HARD
      done <<< $(grep "$HARD_SCOPE_GOMELA_FAIL_LOG" $l | sed "s+$HARD_SCOPE_GOMELA_FAIL_LOG++g")
    fi

    HAS_FRAGMENTS=$(grep "$SOFT_SCOPE_LOG" $l)
    if [ ! -z "$HAS_FRAGMENTS" ]; then
      while read -r fragment
      do
        echo "$fragment,0" >> $CSV_SOFT
      done <<< $(grep "$SOFT_SCOPE_LOG" $l | sed "s+$SOFT_SCOPE_LOG++g")
    fi

    HAS_FRAGMENTS=$(grep "$SOFT_SCOPE_GOMELA_FAIL_LOG" $l)
    # echo $HAS_FRAGMENTS
    if [ ! -z "$HAS_FRAGMENTS" ]; then
      while read -r fragment
      do
        echo "$fragment,0" >> $CSV_SOFT
      done <<< $(grep "$SOFT_SCOPE_GOMELA_FAIL_LOG" $l | sed "s+$SOFT_SCOPE_GOMELA_FAIL_LOG++g")
    fi
  fi
  # echo $(grep "Fragments in scope (hard): [\d]+" $l )
done