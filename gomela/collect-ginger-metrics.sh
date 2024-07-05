#!/bin/bash

TARGET=$1
RESULTS=$2

if [ -z $2 ]; then
  RESULTS="../benchmarks/results"
fi

if [ ! -z $TARGET ]; then
  TARGET="/$TARGET"
fi

if [ ! -d $RESULTS ]; then
  echo "Results directory $RESULTS does not exist"
  exit 1
fi

RESULTS_REGEXP=$(echo $RESULTS | sed "s+\.+\\\.+g")


CSV="$RESULTS$TARGET/ginger-metrics.csv"
rm $CSV
touch $CSV

HARD_SCOPE_LOG="Fragments in scope (hard): "
SOFT_SCOPE_LOG="Fragments in scope (soft): "
OUT_SCOPE_LOG="Fragments out of scope: "
HARD_SCOPE_GOMELA_FAIL_LOG="Gomela front-end failure, fragments in scope (hard): "
SOFT_SCOPE_GOMELA_FAIL_LOG="Gomela front-end failure, fragments in scope (soft): "
OUT_SCOPE_GOMELA_FAIL_LOG="Gomela front-end failure, fragments out of scope: "

echo "Package,Hard scope,Soft scope,Out of scope,Hard scope (front-end failure),Soft scope (front-end failure), Out of scope (front-end failure)" > $CSV

shopt -s globstar
for l in $(ls $RESULTS$TARGET/**/log.csv);
do
  # echo $l
  GREPRES=$(grep -P "Fragments in " "$l")
  if [ ! -z "$GREPRES" ]; then
    echo "Now processing: $l"
    PACKAGE=$(echo $l | sed "s+$RESULTS_REGEXP/++g" | sed "s+/log\.csv++g")
    HARDROW=$(grep "$HARD_SCOPE_LOG" $l | sed "s+$HARD_SCOPE_LOG++g")
    SOFTROW=$(grep "$SOFT_SCOPE_LOG" $l | sed "s+$SOFT_SCOPE_LOG++g")
    OUTROW=$(grep "$OUT_SCOPE_LOG" $l | sed "s+$OUT_SCOPE_LOG++g")
    GOMELA_FAIL_HARDROW=$(grep "$HARD_SCOPE_GOMELA_FAIL_LOG" $l | sed "s+$HARD_SCOPE_GOMELA_FAIL_LOG++g")
    GOMELA_FAIL_SOFTROW=$(grep "$SOFT_SCOPE_GOMELA_FAIL_LOG" $l | sed "s+$SOFT_SCOPE_GOMELA_FAIL_LOG++g")
    GOMELA_FAIL_OUTROW=$(grep "$OUT_SCOPE_GOMELA_FAIL_LOG" $l | sed "s+$OUT_SCOPE_GOMELA_FAIL_LOG++g")
    echo "$PACKAGE,$HARDROW,$SOFTROW,$OUTROW,$GOMELA_FAIL_HARDROW,$GOMELA_FAIL_SOFTROW,$GOMELA_FAIL_OUTROW" >> $CSV
  fi
  # echo $(grep "Fragments in scope (hard): [\d]+" $l )
done