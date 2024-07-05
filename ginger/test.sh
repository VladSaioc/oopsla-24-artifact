#!/bin/bash

set -e fail

verifyIRExample () {
  local dir=examples/$1
  local curr=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      # echo "Directory $dir/$file"
      verifyIRExample $curr/$file
    else
      if [[ $file = *".t" ]];
      then
        echo "Now testing: $dir/$file"
        stack run -- -ir -color "$dir/$file"
        echo "Success: $dir/$file"
      else
        # echo "File $dir/$file"
        echo "Skipping non-VIRGo file $file"
      fi
    fi
  done
}

translateIRExample () {
  local dir=examples/$1
  local curr=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      translateIRExample $curr/$file
    else
      if [[ $file = *".t" ]];
      then
        echo "Now testing: $dir/$file"
        stack run -- -ir -color -skip-verification "$dir/$file"
        echo "Success: $dir/$file"
      else
        echo "Skipping non-VIRGo file $file"
      fi
    fi
  done
}

verifyPromelaExample () {
  local dir=examples/gomelas/$1
  local curr=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      verifyPromelaExample $curr/$file
    else
      if [[ $file = *".pml" ]];
      then
        echo "Now testing: $dir/$file"
        stack run -- -color "$dir/$file"
        echo "Success: $dir/$file"
      else
        echo "Skipping non-Promela file $file"
      fi
    fi
  done
}

# Translation and verification of VIRGo
verifyIRExample good

# VIRGo translation only
translateIRExample translatable

# Translation and verification of Promela
verifyPromelaExample good

# Promela translation only
# --- Nothing here yet

echo ""
echo "All tests executed successfully"
