#!/bin/bash

recursiveVerify () {
  local dir=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      find . -wholename "$dir/$file/*.dfy" -type f -delete
      find . -wholename "$dir/$file/*.res" -type f -delete
      find . -wholename "$dir/$file/*.dll" -type f -delete
      find . -wholename "$dir/$file/*.runtimeconfig.json" -type f -delete
      recursiveVerify $dir/$file
    else
      if [[ $file = *"-ginger.pml" ]]; then
        echo "Now testing: $dir/$file"
        timeout 360 ginger-exe "$dir/$file" &> "$dir/$file-results.res"
      fi
    fi
  done
}

recursiveVerify $1
exit 0
