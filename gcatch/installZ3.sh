#!/usr/bin/env bash
#
# Install z3 from sources
# GCatch requires z3 WITH SOURCES
# You can also checkout https://github.com/Z3Prover/z3 to install Z3
PREF=$1

installZ3() {
  cd ./tools/z3 || exit 1
  if [ -z "${PREF}" ]
  then
    python3 scripts/mk_make.py
  else
    python3 scripts/mk_make.py --prefix="${PREF}"
  fi
  cd build || exit 1
  make
  if [ -z "${PREF}" ]
  then
    sudo make install
  else
    make install
    chmod u=x "${PREF}/bin/z3"
  fi
}

# cd script directory
cd "$(dirname "$(realpath "$0")")" || exit 1

if [ ! -z "${PREF}" ]
then
  echo "Installing z3 at ${PREF}"
  installZ3
  exit
fi

# check z3 bin
if ! Z3=$(which z3); then
  echo 'Cannot detect z3'
  installZ3
  exit
fi

# check z3 header
if echo '#include <z3.h>' | gcc -H -fsyntax-only -E - 1>/dev/null 2>&1; then
  read -p 'z3 and <z3.h> exist. Reinstall z3 from sources? [y/N] ' -r yn
  case $yn in
    [Yy]* ) installZ3;;
    * ) exit 0;;
  esac
else
  read -p 'GCatch requires z3 WITH SOURCES. Reinstall z3 from sources? [Y/n] ' -r yn
  case $yn in
    [Nn]* ) exit 0;;
    * ) installZ3;;
  esac
fi

exit 0
