#!/bin/sh

set -e

# Download and install dafny distribution
# Determine OS and assign it to OS
case $(uname) in
  "Linux")
    OS="ubuntu-20.04"
    ;;
  "Darwin")
    OS="macos-11"
    ;;
esac

# Determine architecture and assign it to ARCH
case $(arch) in
  "x86_64")
    ARCH="x64"
    ;;
  "i386")
    ARCH="x64"
    ;;
  "arm64")
    ARCH="arm64"
esac

DAFNY_V="4.0.0"

URL="https://github.com/dafny-lang/dafny/releases/download/v$DAFNY_V/dafny-$DAFNY_V-$ARCH-$OS.zip"
DAFZIP="dafny-$DAFNY_V-$ARCH-$OS.zip"

[ ! -f "./$DAFZIP" ] && wget $URL

[ ! -d "./dafny" ] && unzip $DAFZIP
