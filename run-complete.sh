#!/bin/bash

set -e

case $1 in
  "-apple-silicon")
    case $(uname) in
      "Darwin")
        case $(arch) in
          "arm64")
            # Prepare M1/M2/M3 machine setup with Colima
            brew install colima
            colima start --cpu 4 --memory 16 --disk 50 --arch amd64
            ;;
        esac
        ;;
    esac
    ;;
esac


docker build -t ginger --target ginger-complete $PWD

docker run --rm -it ginger
