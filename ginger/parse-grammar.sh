case $1 in
  "VIRGo")
  ;;
  "Promela")
  ;;
  *)
    echo "Unrecognized grammar."
    exit 1
  ;;
esac

bnfc --haskell --outputdir=src -p $1 src/$1.cf
stack run alex -- src/$1/Lex$1.x
stack run happy -- src/$1/Par$1.y
rm src/$1/Test$1.hs
