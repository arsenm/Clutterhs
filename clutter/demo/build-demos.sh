#/usr/bin/env sh

for i in `ls *.hs`
  do
    ghc --make -hide-package monads-tf $i
done

