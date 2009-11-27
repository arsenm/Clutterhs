#/usr/bin/env sh

for i in `ls *.hs`
  do
    ghc --make -hide-package transformers $i
done

