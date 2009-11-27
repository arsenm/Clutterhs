#!/usr/bin/env sh

runghc Setup.hs configure --user
runghc Setup.hs build
runghc Setup.hs install

if [ $1 ]; then
    echo "Building demos"
    for i in `ls demo/*.hs`
      do
        ghc --make -hide-package transformers $i
    done
fi


