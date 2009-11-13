#!/bin/bash

runghc Setup.hs configure --user
#runghc Setup.hs configure --with-compiler=ghc-6.12.0.20091010 --user
runghc Setup.hs build
runghc Setup.hs register --inplace --user


