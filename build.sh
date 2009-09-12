#!/bin/bash
unset CPATH

runghc Setup.hs configure
runghc Setup.hs build

cp dist/build/arst/arst .

