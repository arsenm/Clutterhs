#!/usr/bin/env sh

runghc Setup.hs configure --user
runghc Setup.hs build
runghc Setup.hs install

