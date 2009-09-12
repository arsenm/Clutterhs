#!/bin/bash
unset CPATH

runghc Setup.hs configure
runghc Setup.hs build

