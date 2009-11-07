#!/bin/bash

runghc Setup.hs configure --user
runghc Setup.hs build
runghc Setup.hs register --inplace --user

