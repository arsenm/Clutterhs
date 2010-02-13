1;2202;0c#!/usr/bin/env bash

pushd cogl
./build.sh
popd
pushd clutter
./build.sh
popd
pushd clutter-gtk
./build.sh
popd
pushd clutter-gst
./build.sh
popd

