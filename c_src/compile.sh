#!/bin/sh

VERSION="v3.1.0"


if [ ! -d c_src/h3 ]; then
    git clone https://github.com/uber/h3.git c_src/h3
fi

cd c_src/h3

CURRENT_VERSION=`git describe --tags`

if [ ! "$VERSION" = "$CURRENT_VERSION" ]; then
    git clean -ddxxff
    git fetch
    git checkout $VERSION
fi

if [ ! -d build ]; then
    mkdir build
fi
cd build
if [ ! -f Makefile ]; then
    cmake ..
fi
make -j h3
