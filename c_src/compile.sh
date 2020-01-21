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

cmake                          \
    -H.                        \
    -Bbuild                    \
    -DBUILD_TESTING=OFF        \
    -DCMAKE_BUILD_TYPE=Release \
    -DENABLE_COVERAGE=OFF      \
    -DENABLE_DOCS=OFF          \
    -DENABLE_FORMAT=OFF        \
    -DENABLE_LINTING=OFF

cmake --build build --target h3
