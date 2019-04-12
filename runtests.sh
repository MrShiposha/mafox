#!/bin/bash

olddir=$(pwd)
cd "$(dirname "$0")"

if [ ! -f "./build/tests/mafox-tests" ]; then
    ./rebuild.sh
fi

./build.sh
./build/tests/mafox-tests

cd $olddir
