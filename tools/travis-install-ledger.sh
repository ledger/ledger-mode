#!/usr/bin/env bash

set -eu -o pipefail
set -o xtrace

BRANCH=$1

if [ "${TRAVIS_OS_NAME}" = "linux" ]; then
    if [ "${BRANCH}" = "apt-get" ]; then
        sudo apt-get update -qq
        sudo apt-get install -qq ledger
    else
        sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
        sudo apt-get update -qq
        sudo apt-get install -qq gcc-4.8 g++-4.8
        sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 90
        sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 90

        sudo apt-get install -qq libboost-all-dev
        sudo apt-get install -qq libgmp-dev libmpfr-dev libedit-dev

        curl -sL https://github.com/ledger/ledger/archive/$BRANCH.tar.gz | tar xz
        cd ledger-$BRANCH

        cmake .
        make -j2
    fi
fi

if [ "${TRAVIS_OS_NAME}" = "osx" ]; then
    brew update
    brew install ledger
fi
