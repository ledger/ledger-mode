#!/usr/bin/env bash

set -eu -o pipefail
set -o xtrace

BRANCH=$1
LAST_BUILD_FILE=travis-last-build

if [ "${TRAVIS_OS_NAME}" = "linux" ]; then
    if [ "${BRANCH}" = "apt-get" ]; then
        sudo apt-get update -qq
        sudo apt-get install -qq ledger
    else
        sudo apt-get install -qq git
        sudo apt-get install -qq libboost-all-dev # ledger needs Boost's runtime libraries
        sudo apt-get install -qq libgmp-dev libmpfr-dev libedit-dev

        if [ ! -d "ledger-$BRANCH/.git" ]; then
            git clone --depth 1 -b "$BRANCH" "https://github.com/ledger/ledger/" "ledger-$BRANCH"
        fi

        cd "ledger-$BRANCH"

        git fetch origin
        git reset --hard "origin/$BRANCH"

        REV="$(git rev-parse "$BRANCH")"

        if [ "$(cat "$LAST_BUILD_FILE" 2> /dev/null)" = "$REV" ]; then
            echo "Build is up to date"
        else
            sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
            sudo apt-get update -qq
            sudo apt-get install -qq gcc-4.8 g++-4.8
            sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 90
            sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 90

            cmake .
            make -j2
            echo "$REV" > "$LAST_BUILD_FILE"
        fi
    fi
fi

if [ "${TRAVIS_OS_NAME}" = "osx" ]; then
    brew update
    brew install ledger
fi
