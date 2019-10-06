#!/usr/bin/env bash

set -eu -o pipefail
set -o xtrace

VERSION=$1
THISDIR=$(cd "$(dirname "$0")" && pwd)

case "$VERSION" in
    stable)
        nix-env -iA stable -f $THISDIR/default.nix
        ;;
    snapshot)
        nix-env -iA snapshot -f $THISDIR/default.nix
        ;;
    *)
        echo "$0: Unknown branch: $VERSION"
        ;;
esac
