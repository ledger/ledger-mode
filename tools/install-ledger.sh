#!/usr/bin/env bash

set -eu -o pipefail
set -o xtrace

VERSION=$1

case "$VERSION" in
    stable)
        nix-env -iA ledger -f '<nixpkgs>'
        ;;
    snapshot)
        echo "$0: snapshot version of ledger currently unsupported"
        ;;
    *)
        echo "$0: Unknown branch: $VERSION"
        ;;
esac
