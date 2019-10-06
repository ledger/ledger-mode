{ pkgs ? import <nixpkgs> {} }:

let
  ledgerFromGit = builtins.fetchGit { url = git://github.com/ledger/ledger; };
in
{
  stable = pkgs.ledger;

  snapshot =  (pkgs.callPackage ledgerFromGit {}).overrideAttrs(a: { doCheck = false; });
}
