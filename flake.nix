{
  description = "Emacs major mode for editing files in Ledger format";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;

        # Files we care about across every check & build.  Excluding the
        # generated bench/ output and any local nix `result' link keeps
        # rebuilds deterministic.
        src = lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let base = baseNameOf path; in
            !(lib.hasSuffix ".elc" base)
            && base != "result"
            && base != "coverage"
            && !(type == "regular" && base == "current.txt");
        };

        # The Emacs we use for *building* and for *checks*.  Loaded with the
        # extra dev tooling so that a single binary covers everything.
        emacsForChecks = pkgs.emacs;
        emacsWithTools =
          (pkgs.emacsPackagesFor emacsForChecks).emacsWithPackages
            (e: with e; [ package-lint elisp-autofmt undercover ]);

        # Common environment for every check / shell script.  Each derivation
        # gets its own writable copy of the source so writes (e.g. coverage
        # output, formatter scratch dirs) don't fight the read-only Nix store.
        commonShellPrelude = ''
          export LEDGER_MODE_ROOT="$PWD"
          export EMACS=${emacsWithTools}/bin/emacs
          export HOME=$(mktemp -d)
        '';

        mkCheck = name: attrs:
          pkgs.runCommand "ledger-mode-check-${name}"
            ({
              inherit src;
              nativeBuildInputs = (attrs.extraInputs or [ ])
                ++ [ emacsWithTools pkgs.bash pkgs.coreutils pkgs.diffutils ];
            } // (attrs.env or { }))
            (
              ''
                set -euo pipefail
                cp -r $src ./source
                chmod -R u+w ./source
                cd ./source
                ${commonShellPrelude}
                ${attrs.script}
                touch $out
              ''
            );

      in
      {
        # ------------------------------------------------------------ packages
        packages = {
          default = self.packages.${system}.ledger-mode;

          ledger-mode =
            (pkgs.emacsPackagesFor emacsForChecks).trivialBuild {
              pname = "ledger-mode";
              version = self.shortRev or self.dirtyShortRev or "dev";
              inherit src;
              packageRequires = [ ];
              # `trivialBuild' compiles every *.el at the top level by default.
              # Treat warnings as errors so a "successful" build really is one.
              preBuild = ''
                export EMACSLOADPATH="$PWD:''${EMACSLOADPATH:-}"
                # Override the compile flags trivialBuild sets up.
                cat > .ledger-precompile.el <<'EOF'
                (setq byte-compile-error-on-warn t)
                (setq byte-compile-warnings t)
                EOF
              '';
              postInstall = ''
                install -Dm644 README.md $out/share/doc/ledger-mode/README.md
                install -Dm644 LICENSE.md $out/share/doc/ledger-mode/LICENSE.md
                if [ -f doc/ledger-mode.info ]; then
                  install -Dm644 doc/ledger-mode.info $out/share/info/ledger-mode.info
                fi
              '';
            };

          docs = pkgs.runCommand "ledger-mode-docs"
            {
              inherit src;
              nativeBuildInputs = [ pkgs.texinfo ];
            }
            ''
              cp -r $src ./source
              chmod -R u+w ./source
              cd ./source/doc
              makeinfo --no-split ledger-mode.texi -o ledger-mode.info
              makeinfo --html --no-split ledger-mode.texi -o ledger-mode.html
              mkdir -p $out/share/info $out/share/doc/ledger-mode
              install -Dm644 ledger-mode.info $out/share/info/ledger-mode.info
              install -Dm644 ledger-mode.html $out/share/doc/ledger-mode/ledger-mode.html
            '';
        };

        # ------------------------------------------------------------- devShell
        devShells.default = pkgs.mkShell {
          name = "ledger-mode";
          packages = [
            emacsWithTools
            pkgs.ledger
            pkgs.lefthook
            pkgs.gnumake
            pkgs.cmake
            pkgs.texinfo
            pkgs.lcov
            pkgs.nixpkgs-fmt
            pkgs.shellcheck
            pkgs.shfmt
          ];
          shellHook = ''
            export LEDGER_MODE_ROOT="$PWD"
            export EMACS=${emacsWithTools}/bin/emacs
            # The banner is helpful for interactive shells but pure noise when
            # `nix develop --command' is invoked from lefthook/CI for every
            # check, so suppress it unless stdin is a tty.
            if [ -t 1 ] && [ -z "''${LEDGER_MODE_QUIET:-}" ]; then
              echo "ledger-mode dev shell — emacs: $($EMACS --version | head -1)"
              echo "  Available targets:"
              echo "    nix build                       (byte-compiled package)"
              echo "    nix build .#docs                (HTML + info manual)"
              echo "    nix flake check                 (run every check)"
              echo "    nix run .#format                (reformat *.el in place)"
              echo "    nix run .#bench                 (refresh bench/current.txt)"
              echo "    nix run .#bench-baseline-update (overwrite bench/baseline.txt)"
              echo "    lefthook run pre-commit         (mirror the pre-commit hook)"
            fi
          '';
        };

        # ----------------------------------------------------------------- apps
        apps =
          let
            mkScript = name: text:
              let
                drv = pkgs.writeShellApplication {
                  inherit name text;
                  runtimeInputs = [ emacsWithTools pkgs.coreutils pkgs.diffutils pkgs.bash ];
                };
              in
              { type = "app"; program = "${drv}/bin/${name}"; };
          in
          {
            default = self.apps.${system}.format;

            format = mkScript "ledger-mode-format" ''
              export LEDGER_MODE_ROOT="''${LEDGER_MODE_ROOT:-$PWD}"
              cd "$LEDGER_MODE_ROOT"
              "${emacsWithTools}/bin/emacs" -Q --batch --load tools/format.el
            '';

            bench = mkScript "ledger-mode-bench" ''
              export LEDGER_MODE_ROOT="''${LEDGER_MODE_ROOT:-$PWD}"
              cd "$LEDGER_MODE_ROOT"
              "${emacsWithTools}/bin/emacs" -Q --batch --load tools/benchmark.el
              cat bench/current.txt
            '';

            bench-baseline-update = mkScript "ledger-mode-bench-baseline-update" ''
              export LEDGER_MODE_ROOT="''${LEDGER_MODE_ROOT:-$PWD}"
              cd "$LEDGER_MODE_ROOT"
              "${emacsWithTools}/bin/emacs" -Q --batch --load tools/benchmark.el
              cp bench/current.txt bench/baseline.txt
              echo "Baseline updated."
            '';
          };

        # --------------------------------------------------------------- checks
        checks = {
          # Byte compile every source file with warnings as errors.
          byte-compile = mkCheck "byte-compile" {
            script = ''
              "$EMACS" -Q --batch --load tools/byte-compile.el
            '';
          };

          # Run the existing ERT suite.
          tests = mkCheck "tests" {
            extraInputs = [ pkgs.ledger ];
            script = ''
              export PATH=${pkgs.ledger}/bin:$PATH
              "$EMACS" -Q --batch --load tools/run-tests.el
            '';
          };

          # package-lint + checkdoc.
          lint = mkCheck "lint" {
            script = ''
              "$EMACS" -Q --batch --load tools/lint.el
            '';
          };

          # Source must already be in formatted form.
          format = mkCheck "format" {
            script = ''
              bash tools/format-check.sh
            '';
          };

          # Tests pass under undercover and coverage hasn't dropped.
          coverage = mkCheck "coverage" {
            extraInputs = [ pkgs.ledger ];
            script = ''
              export PATH=${pkgs.ledger}/bin:$PATH
              "$EMACS" -Q --batch --load tools/coverage.el
              bash tools/coverage-check.sh
            '';
          };

          # Microbenchmarks within 5% of the committed baseline.
          benchmark = mkCheck "benchmark" {
            extraInputs = [ pkgs.ledger ];
            script = ''
              export PATH=${pkgs.ledger}/bin:$PATH
              "$EMACS" -Q --batch --load tools/benchmark.el
              bash tools/benchmark-check.sh
            '';
          };

          # Build the texinfo manual; produces info+html.
          docs = self.packages.${system}.docs;

          # The full byte-compiled package builds.
          build = self.packages.${system}.ledger-mode;

          # All shell scripts must be free of shellcheck warnings.
          shellcheck = pkgs.runCommand "ledger-mode-shellcheck"
            {
              inherit src;
              nativeBuildInputs = [ pkgs.shellcheck ];
            }
            ''
              cd $src
              shellcheck tools/*.sh
              touch $out
            '';

          # Nix files are nixpkgs-fmt-clean.
          nix-format = pkgs.runCommand "ledger-mode-nix-format"
            {
              inherit src;
              nativeBuildInputs = [ pkgs.nixpkgs-fmt ];
            }
            ''
              cd $src
              nixpkgs-fmt --check flake.nix nix/*.nix 2>/dev/null || \
                nixpkgs-fmt --check flake.nix
              touch $out
            '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
