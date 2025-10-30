{
  description = "A Collection of Effectful Libraries";

  # nix
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";

  # haskell
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  inputs.exception-utils = {
    url = "github:tbidne/exception-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  inputs.fs-utils = {
    url = "github:tbidne/fs-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  outputs =
    inputs@{
      flake-parts,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc9122";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                Cabal-syntax_3_10_3_0 = hlib.doJailbreak prev.Cabal-syntax_3_10_3_0;
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
              ];
          };
          pkgsCompiler = {
            inherit pkgs compiler;
          };
          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };
          hsOverlay = (
            compiler.extend (
              hlib.compose.packageSourceOverrides {
                effectful-utils = ./lib/effectful-utils;
                environment-effectful = ./lib/environment-effectful;
                fs-effectful = ./lib/fs-effectful;
                ioref-effectful = ./lib/ioref-effectful;
                logger-effectful = ./lib/logger-effectful;
                optparse-effectful = ./lib/optparse-effectful;
                process-dynamic-effectful = ./lib/process-dynamic-effectful;
                stm-effectful = ./lib/stm-effectful;
                terminal-effectful = ./lib/terminal-effectful;
                concurrent-effectful = ./lib/concurrent-effectful;
                time-effectful = ./lib/time-effectful;
                typed-process-dynamic-effectful = ./lib/typed-process-dynamic-effectful;
                unix-compat-effectful = ./lib/unix-compat-effectful;
                unix-effectful = ./lib/unix-effectful;
              }
            )
          );
          packages = p: [
            p.environment-effectful
            p.fs-effectful
            p.ioref-effectful
            p.logger-effectful
            p.optparse-effectful
            p.process-dynamic-effectful
            p.stm-effectful
            p.terminal-effectful
            p.concurrent-effectful
            p.time-effectful
            p.typed-process-dynamic-effectful
            p.unix-compat-effectful
            p.unix-effectful
          ];

          mkPkg =
            name: root: source-overrides:
            compiler.developPackage {
              inherit name root source-overrides;
              returnShellEnv = false;
            };
          compilerPkgs = {
            inherit compiler pkgs;
          };
        in
        {
          packages.concurrent-effectful = mkPkg "concurrent-effectful" ./lib/concurrent-effectful { };
          packages.effectful-utils = mkPkg "effectful-utils" ./lib/effectful-utils { };
          packages.env-guard-effectful = mkPkg "env-guard-effectful" ./lib/env-guard-effectful { };
          packages.environment-effectful = mkPkg "environment-effectful" ./lib/environment-effectful {
            effectful-utils = ./lib/effectful-utils;
          };
          packages.fs-effectful = mkPkg "fs-effectful" ./lib/fs-effectful {
            effectful-utils = ./lib/effectful-utils;
            ioref-effectful = ./lib/ioref-effectful;
            unix-compat-effectful = ./lib/unix-compat-effectful;
          };
          packages.ioref-effectful = mkPkg "ioref-effectful" ./lib/ioref-effectful { };
          packages.logger-effectful = mkPkg "logger-effectful" ./lib/logger-effectful {
            concurrent-effectful = ./lib/concurrent-effectful;
            effectful-utils = ./lib/effectful-utils;
            time-effectful = ./lib/time-effectful;
          };
          packages.optparse-effectful = mkPkg "optparse-effectful" ./lib/optparse-effectful {
            effectful-utils = ./lib/effectful-utils;
            fs-effectful = ./lib/fs-effectful;
            ioref-effectful = ./lib/ioref-effectful;
            unix-compat-effectful = ./lib/unix-compat-effectful;
          };
          packages.process-dynamic-effectful =
            mkPkg "process-dynamic-effectful" ./lib/process-dynamic-effectful
              {
                effectful-utils = ./lib/effectful-utils;
              };
          packages.stm-effectful = mkPkg "stm-effectful" ./lib/stm-effectful { };
          packages.terminal-effectful = mkPkg "terminal-effectful" ./lib/terminal-effectful {
            effectful-utils = ./lib/effectful-utils;
          };
          packages.time-effectful = mkPkg "time-effectful" ./lib/time-effectful {
            effectful-utils = ./lib/effectful-utils;
          };
          packages.typed-process-dynamic-effectful =
            mkPkg "time-effectful" ./lib/typed-process-dynamic-effectful
              {
                effectful-utils = ./lib/effectful-utils;
              };
          packages.unix-compat-effectful = mkPkg "unix-compat-effectful" ./lib/unix-compat-effectful {
            effectful-utils = ./lib/effectful-utils;
          };
          packages.unix-effectful = mkPkg "unix-effectful" ./lib/unix-effectful {
            effectful-utils = ./lib/effectful-utils;
          };

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            # Restore this once hlint is working again.
            #buildInputs = (nix-hs-utils.mkBuildTools pkgsCompiler) ++ (nix-hs-utils.mkDevTools pkgsCompiler);
            buildInputs = [
              (hlib.dontCheck compiler.cabal-fmt)
              (hlib.dontCheck compiler.haskell-language-server)
              pkgs.nixfmt-rfc-style
            ]
            ++ (nix-hs-utils.mkBuildTools pkgsCompiler);
          };

          apps = {
            format = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.format (pkgsCompiler // pkgsMkDrv))
                (nix-hs-utils.format-yaml pkgsMkDrv)
              ];
            };

            lint = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.lint (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.lint-yaml pkgsMkDrv)
              ];
            };

            lint-refactor = nix-hs-utils.lint-refactor pkgsCompiler;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
