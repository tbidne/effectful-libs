{
  description = "A Collection of Effectful Effects";

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
  outputs =
    inputs@{ flake-parts
    , nix-hs-utils
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc962";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              file-io = final.callHackage "file-io" "0.1.0.1" { };
              hedgehog = prev.hedgehog_1_3;
              hlint = prev.hlint_3_6_1;
              ormolu = prev.ormolu_0_7_1_0;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
            ];
          };
          pkgsCompiler = { inherit pkgs compiler; };
          hsOverlay =
            (compiler.extend (hlib.compose.packageSourceOverrides {
              environment-effectful = ./lib/environment-effectful;
              exceptions-effectful = ./lib/exceptions-effectful;
              fs-effectful = ./lib/fs-effectful;
              ioref-effectful = ./lib/ioref-effectful;
              logger-effectful = ./lib/logger-effectful;
              logger-ns-effectful = ./lib/logger-ns-effectful;
              optparse-effectful = ./lib/optparse-effectful;
              stm-effectful = ./lib/stm-effectful;
              terminal-effectful = ./lib/terminal-effectful;
              concurrent-effectful = ./lib/concurrent-effectful;
              time-effectful = ./lib/time-effectful;
              typed-process-effectful = ./lib/typed-process-effectful;
              unix-compat-effectful = ./lib/unix-compat-effectful;
            }));
          packages = p: [
            p.environment-effectful
            p.exceptions-effectful
            p.fs-effectful
            p.ioref-effectful
            p.logger-effectful
            p.logger-ns-effectful
            p.optparse-effectful
            p.stm-effectful
            p.terminal-effectful
            p.concurrent-effectful
            p.time-effectful
            p.typed-process-effectful
            p.unix-compat-effectful
          ];

          mkPkg = name: root: source-overrides: compiler.developPackage {
            inherit name root source-overrides;
            returnShellEnv = false;
          };
          mkPkgsExceptions = name: root: mkPkg name root {
            exceptions-effectful = ./lib/exceptions-effectful;
          };

          hsDirs = "lib/*-effectful";
        in
        {
          packages.concurrent-effectful = mkPkgsExceptions "concurrent-effectful" ./lib/concurrent-effectful;
          packages.environment-effectful = mkPkg "environment-effectful" ./lib/environment-effectful { };
          packages.exceptions-effectful = mkPkg "exceptions-effectful" ./lib/exceptions-effectful { };
          packages.fs-effectful =
            mkPkg "fs-effectful" ./lib/fs-effectful {
              exceptions-effectful = ./lib/exceptions-effectful;
              ioref-effectful = ./lib/ioref-effectful;
            };
          packages.ioref-effectful = mkPkgsExceptions "ioref-effectful" ./lib/ioref-effectful;
          packages.logger-effectful = mkPkgsExceptions "logger-effectful" ./lib/logger-effectful;
          packages.logger-ns-effectful =
            mkPkg "logger-ns-effectful" ./lib/logger-ns-effectful {
              exceptions-effectful = ./lib/exceptions-effectful;
              logger-effectful = ./lib/logger-effectful;
              time-effectful = ./lib/time-effectful;
            };
          packages.optparse-effectful =
            mkPkg "optparse-effectful" ./lib/optparse-effectful {
              exceptions-effectful = ./lib/exceptions-effectful;
              fs-effectful = ./lib/fs-effectful;
              ioref-effectful = ./lib/ioref-effectful;
            };
          packages.stm-effectful = mkPkgsExceptions "stm-effectful" ./lib/stm-effectful;
          packages.terminal-effectful = mkPkgsExceptions "terminal-effectful" ./lib/terminal-effectful;
          packages.time-effectful = mkPkgsExceptions "time-effectful" ./lib/time-effectful;
          packages.typed-process-effectful =
            mkPkg "typed-process-effectful" ./lib/typed-process-effectful {
              exceptions-effectful = ./lib/exceptions-effectful;
            };
          packages.unix-compat-effectful =
            mkPkgsExceptions "unix-compat-effectful" ./lib/unix-compat-effectful;

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs =
              (nix-hs-utils.mkBuildTools pkgsCompiler)
              ++ (nix-hs-utils.mkDevTools pkgsCompiler);
          };

          apps = {
            format = nix-hs-utils.format {
              inherit compiler hsDirs pkgs;
            };
            lint = nix-hs-utils.lint {
              inherit compiler hsDirs pkgs;
            };
            lint-refactor = nix-hs-utils.lint-refactor {
              inherit compiler hsDirs pkgs;
            };
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
