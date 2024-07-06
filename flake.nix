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
          ghc-version = "ghc982";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              { }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
              ];
          };
          pkgsCompiler = {
            inherit pkgs compiler;
          };
          hsOverlay = (
            compiler.extend (
              hlib.compose.packageSourceOverrides {
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
                unix-compat-effectful = ./lib/unix-compat-effectful;
              }
            )
          );
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
            p.unix-compat-effectful
          ];

          mkPkg =
            name: root: source-overrides:
            compiler.developPackage {
              inherit name root source-overrides;
              returnShellEnv = false;
            };
          mkPkgsExceptions =
            name: root: mkPkg name root { exceptions-effectful = ./lib/exceptions-effectful; };
          compilerPkgs = {
            inherit compiler pkgs;
          };
        in
        {
          packages.concurrent-effectful = mkPkgsExceptions "concurrent-effectful" ./lib/concurrent-effectful;
          packages.env-guard-effectful = mkPkg "env-guard-effectful" ./lib/env-guard-effectful { };
          packages.environment-effectful = mkPkg "environment-effectful" ./lib/environment-effectful { };
          packages.exceptions-effectful = mkPkg "exceptions-effectful" ./lib/exceptions-effectful { };
          packages.fs-effectful = mkPkg "fs-effectful" ./lib/fs-effectful {
            exceptions-effectful = ./lib/exceptions-effectful;
            ioref-effectful = ./lib/ioref-effectful;
            unix-compat-effectful = ./lib/unix-compat-effectful;
          };
          packages.ioref-effectful = mkPkgsExceptions "ioref-effectful" ./lib/ioref-effectful;
          packages.logger-effectful = mkPkgsExceptions "logger-effectful" ./lib/logger-effectful;
          packages.logger-ns-effectful = mkPkg "logger-ns-effectful" ./lib/logger-ns-effectful {
            exceptions-effectful = ./lib/exceptions-effectful;
            logger-effectful = ./lib/logger-effectful;
            time-effectful = ./lib/time-effectful;
          };
          packages.optparse-effectful = mkPkg "optparse-effectful" ./lib/optparse-effectful {
            exceptions-effectful = ./lib/exceptions-effectful;
            fs-effectful = ./lib/fs-effectful;
            ioref-effectful = ./lib/ioref-effectful;
            unix-compat-effectful = ./lib/unix-compat-effectful;
          };
          packages.stm-effectful = mkPkgsExceptions "stm-effectful" ./lib/stm-effectful;
          packages.terminal-effectful = mkPkgsExceptions "terminal-effectful" ./lib/terminal-effectful;
          packages.time-effectful = mkPkgsExceptions "time-effectful" ./lib/time-effectful;
          packages.unix-compat-effectful = mkPkgsExceptions "unix-compat-effectful" ./lib/unix-compat-effectful;

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs = (nix-hs-utils.mkBuildTools pkgsCompiler) ++ (nix-hs-utils.mkDevTools pkgsCompiler);
          };

          apps = {
            format = nix-hs-utils.format pkgsCompiler;
            lint = nix-hs-utils.lint pkgsCompiler;
            lintRefactor = nix-hs-utils.lintRefactor pkgsCompiler;
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
