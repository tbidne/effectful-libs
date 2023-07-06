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
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            ghcid
            haskell-language-server
          ];
          ghc-version = "ghc945";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              apply-refact = prev.apply-refact_0_11_0_0;
              # These tests seems to hang, see:
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
              hedgehog = prev.hedgehog_1_2;
              tasty-hedgehog = prev.tasty-hedgehog_1_4_0_1;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
            ];
          };
          hsOverlay =
            (compiler.extend (hlib.compose.packageSourceOverrides {
              effectful-callstack = ./effectful-callstack;
              effectful-fs = ./effectful-fs;
              effectful-ioref = ./effectful-ioref;
              effectful-logger = ./effectful-logger;
              effectful-logger-ns = ./effectful-logger-ns;
              effectful-stm = ./effectful-stm;
              effectful-terminal = ./effectful-terminal;
              effectful-thread = ./effectful-thread;
              effectful-time = ./effectful-time;
            }));
          packages = p: [
            p.effectful-callstack
            p.effectful-fs
            p.effectful-ioref
            p.effectful-logger
            p.effectful-logger-ns
            p.effectful-stm
            p.effectful-terminal
            p.effectful-thread
            p.effectful-time
          ];

          mkPkg = name: root: source-overrides: compiler.developPackage {
            inherit name root source-overrides;
            returnShellEnv = false;
          };
          mkPkgsCallStack = name: root: mkPkg name root {
            effectful-callstack = ./effectful-callstack;
          };

          hsDirs = "effectful-*";
        in
        {
          packages.effectful-callstack = mkPkg "effectful-callstack" ./effectful-callstack { };
          packages.effectful-fs = mkPkgsCallStack "effectful-fs" ./effectful-fs;
          packages.effectful-ioref = mkPkgsCallStack "effectful-ioref" ./effectful-ioref;
          packages.effectful-logger = mkPkgsCallStack "effectful-logger" ./effectful-logger;
          packages.effectful-logger-ns =
            mkPkg "effectful-logger-ns" ./effectful-logger-ns {
              effectful-callstack = ./effectful-callstack;
              effectful-logger = ./effectful-logger;
              effectful-time = ./effectful-time;
            };
          packages.effectful-stm = mkPkgsCallStack "effectful-stm" ./effectful-stm;
          packages.effectful-terminal = mkPkgsCallStack "effectful-terminal" ./effectful-terminal;
          packages.effectful-thread = mkPkgsCallStack "effectful-thread" ./effectful-thread;
          packages.effectful-time = mkPkgsCallStack "effectful-thread" ./effectful-time;

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs =
              (nix-hs-utils.mkBuildTools pkgs compiler)
              ++ (nix-hs-utils.mkDevTools { inherit pkgs compiler; });
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
