{
  description = "A Collection of Effectful Effects";

  # nix
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  # haskell
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { algebra-simple
    , bounds
    , flake-parts
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit self; } {
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
          ghc-version = "ghc925";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              algebra-simple = final.callCabal2nix "algebra-simple" algebra-simple { };
              bounds = final.callCabal2nix "bounds" bounds { };
              # These tests seems to hang, see:
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
              hedgehog = prev.hedgehog_1_2;
              tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
            };
          };
          hsOverlay =
            (compiler.extend (hlib.compose.packageSourceOverrides {
              effectful-callstack = ./effectful-callstack;
              effectful-fs = ./effectful-fs;
              effectful-ioref = ./effectful-ioref;
              effectful-logger = ./effectful-logger;
              effectful-logger-namespace = ./effectful-logger-namespace;
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
            p.effectful-logger-namespace
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
        in
        {
          packages.effectful-callstack = mkPkg "effectful-callstack" ./effectful-callstack { };
          packages.effectful-fs = mkPkgsCallStack "effectful-fs" ./effectful-fs;
          packages.effectful-ioref = mkPkgsCallStack "effectful-ioref" ./effectful-ioref;
          packages.effectful-logger = mkPkgsCallStack "effectful-logger" ./effectful-logger;
          packages.effectful-logger-namespace =
            mkPkg "effectful-logger-namespace" ./effectful-logger-namespace {
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
            buildInputs = (buildTools compiler) ++ (devTools compiler);
          };
          devShells.ci = hsOverlay.shellFor {
            inherit packages;
            withHoogle = false;
            buildInputs = buildTools compiler;
          };
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
