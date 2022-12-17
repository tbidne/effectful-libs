{
  description = "A Collection of Effectful Effects";

  # nix
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    { flake-parts
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
            }));
          packages = p: [
            p.effectful-callstack
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
