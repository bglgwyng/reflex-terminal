{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        let
          inherit (pkgs) lib;
        in
        {
          haskellProjects.default = {
            projectRoot = lib.fileset.toSource {
              root = ./.;
              fileset = lib.fileset.unions [
                ./reflex-terminal.cabal
                ./exe
                ./lib
                ./CHANGELOG.md
                ./LICENSE
              ];
            };
            devShell = {
              hlsCheck.enable = false;
              tools = hpkgs: {
                inherit (hpkgs) cabal-gild doctest fourmolu;
              };
            };
          };
        };
    };
}
