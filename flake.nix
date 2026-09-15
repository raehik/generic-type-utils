{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    ghc-typenats-bits.url   = "github:raehik/ghc-typenats-bits";
    ghc-typenats-bits.flake = false;
  };

  outputs = inputs:
  let
    defDevShell = compiler: {
      mkShellArgs.name = "${compiler}";
      hoogle = false;
      tools = _: {
        haskell-language-server = null;
        hlint = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc914-generic-type-utils;
        devShells.default = self'.devShells.ghc914;
        haskellProjects.ghc914 = {
          basePackages = pkgs.haskell.packages.ghc914;
          packages.ghc-typenats-bits.source = inputs.ghc-typenats-bits;
          devShell = {
            mkShellArgs.name = "ghc914";
            hoogle = false;
            tools = _: {
              haskell-language-server = null;
              hlint = null;
              ghcid = null;
              # 2026-09-14: not built by Nixpkgs & takes ages, just use prebuilt
              cabal-install = pkgs.cabal-install;
            };
          };
        };
        haskellProjects.ghc912 = {
          basePackages = pkgs.haskell.packages.ghc912;
          devShell = defDevShell "ghc912";
          packages.ghc-typenats-bits.source = inputs.ghc-typenats-bits;
        };
        haskellProjects.ghc910 = {
          basePackages = pkgs.haskell.packages.ghc910;
          devShell = defDevShell "ghc910";
          packages.ghc-typenats-bits.source = inputs.ghc-typenats-bits;
        };
      };
    };
}
