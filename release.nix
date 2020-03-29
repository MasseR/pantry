with (builtins.fromJSON (builtins.readFile ./nixpkgs.ghc.json));


let
  masse-prelude-src = pkgs.fetchFromGitHub {
    owner = "MasseR";
    repo = "masse-prelude";
    rev = "52fdf7cd5b70e399d6279083757b1ea0806aa68a";
    sha256 = "0pzp97899r2wxsbz5s8mn6h1hyjbq786aq83i4dcmgqv68g8v0ql";
  };
  overrides = pkgs: with pkgs.haskell.lib; {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc865 = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib; rec {
            masse-prelude = super.callPackage masse-prelude-src {};
            groceries-server = super.callPackage ./groceries-server {};
          };
        };
      };
    };
  };
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.packageOverrides = overrides; config.allowUnfree = true; };

in

rec {
  inherit (pkgs.haskell.packages.ghc865) groceries-server;
  shell = pkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with pkgs; [
      entr
      ghcid
      haskell.packages.ghc865.cabal-install
      haskell.packages.ghc865.hasktags
      stylish-haskell
      hlint
      binutils
      (haskell.packages.ghc865.ghcWithHoogle
        (_: builtins.concatMap
          (p: p.buildInputs ++ p.propagatedBuildInputs)
          [ groceries-server ]))
    ];
  };
}
