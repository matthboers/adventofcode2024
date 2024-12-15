let
  pkgs = import <nixpkgs> { };
  inherit (pkgs.lib.trivial) pipe;

  appendTools = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
  [
    cabal-install
    ghcid
    haskell-language-server
  ]);

  appendLibraries = drv: pkgs.haskell.lib.addExtraLibraries drv (with pkgs;
  [
  ]);
  
in
pkgs.haskellPackages.developPackage {
  root = ./.;

  # Construction stolen from
  # https://github.com/sireto/govtool/blob/b5384e5e48fc336c7923e0a38709315e7d19df96/govtool/backend/default.nix
  modifier = drv: pipe drv [ appendTools appendLibraries ];

  overrides = final: _prev: {
  };
}