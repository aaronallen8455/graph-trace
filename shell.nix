{ system ? builtins.currentSystem }:

with import <nixpkgs> { inherit system; };

mkShell {
  buildInputs = [
    haskell.compiler.ghc901
    cabal-install
  ];
}
