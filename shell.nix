{ system ? builtins.currentSystem }:

with import <nixpkgs> { inherit system; };

mkShell {
  buildInputs = [
    haskell.compiler.ghc921
    cabal-install
    graphviz
  ];
}
