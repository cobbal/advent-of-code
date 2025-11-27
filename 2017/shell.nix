let nixpkgs = import <nixos> {}; in
with nixpkgs;
stdenv.mkDerivation {
  name = "adv-code-2017";
  buildInputs = [
    binaryen
    guile
    just
    nodejs
    rlwrap
    wabt
    (callPackage ./wac-cli.nix {})
    wkg
    wasm-tools
    wasmtime
  ];
}
