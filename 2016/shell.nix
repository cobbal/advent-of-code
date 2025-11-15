let nixpkgs = import <nixos> {}; in
with nixpkgs;
stdenv.mkDerivation {
  name = "adv-code-2016";
  buildInputs = [
    just
    nodejs
    wabt
    wasmtime
  ];
}
