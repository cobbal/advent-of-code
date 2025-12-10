let nixpkgs = import <nixos> {}; in
with nixpkgs;
stdenv.mkDerivation {
  name = "adv-code-2025";
  buildInputs = [
    just
    bun
    deno
    nodejs
  ];
}
