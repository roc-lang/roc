{ pkgs }:
let
  rustVersion = (pkgs.rust-bin.fromRustupToolchainFile ../rust-toolchain.toml);
  rustPlatform = pkgs.makeRustPlatform {
    cargo = rustVersion;
    rustc = rustVersion;
  };
  compile-deps = pkgs.callPackage ./compile-deps.nix { };
in
{
  inherit rustPlatform compile-deps;
  rust-shell =
    (rustVersion.override { extensions = [ "rust-src" "rust-analyzer" ]; });
  roc-cli = pkgs.callPackage ./roc-cli.nix { inherit compile-deps rustPlatform; };
  roc-lang-server = {};
}
