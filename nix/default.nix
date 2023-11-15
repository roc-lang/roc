{ pkgs }:
let
  rustVersion = (pkgs.rust-bin.fromRustupToolchainFile ../rust-toolchain.toml);
  rustPlatform = pkgs.makeRustPlatform {
    cargo = rustVersion;
    rustc = rustVersion;
  };

  # this will allow our callPackage to reference our own packages defined below
  # mainly helps with passing compile-deps and rustPlatform to builder automatically
  callPackage = pkgs.lib.callPackageWith (pkgs // packages);

  packages = {
    inherit rustPlatform;
    compile-deps = callPackage ./compile-deps.nix { };
    rust-shell =
      (rustVersion.override { extensions = [ "rust-src" "rust-analyzer" ]; });

    roc-cli = callPackage ./builder.nix { }; # TODO: this builds the language server as `roc_ls`
    roc-lang-server = callPackage ./builder.nix { subPackage = "lang_srv"; };
  };

in
packages
