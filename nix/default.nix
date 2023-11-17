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

    # all rust crates in workspace.members of Cargo.toml
    roc-full = callPackage ./builder.nix { };
    roc-lang-server = callPackage ./builder.nix { subPackage = "lang_srv"; };
    # only the CLI crate = executable provided in nightly releases
    roc-cli = callPackage ./builder.nix { subPackage = "cli"; };
  };

in
packages
