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
      # llvm-tools-preview for code coverage with cargo-llvm-cov
      (rustVersion.override { extensions = [ "rust-src" "rust-analyzer" "llvm-tools-preview"]; });


    # contains all rust crates in workspace.members of Cargo.toml
    roc-full = (callPackage ./builder.nix { }).roc-release;
    roc-full-debug = (callPackage ./builder.nix { }).roc-debug;

    roc-lang-server = (callPackage ./builder.nix { subPackage = "language_server"; }).roc-release;
    roc-lang-server-debug = (callPackage ./builder.nix { subPackage = "language_server"; }).roc-debug;

    # only the CLI crate = executable provided in nightly releases
    roc-cli = (callPackage ./builder.nix { subPackage = "cli"; }).roc-release;
    roc-cli-debug = (callPackage ./builder.nix { subPackage = "cli"; }).roc-debug;
  };

in
packages
