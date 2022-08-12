{ rev ? "541a3ca27c9a8220b46f4feb7dd8e94336a77f42", # nixpkgs master
nixpkgsSource ? builtins.fetchTarball {
  url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
  sha256 = "sha256:1mxv0zigm98pawf05kd4s8ipvk1pvvdsn1yh978c5an97kz0ck5w";
}, pkgs ? import nixpkgsSource { }
, cargoSha256 ? "sha256-treL2sWPcZ1NBwdab3FOb2FI2wT/Vt9tD4XRfJ8rYWA=", }:
# we only this file to release a nix package, use flake.nix for development
let
  rustPlatform = pkgs.rustPlatform;
  llvmPkgs = pkgs.llvmPackages_13;
  # nix does not store libs in /usr/lib or /lib
  nixGlibcPath = if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";
in rustPlatform.buildRustPackage {
  pname = "roc";
  version = "0.0.1";

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  inherit cargoSha256;

  LLVM_SYS_130_PREFIX = "${llvmPkgs.llvm.dev}";

  # required for zig
  XDG_CACHE_HOME =
    "xdg_cache"; # prevents zig AccessDenied error github.com/ziglang/zig/issues/6810
  # want to see backtrace in case of failure
  RUST_BACKTRACE = 1;

  # skip running rust tests, problems:
  # building of example platforms requires network: Could not resolve host
  # zig AccessDenied error github.com/ziglang/zig/issues/6810
  # Once instance has previously been poisoned ??
  doCheck = false;

  nativeBuildInputs = (with pkgs; [
    cmake
    git
    pkg-config
    python3
    llvmPkgs.clang
    llvmPkgs.llvm.dev
    zig
    rust-bindgen
  ]);

  buildInputs = (with pkgs;
    [
      libffi
      libiconv
      libxkbcommon
      libxml2
      ncurses
      zlib
      cargo
      makeWrapper # necessary for postBuild wrapProgram
    ] ++ lib.optionals pkgs.stdenv.isLinux [
      alsa-lib
      valgrind
      vulkan-headers
      vulkan-loader
      vulkan-tools
      vulkan-validation-layers
      xorg.libX11
      xorg.libXcursor
      xorg.libXi
      xorg.libXrandr
      xorg.libxcb
    ] ++ lib.optionals pkgs.stdenv.isDarwin [
      pkgs.darwin.apple_sdk.frameworks.AppKit
      pkgs.darwin.apple_sdk.frameworks.CoreFoundation
      pkgs.darwin.apple_sdk.frameworks.CoreServices
      pkgs.darwin.apple_sdk.frameworks.CoreVideo
      pkgs.darwin.apple_sdk.frameworks.Foundation
      pkgs.darwin.apple_sdk.frameworks.Metal
      pkgs.darwin.apple_sdk.frameworks.Security
    ]);

  # cp: to copy str.zig,list.zig...
  # wrapProgram pkgs.stdenv.cc: to make ld available for compiler/build/src/link.rs
  postInstall = if pkgs.stdenv.isLinux then ''
    cp -r target/x86_64-unknown-linux-gnu/release/lib/. $out/lib
    wrapProgram $out/bin/roc --set NIX_GLIBC_PATH ${nixGlibcPath} --prefix PATH : ${
      pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]
    }
  '' else ''
    cp -r target/aarch64-apple-darwin/release/lib/. $out/lib
    wrapProgram $out/bin/roc --prefix PATH : ${
      pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]
    }
  '';
}
