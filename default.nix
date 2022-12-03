{ rev ? (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked.rev
, nixpkgsSource ? builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked.narHash;
  }
, pkgs ? import nixpkgsSource { }
,
}:
# we only this file to release a nix package, use flake.nix for development
let
  rustPlatform = pkgs.rustPlatform;
  llvmPkgs = pkgs.llvmPackages_13;
  # nix does not store libs in /usr/lib or /lib
  nixGlibcPath = if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";
in
rustPlatform.buildRustPackage {
  pname = "roc";
  version = "0.0.1";

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "confy-0.5.0" = "sha256-BVTczVbURL1Id/k/5ArlDQTZxLuI3XxQl7BdIx230U4=";
      "criterion-0.3.5" = "sha256-7REd3phV6PBzqWwKF8hwttw4FTq2tKGxxAAJDpLC50A=";
      "inkwell-0.1.0" = "sha256-vhFbiGP8FxMoLkapctPYmkfV+tO6x4KkoC3FwCHcy/4=";
      "plotters-0.3.1" = "sha256-noy/RSjoEPZZbOJTZw1yxGcX5S+2q/7mxnUrzDyxOFw=";
      "rustyline-9.1.1" = "sha256-aqQqz6nSp+Qn44gm3jXmmQUO6/fYTx7iLph2tbA24Bs=";
      "wasm3-0.5.0" = "sha256-RwAZ4hjdDk/MvY1S1ymduqn+ubNPQdkMcXJHU2Zxq8k=";
    };
  };

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
  postInstall =
    if pkgs.stdenv.isLinux then ''
      wrapProgram $out/bin/roc --set NIX_GLIBC_PATH ${nixGlibcPath} --prefix PATH : ${
        pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]
      }
    '' else ''
      wrapProgram $out/bin/roc --prefix PATH : ${
        pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]
      }
    '';
}
