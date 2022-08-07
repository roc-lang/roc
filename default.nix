{ }:
# we only this file to release a nix package, use flake.nix for development
let
  rev = "f6342b8b9e7a4177c7e775cdbf38e1c1b43e7ab3"; # nixpkgs master
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/tarball/${rev}";
    sha256 = "JTiKsBT1BwMbtSUsvtSl8ffkiirby8FaujJVGV766Q8=";
  };
  pkgs = import nixpkgs { };
  rustPlatform = pkgs.rustPlatform;
  llvmPkgs = pkgs.llvmPackages_13;
  # nix does not store libs in /usr/lib or /lib
  nixGlibcPath = if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";
in
rustPlatform.buildRustPackage {
  pname = "roc";
  version = "0.0.1";

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  cargoSha256 = "sha256-Pd84GGtW1ecrP03uiCVcybIUtWCSDGfLl+fbbdmFyiE=";

  LLVM_SYS_130_PREFIX = "${llvmPkgs.llvm.dev}";

  # required for zig
  XDG_CACHE_HOME = "xdg_cache"; # prevents zig AccessDenied error github.com/ziglang/zig/issues/6810
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

  buildInputs = (with pkgs; [
    libffi
    libiconv
    libxkbcommon
    libxml2
    ncurses
    zlib
    cargo
    makeWrapper # necessary for postBuild wrapProgram
  ]
  ++ lib.optionals pkgs.stdenv.isLinux [
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
  ]
  ++ lib.optionals pkgs.stdenv.isDarwin [
      AppKit
      CoreFoundation
      CoreServices
      CoreVideo
      Foundation
      Metal
      Security
  ]);

  # cp: to copy str.zig,list.zig...
  # wrapProgram pkgs.stdenv.cc: to make ld available for compiler/build/src/link.rs
  postInstall = if pkgs.stdenv.isLinux then ''
    cp -r target/x86_64-unknown-linux-gnu/release/lib/. $out/lib
    wrapProgram $out/bin/roc --set NIX_GLIBC_PATH ${nixGlibcPath} --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]}
  '' else ''
    cp -r target/aarch64-apple-darwin/release/lib/. $out/lib
    wrapProgram $out/bin/roc --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.stdenv.cc ]}
  '';
}
