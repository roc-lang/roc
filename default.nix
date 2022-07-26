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
in
rustPlatform.buildRustPackage {
  pname = "roc";
  version = "0.0.1";

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  cargoSha256 = "sha256-oSi9UIom3YowgfR1U4c6her3SsfeV//t6Dy3eOQaW9o=";

  LLVM_SYS_130_PREFIX = "${llvmPkgs.llvm.dev}";

  # for cli bindgen "No such file or directory"
  preBuild = ''
    # From: https://github.com/NixOS/nixpkgs/blob/1fab95f5190d087e66a3502481e34e15d62090aa/pkgs/applications/networking/browsers/firefox/common.nix#L247-L253
    # Set C flags for Rust's bindgen program. Unlike ordinary C
    # compilation, bindgen does not invoke $CC directly. Instead it
    # uses LLVM's libclang. To make sure all necessary flags are
    # included we need to look in a few places.
    export BINDGEN_EXTRA_CLANG_ARGS="$(< ${pkgs.stdenv.cc}/nix-support/libc-crt1-cflags) \
      $(< ${pkgs.stdenv.cc}/nix-support/libc-cflags) \
      $(< ${pkgs.stdenv.cc}/nix-support/cc-cflags) \
      $(< ${pkgs.stdenv.cc}/nix-support/libcxx-cxxflags) \
      ${pkgs.lib.optionalString pkgs.stdenv.cc.isClang "-idirafter ${pkgs.stdenv.cc.cc}/lib/clang/${pkgs.lib.getVersion pkgs.stdenv.cc.cc}/include"} \
      ${pkgs.lib.optionalString pkgs.stdenv.cc.isGNU "-isystem ${pkgs.stdenv.cc.cc}/include/c++/${pkgs.lib.getVersion pkgs.stdenv.cc.cc} -isystem ${pkgs.stdenv.cc.cc}/include/c++/${pkgs.lib.getVersion pkgs.stdenv.cc.cc}/${pkgs.stdenv.hostPlatform.config}"}
    "
  '';

  # required for zig
  XDG_CACHE_HOME = "/build/xdgcache";
  # nix does not store libs in /usr/lib or /lib
  NIX_GLIBC_PATH = if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";
  # want to see backtrace in case of failure
  RUST_BACKTRACE = 1;

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

}
