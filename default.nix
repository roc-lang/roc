{ }:

let
  sources = import nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  rustPlatform = pkgs.rustPlatform;
  llvmPkgs = pkgs.llvmPackages_12;
in
rustPlatform.buildRustPackage {
  pname = "roc";
  version = "0.1.0";

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  cargoSha256 = "11r8drmps92i7l3bg3zabds9n5rim9f4fjsgr86nhqhcsivi80y4";

  LLVM_SYS_120_PREFIX = "${llvmPkgs.llvm.dev}";

  # required for zig builds
  XDG_CACHE_HOME = "/build/xdgcache";

  nativeBuildInputs = (with pkgs; [
    cmake
    git
    pkg-config
    python3
    llvmPkgs.lld
    llvmPkgs.clang
    llvmPkgs.llvm.dev

    (import ./nix/zig.nix { inherit pkgs; })
  ]);

  buildInputs = (with pkgs; [
    (import ./nix/debugir.nix { inherit pkgs; })
    libffi
    libiconv
    libxkbcommon
    libxml2
    ncurses
    zlib
  ]
  ++ lib.optionals pkgs.stdenv.isLinux [
      alsa-lib
      glibc_multi
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
