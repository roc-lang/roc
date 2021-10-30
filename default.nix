{ }:

let
  nixpkgsUnstableRef = "47b36ad103aeff17f9be6fb7b4847d63d53f227a";
  nixpkgsUnstableSha = "109shladi2pj27mmna2g53m59m110pbczhnskrn3knbgpdmd78xz";
  nixpkgsUnstableUrl = "https://github.com/nixos/nixpkgs/archive/${nixpkgsUnstableRef}.tar.gz";
  nixpkgsUnstableTar = builtins.fetchTarball {
    url = nixpkgsUnstableUrl;
    sha256 = nixpkgsUnstableSha;
  };

  pkgs = import "${nixpkgsUnstableTar}" {};
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
