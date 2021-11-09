{ }:

let
  sources = import nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  darwinInputs = with pkgs;
    lib.optionals stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
      AppKit
      CoreFoundation
      CoreServices
      CoreVideo
      Foundation
      Metal
      Security
    ]);

  linuxInputs = with pkgs;
    lib.optionals stdenv.isLinux [
      valgrind
      vulkan-headers
      vulkan-loader
      vulkan-tools
      vulkan-validation-layers
      xorg.libX11
      xorg.libXcursor
      xorg.libXrandr
      xorg.libXi
      xorg.libxcb
      alsa-lib
    ];

  llvmPkgs = pkgs.llvmPackages_12;

  zig = import ./nix/zig.nix { inherit pkgs; };
  debugir = import ./nix/debugir.nix { inherit pkgs; };

  inputs = with pkgs; [
    # build libraries
    rustc
    cargo
    clippy
    rustfmt
    cmake
    git
    python3
    llvmPkgs.llvm.dev
    llvmPkgs.clang
    libxkbcommon
    pkg-config
    zig

    # lib deps
    libffi
    libxml2
    ncurses
    zlib
    libiconv

    # faster builds - see https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
    llvmPkgs.lld
    debugir

    # meta-tools
    # note: niv manages its own nixpkgs so it doesn't need pkgs.callPackage. Do
    # `cachix use niv` to get cached builds!
    (import sources.niv { }).niv

    # tools for development environment
    less
  ];
in pkgs.mkShell {
  buildInputs = inputs ++ darwinInputs ++ linuxInputs;

  # Additional Env vars
  LLVM_SYS_120_PREFIX = "${llvmPkgs.llvm.dev}";
  NIX_GLIBC_PATH =
    if pkgs.stdenv.isLinux then "${pkgs.glibc_multi.out}/lib" else "";
  LD_LIBRARY_PATH = with pkgs;
    lib.makeLibraryPath
    ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ] ++ linuxInputs);

  COREAUDIO_SDK_PATH = if pkgs.stdenv.isDarwin then
  # The coreaudio-sys crate is configured to look for things in whatever the
  # output of `xcrun --sdk macosx --show-sdk-path` is. However, this does not
  # always contain the right frameworks, and it uses system versions instead of
  # what we control via Nix. Instead of having to run a lot of extra scripts
  # to set our systems up to build, we can just create a SDK directory with
  # the same layout as the `MacOSX{version}.sdk` that XCode produces.
    pkgs.symlinkJoin {
      name = "sdk";
      paths = with pkgs.darwin.apple_sdk.frameworks; [
        AudioToolbox
        AudioUnit
        CoreAudio
        CoreFoundation
        CoreMIDI
        OpenAL
      ];
      postBuild = ''
        mkdir $out/System
        mv $out/Library $out/System
      '';
    }
  else
  # TODO: I'm not 100% confident that this being blank won't cause issues for
  # Nix-on-Linux development. It may be sufficient to use the pkgs.symlinkJoin
  # above regardless of system! That'd set us up for cross-compilation as well.
    "";


}
