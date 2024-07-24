{ pkgs }:
let
  llvmPkgs = pkgs.llvmPackages_18;
  llvmVersion = builtins.splitVersion llvmPkgs.release_version;
  llvmMajorMinorStr = builtins.elemAt llvmVersion 0 + builtins.elemAt llvmVersion 1;
  # nix does not store libs in /usr/lib or /lib
  glibcPath =
    if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";
  libGccSPath =
    if pkgs.stdenv.isLinux then "${pkgs.stdenv.cc.cc.lib}/lib" else "";
in
{
  inherit llvmPkgs llvmVersion llvmMajorMinorStr glibcPath libGccSPath;

  darwinInputs = with pkgs;
    lib.optionals stdenv.isDarwin
      (with pkgs.darwin.apple_sdk.frameworks; [
        AppKit
        CoreFoundation
        CoreServices
        Foundation
        Security
      ]);
}
