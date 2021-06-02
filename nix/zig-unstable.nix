{ pkgs }:

let
  # system helpers
  splitSystem = builtins.split "-" builtins.currentSystem;
  arch = builtins.elemAt splitSystem 0;
  isAarch64 = arch == "aarch64";
  setForSystem = { darwin, darwinAarch64, linux, linuxAarch64 }:
    if pkgs.stdenv.isDarwin
    then (
      if isAarch64
      then darwinAarch64
      else darwin
    )
    else (
      if isAarch64
      then linuxAarch64
      else linux
    );

  version = "0.8.0-dev.2711+11ae6c42c";
  osName =
    if pkgs.stdenv.isDarwin
    then "macos" else "linux";
  archiveName = "zig-${osName}-${arch}-${version}";
  sha256 = setForSystem {
    darwin = "bf2a4cd1516d202cfbbcaa7b1308d36aa21a9f9284b39297e70f98c003f479e3";
    darwinAarch64 = "6bc35c3b40b853cd351c890c94c4a6043f5ca492ff6d704bdb1544fe1fe54d9a";
    linux = "b443cc2259fe7712ffc954745266e3ec846e27854713d817bcec35fefd655a8c";
    linuxAarch64 = "229830e6dc92f641a1106af3a8ee96fdef379ffd3a3d7db7ed62d2b46bd8ed45";
  };
in
pkgs.stdenv.mkDerivation {
  pname = "zig-unstable";
  version = version;
  src = pkgs.fetchurl {
    inherit sha256;
    name = "${archiveName}.tar.xz";
    url = "https://ziglang.org/builds/${archiveName}.tar.xz";
  };
  phases = [ "unpackPhase" ];
  unpackPhase = ''
    mkdir -p $out/bin
    tar -xf $src
    cp ${archiveName}/zig $out/zig
    cp -r ${archiveName}/lib $out/lib
    ln -s "$out/zig" "$out/bin/zig"
    chmod +x $out/bin/zig
  '';
}
