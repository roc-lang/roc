{ pkgs }:

let
  version = "0.9.1";

  osName = if pkgs.stdenv.isDarwin then "macos" else "linux";

  splitSystem = builtins.split "-" builtins.currentSystem;
  arch = builtins.elemAt splitSystem 0;
  isAarch64 = arch == "aarch64";

  archiveName = "zig-${osName}-${arch}-${version}";

  # If your system is not aarch64, we assume it's x86_64
  sha256 = if pkgs.stdenv.isDarwin then
    if isAarch64 then
      "8c473082b4f0f819f1da05de2dbd0c1e891dff7d85d2c12b6ee876887d438287" # macos-aarch64
    else
      "2d94984972d67292b55c1eb1c00de46580e9916575d083003546e9a01166754c" # macos-x86_64
  else if isAarch64 then
    "5d99a39cded1870a3fa95d4de4ce68ac2610cca440336cfd252ffdddc2b90e66" # linux-aarch64
  else
    "be8da632c1d3273f766b69244d80669fe4f5e27798654681d77c992f17c237d7"; # linux-x86_64
in pkgs.stdenv.mkDerivation {
  pname = "zig";
  version = version;
  src = pkgs.fetchurl {
    inherit sha256;
    name = "${archiveName}.tar.xz";
    url = "https://ziglang.org/download/${version}/${archiveName}.tar.xz";
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
