{ pkgs }:

let
  version = "0.8.0";

  osName = if pkgs.stdenv.isDarwin then "macos" else "linux";

  splitSystem = builtins.split "-" builtins.currentSystem;
  arch = builtins.elemAt splitSystem 0;
  isAarch64 = arch == "aarch64";

  archiveName = "zig-${osName}-${arch}-${version}";

  # If your system is not aarch64, we assume it's x86_64
  sha256 = if pkgs.stdenv.isDarwin then
    if isAarch64 then
      "b32d13f66d0e1ff740b3326d66a469ee6baddbd7211fa111c066d3bd57683111"
    else
      "279f9360b5cb23103f0395dc4d3d0d30626e699b1b4be55e98fd985b62bc6fbe"
  else if isAarch64 then
    "ee204ca2c2037952cf3f8b10c609373a08a291efa4af7b3c73be0f2b27720470"
  else
    "502625d3da3ae595c5f44a809a87714320b7a40e6dff4a895b5fa7df3391d01e";
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
