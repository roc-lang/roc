{ pkgs, isMacOS }:

# We require at least specific commit of Zig after the latest tagged
# release (0.6.0), so we just download the binaries for that commit

let
  version = "0.6.0+0088efc4b";
  osName =
    if isMacOS
      then "macos"
      else "linux";
  archiveName = "zig-${osName}-x86_64-${version}";
  sha256 = 
    if isMacOS
      then "665c1a7f472cfc5e0715f0ddf6ff8409fb749ac91cbbae68c443b4a37ebd058e"
      else "bab70ae3bd0af538022bc3ef50d8f34fa8dceac39ba7d9e5d528eee7e6d5a1cf";
in
pkgs.stdenv.mkDerivation {
  pname = "zig";
  version = version;
  buildInputs = [ pkgs.gzip ];
  src = pkgs.fetchurl {
    inherit sha256;
    name = "${archiveName}.tar.xz";
    url = "https://ziglang.org/builds/${archiveName}.tar.xz";
  };
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    tar -xf $src
    cp ${archiveName}/zig $out/zig
    cp -r ${archiveName}/lib $out/lib
    ln -s "$out/zig" "$out/bin/zig"
    chmod +x $out/bin/zig
  '';
}
