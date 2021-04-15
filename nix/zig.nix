{ pkgs, isMacOS, isAarch64 }:

if isMacOS then
  let
    version = "0.7.1";
    osName =
      if isMacOS
        then "macos"
        else "linux";
    archName =
      if isAarch64
        then "aarch64"
        else "x86_64";
    archiveName = "zig-${osName}-${archName}-${version}";
    sha256 = "845cb17562978af0cf67e3993f4e33330525eaf01ead9386df9105111e3bc519";
  in
  pkgs.stdenv.mkDerivation {
    pname = "zig";
    version = version;
    buildInputs = [ pkgs.gzip ];
    src = pkgs.fetchurl {
      inherit sha256;
      name = "${archiveName}.tar.xz";
      url = "https://ziglang.org/download/${version}/${archiveName}.tar.xz";
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
else
  pkgs.zig
