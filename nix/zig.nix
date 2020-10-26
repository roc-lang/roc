{ pkgs, isMacOS }:

if isMacOS
  then
    # As of 2020-10-25, building Zig from source on MacOS fails
    # so we just download the binary from their release page
    let
      version = "0.6.0";
      archiveName = "zig-macos-x86_64-${version}+91a1c20e7";
    in
    pkgs.stdenv.mkDerivation {
      pname = "zig";
      version = version;
      buildInputs = [ pkgs.gzip ];
      src = pkgs.fetchurl {
        name = "${archiveName}.tar.xz";
        url = "https://ziglang.org/builds/${archiveName}.tar.xz";
        sha256 = "0svwlk76w171ikr8wjzchm4svd4hvna8idv84pi7ar2fr4i8bkic";
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
