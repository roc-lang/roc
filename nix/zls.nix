{ pkgs, zig }:

# As of 2020-10-25, building zls is not available on Nix. For some reason,
# this hangs on `zig build`. I'll try to figure it our later.

let
  rev = "e8c20351d85da8eb4bf22480045b994007284d69";
in
pkgs.stdenv.mkDerivation {
  pname = "zig-language-server";
  version = rev;
  src = pkgs.fetchgit {
    inherit rev;
    fetchSubmodules = true;
    url = "https://github.com/zigtools/zls.git";
    sha256 = "06g8gml1g0fmvcfysy93bd1hb64vjd2v12x3kgxz58kmk5z0168y";
  };
  phases = [ "buildPhase" "installPhase" ];
  buildInputs = [ zig ];
  buildPhase = ''
    zig build
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp ./zig-cache/bin/zls $out/bin/zls
    chmod +x $out/bin/zls
  '';
}
