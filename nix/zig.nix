{ pkgs }:

let
  version = "0.8.1";

  osName = if pkgs.stdenv.isDarwin then "macos" else "linux";

  splitSystem = builtins.split "-" builtins.currentSystem;
  arch = builtins.elemAt splitSystem 0;
  isAarch64 = arch == "aarch64";

  archiveName = "zig-${osName}-${arch}-${version}";

  # If your system is not aarch64, we assume it's x86_64
  sha256 = if pkgs.stdenv.isDarwin then
    if isAarch64 then
      "5351297e3b8408213514b29c0a938002c5cf9f97eee28c2f32920e1227fd8423" # macos-aarch64
    else
      "16b0e1defe4c1807f2e128f72863124bffdd906cefb21043c34b673bf85cd57f" # macos-x86_64
  else if isAarch64 then
    "2166dc9f2d8df387e8b4122883bb979d739281e1ff3f3d5483fec3a23b957510" # linux-aarch64
  else
    "6c032fc61b5d77a3f3cf781730fa549f8f059ffdb3b3f6ad1c2994d2b2d87983"; # linux-x86_64
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
