{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  name = "debugir";
  src = pkgs.fetchFromGitHub {
    owner = "vaivaswatha";
    repo = "debugir";
    rev = "b981e0b74872d9896ba447dd6391dfeb63332b80";
    sha256 = "Gzey0SF0NZkpiObk5e29nbc41dn4Olv1dx+6YixaZH0=";
  };
  buildInputs = with pkgs; [ cmake libxml2 llvmPackages_13.llvm.dev ];
  buildPhase = ''
    mkdir build
    cd build
    cmake -DLLVM_DIR=${pkgs.llvmPackages_13.llvm.dev} -DCMAKE_BUILD_TYPE=Release ../
    cmake --build ../
    cp ../debugir .
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp debugir $out/bin
  '';
}
