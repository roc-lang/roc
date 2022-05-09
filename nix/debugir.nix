{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  name = "debugir";
  src = pkgs.fetchFromGitHub {
    owner = "vaivaswatha";
    repo = "debugir";
    rev = "db871e6cee7f653e284b226e2567a2574635247c";
    sha256 = "0rgh9gawf92mjya1plxlgi9azkwca3gq8qa5hri18k4b7sbjm6lx";
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
