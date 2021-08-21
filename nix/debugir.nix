{ pkgs ? import <nixpkgs> {}}:

pkgs.stdenv.mkDerivation {
  name = "debugir";
  src = pkgs.fetchFromGitHub {
    owner = "vaivaswatha";
    repo = "debugir";
    rev = "ed454ba264f30d2a70264357a31d94db3dd676eb";
    sha256 = "08hrn66zn5pa8jk45msl9ipa8d1p7r9gmpknh41fyjr6c7qpmfrk";
  };
  buildInputs = with pkgs; [
    cmake
    libxml2
    llvmPackages_12.llvm.dev
  ];
  buildPhase = ''
    mkdir build
    cd build
    cmake -DLLVM_DIR=${pkgs.llvmPackages_12.llvm.dev} -DCMAKE_BUILD_TYPE=Release ../
    cmake --build ../
    cp ../debugir .
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp debugir $out/bin
  '';
}
