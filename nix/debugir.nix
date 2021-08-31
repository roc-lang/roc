{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  name = "debugir";
  # we can go back to upstream when https://github.com/vaivaswatha/debugir/pull/4 gets merged
  src = pkgs.fetchFromGitHub {
    owner = "Arkham";
    repo = "debugir";
    rev = "8d543e8796a1ab28ebf708e8e6883a574dd5e48c";
    sha256 = "10sz3jwfkmqbp91d5aj9wd79k0cksl9qh4hirlivfyw8dcmbk384";
  };
  buildInputs = with pkgs; [ cmake libxml2 llvmPackages_12.llvm.dev ];
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
