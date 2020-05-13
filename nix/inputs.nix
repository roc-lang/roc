{ pkgs }: [
  pkgs.rustup
  pkgs.cargo
  pkgs.llvm_10
  # libraries for llvm
  pkgs.libffi
  pkgs.libxml2
  pkgs.zlib
]
