{ pkgs }: [
  pkgs.rustup
  pkgs.cargo
  pkgs.llvm_8
  # libraries for llvm
  pkgs.libffi
  pkgs.libxml2
  pkgs.zlib
]
