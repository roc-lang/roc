{ ... }: rec {
  default = simple;
  simple = {
    description = "Basic flake with roc cli + lsp";
    path = ./simple;
  };
}
