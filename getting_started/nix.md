# Roc installation guide for Nix

To quickly try out roc, use `nix run`
```shell
nix run roc-lang/roc -- <roc args>
# example nix run roc-lang/roc -- repl
```

<details>
<summary>

## Install via Flakes

</summary>

### Bootstrap a project with a template

```shell
# use the template in the current directory
nix flake init --template github:roc-lang/roc#simple --refresh
```

### Add roc to existing flake
```nix
{
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
        flake-utils.url = "github:numtide/flake-utils";
        roc.url = "github:roc-lang/roc";
    };

    outputs = {nixpkgs, roc, flake-utils, ...}:
        flake-utils.lib.eachDefaultSystem (system:
            let
            pkgs = import nixpkgs { inherit system; };
            rocPkgs = roc.packages.${system};
            in
            {
                devShells = {
                    default = pkgs.mkShell {
                        buildInputs = with pkgs;
                        [
                            rocPkgs.cli
                        ];
                    };
                };
            }
        );
}
```

</details>
