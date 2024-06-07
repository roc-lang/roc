# Nix

## Try out

To quickly try out roc without installing, use `nix run`:
```shell
nix run github:roc-lang/roc -- <roc args>
# examples:
# - nix run github:roc-lang/roc -- repl
# - nix run github:roc-lang/roc -- dev main.roc
```

## Use with Flakes


### Start your project with our template

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

## Next Steps

- [editor setup](https://www.roc-lang.org/install#editor-extensions)
- [tutorial](https://www.roc-lang.org/tutorial)
- [examples](https://www.roc-lang.org/examples)
