# Roc installation guide for Nix


## NixOS quick start

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
        roc.url = "github:roc-lang/roc";
    };

    outputs = {roc,flake-utils, ...}:
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
