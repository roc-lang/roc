# devtools

[Install nix](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#installing-nix) if you have not already done so.

To make rust-analyzer and other vscode extensions work well you want them using the same rustc, glibc, zig... as specified in the roc nix flake.
The easiest way to do this is to use another flake for all your dev tools that takes the roc flake as an input.

The flake in this folder is meant for vscode, feel free to create a PR if you'like to add a flake for a different editor.

Further steps:

1. Copy the flake.nix and flake.lock file from the devtools folder to a new folder outside of the roc repo folder.
1. Run `git init` in the new folder.
1. Execute `git add flake.nix flake.lock`, nix will error if you don't do this.
1. Change `roc.url = "path:/home/username/gitrepos/roc";` to the location of the roc folder on your machine.
1. Follow instructions about vscode extensions [here](#extensions).
1. add other dev tools you like in the `devInputs` list. You can search for those [here](https://search.nixos.org/packages).
1. Run `nix develop`.
1. `cd` to the folder of the roc repo
1. Run `code .` to start vscode.

vscode is able to share settings between this nix version and your regular vscode so there is no need to set everything up from scratch.

If you use lorri or direnv it is possible to load the dev flake instead of the roc flake.
For lorri:

1. copy the `shell.nix` at the root of this repo to the folder containing your dev tools flake.
1. edit `.envrc` to contain:

```sh
eval "$(lorri direnv --shell-file path-to-your-dev-flake-folder/shell.nix)"
```

## Extensions

### for those who have some Nix experience

Add vscode extensions you want to use below `vscodeExtensions = ...`. You can search for extension names [here](https://search.nixos.org/packages?channel=22.05&from=0&size=50&sort=relevance&type=packages&query=vscode-extensions+extensionYouAreSearchingFor). You may not be able to install extensions through the UI in vscode, I'd recommend always adding them to the flake. If you are inside a `nix develop` shell, run `exit` and `nix develop path-to-your-dev-flake-folder` to be able to run vscode with the newly added extensions.

If your extension is not available on nix, you can add them [from the vscode marketplaces as well](https://stackoverflow.com/a/54812021/4200103).

### for those with little to no Nix experience

Instead of running `code` in the last step you can use the `--extensions-dir` flag to allow you to install extensions using the vscode GUI.
On MacOS or Linux:

```sh
code --extensions-dir="$HOME/.vscode/extensions"
```
