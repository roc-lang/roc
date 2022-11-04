# devtools

To make rust-analyzer and other vscode extensions work well you want them using the same rustc, glibc, zig... as specified in the roc nix flake.
The easiest way to do this is to use another flake for all your dev tools that takes the roc flake as an input.

Use the flake in this folder that uses your editor of choice as a starting template. If your editor is not listed, feel free to make a PR and add your flake.

Further steps:

1. Copy the flake for your favorite editor to a new folder outside of the roc repo folder.
1. Run `git init` in the new folder.
1. Rename the copied flake to `flake.nix`.
1. Execute `git add flake.nix`, nix will error if you don't do this.
1. Change `roc.url = "path:/home/anton/gitrepos/roc3/roc";` to the location of the roc folder on your machine.
1. Follow instructions about vscode extensions [here](#extensions).
1. add other dev tools you like in the `devInputs` list. You can search for those [here](https://search.nixos.org/packages).
1. From the roc folder run `nix develop path-to-your-dev-flake-folder`.
1. Run the `code` command to start vscode.

vscode is able to share settings between this nix version and your regular vscode so there is no need to set everything up from scratch.

I recommend creating a git repository to save this custom flake.

If you use lorri or direnv it is possible to load the dev flake instead of the roc flake.
For lorri:

1. copy the `shell.nix` at the root of this repo to the folder containing your dev tools flake.
1. edit `.envrc` to contain:

```sh
eval "$(lorri direnv --shell-file path-to-your-dev-flake-folder/shell.nix)"
```

## Extensions

### for those we have some Nix experience

Add vscode extensions you want to use below `vscodeExtensions = ...`. You can search for extension names [here](https://search.nixos.org/packages?channel=22.05&from=0&size=50&sort=relevance&type=packages&query=vscode-extensions+extensionYouAreSearchingFor). You may not be able to install extensions through the UI in vscode, I'd recommend always adding them to the flake. If you are inside a `nix develop` shell, run `exit` and `nix develop path-to-your-dev-flake-folder` to be able to run vscode with the newly added extensions.

If your extension is not available on nix, you can add them [from the vscode marketplaces as well](https://stackoverflow.com/a/54812021/4200103).

### for those with little to no Nix experience

Instead of running `code` in the last step you can use the `--extensions-dir` flag to allow you to install extensions using the vscode GUI.
On MacOS or Linux:

```sh
code --extensions-dir="$HOME/.vscode/extensions"
```
