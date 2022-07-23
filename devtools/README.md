To make rust-analyzer and other vscode extensions work well you want them using the same rustc, glibc, zig... as specified in the roc nix flake.
The easiest way to do this is to use another flake for all your dev tools that takes the roc flake as an input.

Use the flake in this folder that uses your editor of choice as a starting template. If your editor is not listed, feel free to make a PR and add your flake.

Further steps:
1. copy the flake for your favorite editor to a new folder outside of the roc repo folder
1. rename the copied flake to `flake.nix`
1. change `roc.url = "path:/home/anton/gitrepos/roc3/roc";` to the location of the roc folder on your machine
1. add vscode extensions you like below `vscodeExtensions = ...`. You can search for extension names [here](https://search.nixos.org/packages?channel=22.05&from=0&size=50&sort=relevance&type=packages&query=vscode-extensions+extensionYouAreSearchingFor). 
1. add other dev tools you like in the `devInputs` list. You can search for those [here](https://search.nixos.org/packages).
1. from the roc folder run `nix develop path-to-your-dev-flake-folder`
1. run the `code` command to start vscode

vscode is able to share settings between this nix version and your regular vscode so there is no need to set everything up from scratch.

I recommend creating a git repository to save this custom flake.

If you use lorri or direnv it is possible to load the dev flake instead of the roc flake.
For lorri:
1. copy the `shell.nix` at the root of this repo to the folder containing your dev tools flake.
1. edit `.envrc` to contain:
```
eval "$(lorri direnv --shell-file path-to-your-dev-flake-folder/shell.nix)"
``` 