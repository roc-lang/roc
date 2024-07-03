# roc_language_server

This is a basic language server for Roc.

Support for the following LSP features are provided:

- Inline diagnostics
- Hover to view type of value
- Go-to-definition
  - <details><summary>Example</summary>

    https://github.com/ayazhafiz/roc/assets/20735482/23a57d06-5b70-46f2-b0c4-5836eaec669b

    </details>
  - Note that go-to-definition for the builtins does not yet work.
  - Go-to-definition for abilities resolves to their specialization, if one exists.
    - <details><summary>Example</summary>

      https://github.com/ayazhafiz/roc/assets/20735482/1ba98bf9-518b-4c47-b606-a6ce6767566f

      </details>
- Formatting Roc files on save
  - <details><summary>Example</summary>

    https://github.com/ayazhafiz/roc/assets/20735482/fbbe4bc1-64af-4c7d-b633-d7761906df11

    </details>

[Semantic highlighting](https://github.com/microsoft/vscode/wiki/Semantic-Highlighting-Overview#what-is-the-difference-between-syntax-and-semantic-highlighting) will be added soon. Additional features require
changes to the compiler infrastructure that are not yet available.

Note that the language server is a bit naïve:
- If you make a change in a dependency, you'll also need to make a change in
    the dependents' files for the changes to be picked up.
- The language server will only operate on changes on save, auto-saving is recommended.

## Installing

The roc_language_server binary is included with the [nightly releases](https://github.com/roc-lang/roc/releases). We recommend using the same version of roc and roc_language_server.

### Building from source

Follow the [building from source](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md) instructions for roc. Then run:

```bash
# do `nix develop` first if you're using nix!
cargo build --bin roc_language_server --release
```

This will give you the language server binary at:

```
target/release/roc_language_server
```

### Configuring in your editor

Please follow your editor's language server implementation's documentation to see how custom language servers should be configured.

#### [coc.nvim](https://github.com/neoclide/coc.nvim)

Add the following to your coc JSON configuration file:

```json
{
  "languageserver": {
    "roc": {
      "command": "<path to binary folder>/roc_language_server",
      "filetypes": ["roc"]
    }
  }
}
```

If you're using coc.nvim and want to use the configuration above, be sure to also instruct your vim that `*.roc` files have roc filetype.

#### [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)

Add the following to your lspconfig setup:

```lua
local lspconfig = require('lspconfig')
lspconfig.roc_ls.setup()
```

Make sure that the roc binary is on your `PATH`.
Further instructions on how to setup nvim-lspconfig can be found in their [docs](https://github.com/neovim/nvim-lspconfig?tab=readme-ov-file#quickstart).

## Debug

If you want to debug the server, use [debug_server.sh](./debug_server.sh)
instead of the direct binary.

If you would like to enable debug logging set the `ROCLS_LOG` environment variable to `debug` or `trace` for even more logs. 
eg: `ROCLS_LOG=debug`  

## Testing

Tests use expect-test, which is a snapshot/expect testing framework.
If a change is made that requires updating the expect tests run `cargo test` confirm that the diff is correct, then run `UPDATE_EXPECT=1 cargo test` to update the contents of the files with the new output.

## Config

You can set the environment variables below to control the operation of the language.

`ROCLS_DEBOUNCE_MS`: Sets the amount of time to delay starting analysis of the document when a change comes in. This prevents starting pointless analysis while you are typing normally. 
Default: `100`

`ROCLS_LATEST_DOC_TIMEOUT_MS`: Sets the timeout for waiting for an analysis of the latest document to be complete. If a request is sent that needs the latest version of the document to be analyzed, then it will wait up to this duration before just giving up.
Default: `5000`  
