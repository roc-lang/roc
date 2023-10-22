# roc_ls

This is a rudimentary language server for supporting basic editor usage in Roc.

Support for the following LSP features are provided:

- Inline diagnostics
- Hover support to get types of values
- Go-to-definition
  !(go-to-definition in roc pg)[./img/go-to-definition.mov]
  - Note that go-to-definition for the builtins does not yet work.
  - Go-to-definition for abilities resolves to their specialization, if one exists.
    !(go-to-definition in roc pg)[./img/go-to-definition-abilities.mov]
- Formatting Roc files on save
  !(format)[./img/format.mov]

Semantic highlighting will also be added soon. Additional features require
changes to the compiler infrastructure that are not yet available.

Note that the language server is a bit naive:
- If you make a change in a dependency, you'll need to also make a change in
    the dependents' files for the changes to be picked up
- The language server will only operate on changes that are also reflected on
    disk (so save often)

## Installing

At this time, only from-source installations of the binary are supported.

Follow the (installation from source)[https://github.com/roc-lang/roc/tree/main/getting_started#installation] instructions. Then run

```
cargo build -p roc_lang_srv --release
```

which will give you a language server binary at

```
target/release/roc_ls
```

### Configuring in your editor

Please follow your editor's language server implementation's documentation to see how custom language servers should be configured.

#### [coc.nvim](https://github.com/neoclide/coc.nvim)

Add the following to your coc JSON configuration file:

```
{
  "languageserver": {
    "roc": {
      "command": "<path to binary folder>/roc_ls",
      "filetypes": ["roc"]
    }
  }
}
```

If you're using coc.nvim and want to use the configuration above, be sure to also instruct your vim that `*.roc` files have roc filetype.

## Debug

If you want to debug the server, use [debug_server.sh](./debug_server.sh)
instead of the direct binary.
