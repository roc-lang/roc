# Roc LSP
The Roc compiler can now expose an experimental Language Server Protocol (LSP) endpoint
written in Zig as part of the Rust to Zig rewrite.

## Current state
The experimental LSP currently only holds the scaffolding for the incoming implementation.
It doesn't implement any LSP capabilities yet except `initialized` and `exit` which allows it
to be connected to an editor and verify it's actually running.

## Starting the server
Build the Roc toolchain and run:
```bash
roc experimental-lsp
```
Normally you would just let your editor handle this.

## Debugging
Because the LSP takes control of the standard input and output, an optional flag was added.
```bash
roc experimental-lsp --debug-transport
```

Passing the `--debug-transport` flag will create a log file in your OS tmp folder (`/tmp` on Unix
systems). A mirror of the raw JSON-RPC traffic will be appended to the log file. Watching the file 
will allow an user to see incoming and outgoing message between the server and the editor
```bash
tail -f /tmp/roc-lsp-debug-1763905495474.log 
---
[1763905495476] OUT (127 bytes)
{"jsonrpc":"2.0","id":1,"result":{"capabilities":{"positionEncoding":"utf-16"},"serverInfo":{"name":"roc-lsp","version":null}}}
---
[1763905495529] IN (52 bytes)
{"jsonrpc":"2.0","method":"initialized","params":{}}
---
```

## Editor examples

### Neovim (lua + nvim-lspconfig)
The plan is to eventually just add the lsp to the Mason package manager, but for the meantime a manual
setup is possible. Your config might vary, but it'll look something like this:
```lua
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")

if not configs.roc_lsp then
  configs.roc_lsp = {
    default_config = {
      name = 'roc_lsp',
      cmd = { '/path/to/roc', 'experimental-lsp', '--debug-transport' },
      filetypes = { 'roc' },
      root_dir = function(fname)
        return util.path.dirname(fname)
      end,
      single_file_support = true,
    },
  }
end

-- If using Mason, this next part goes other the Mason setup
lspconfig.roc_lsp.setup({
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
})

-- If using Mason, you might have to remove the ensure_installed check
ensure_installed = vim.tbl_filter(function(name)
  return name ~= 'roc_lsp'
end, ensure_installed)
```

### VS Code

TODO

### Zed

TODO
