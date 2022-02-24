# Web REPL

## High-level diagram

![High-level diagram](./repl-www.png)

## Steps

- User types text into the HTML `<input />` tag
- JS detects the `onchange` event and passes the input text to the Roc compiler WebAssembly module
- Roc compiler WebAssembly module
    - Parses the text (currently just a single line)
    - Type checks
    - Monomorphizes
    - Generates WebAssembly using the development backend (not LLVM)
    - Returns a slice of bytes to JavaScript
- JavaScript
    - Takes the slice of bytes and creates a `WebAssembly.Instance`
    - Runs the WebAssembly app
    - Gets the memory address of the result and makes a copy of the app's entire memory buffer
    - Passes the result address and the memory buffer to the compiler for analysis
- Roc compiler WebAssembly module
    - Analyses the bytes of the result, based on the known return type from earlier
    - Traverses the copied memory buffer to find any child values
    - Produces a user-friendly String and passes it to JavaScript
- JavaScript
    - Displays the input and output text on the web page

## Related crates

There are several directories/packages involved here:
- `repl_www`: The web page with its JavaScript and a build script
- `repl_wasm`: The Rust crate that becomes the "compiler" WebAssembly module
- `repl_eval`: REPL logic shared between `repl_cli` and `repl_wasm`

