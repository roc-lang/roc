# A mockin' Roc REPL!

This is a mock-up Web REPL for a fake compiler. The only valid inputs are the numbers 0-255! The output program is a Wasm module that counts backwards from that number to 1.

The same web page should work with minimal adjustments whenever we manage to get a WebAssembly build of the Roc compiler working. But this way, I was able to build up the functionality more gradually.

How it works

- There are two Wasm modules: a compiler, and an app
- The compiler simply modifies a single byte in an otherwise fixed Wasm binary, using your input.
- The compiler sends the Wasm binary to JS, which runs it
- JS calls back into another function in the compiler that stringifies the result from the app
- JS reads the string and displays it under the input

See it live on [GitHub Pages](https://brian-carroll.github.io/mock-repl/)
