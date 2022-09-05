# Hello, World!

To run this website, first compile either of these identical apps:

```bash
# Option A: Compile examples/platform-switching/rocLovesWebAssembly.roc
cargo run -- build --target=wasm32 examples/platform-switching/rocLovesWebAssembly.roc

# Option B: Compile examples/platform-switching/main.roc with `pf: "web-assembly-platform/main.roc"` and move the result
cargo run -- build --target=wasm32 examples/platform-switching/main.roc
(cd examples/platform-switching && mv rocLovesPlatforms.wasm web-assembly-platform/rocLovesWebAssembly.wasm)
```

Then `cd` into the website directory
and run any web server that can handle WebAssembly.
For example, with `http-server`:

```bash
cd examples/platform-switching/web-assembly-platform
npm install -g http-server
http-server
```

Now open your browser at http://localhost:8080

## Design Notes

This demonstrates the basic design of hosts: Roc code gets compiled into a pure
function (in this case, a thunk that always returns `"Hello, World!\n"`) and
then the host calls that function. Fundamentally, that's the whole idea! The host
might not even have a `main` - it could be a library, a plugin, anything.
Everything else is built on this basic "hosts calling linked pure functions" design.

For example, things get more interesting when the compiled Roc function returns
a `Task` - that is, a tagged union data structure containing function pointers
to callback closures. This lets the Roc pure function describe arbitrary
chainable effects, which the host can interpret to perform I/O as requested by
the Roc program. (The tagged union `Task` would have a variant for each supported
I/O operation.)

In this trivial example, it's very easy to line up the API between the host and
the Roc program. In a more involved host, this would be much trickier - especially
if the API were changing frequently during development.

The idea there is to have a first-class concept of "glue code" which host authors
can write (it would be plain Roc code, but with some extra keywords that aren't
available in normal modules - kinda like `port module` in Elm), and which
describe both the Roc-host/C boundary as well as the Roc-host/Roc-app boundary.
Roc application authors only care about the Roc-host/Roc-app portion, and the
host author only cares about the Roc-host/C boundary when implementing the host.

Using this glue code, the Roc compiler can generate C header files describing the
boundary. This not only gets us host compatibility with C compilers, but also
Rust FFI for free, because [`rust-bindgen`](https://github.com/rust-lang/rust-bindgen)
generates correct Rust FFI bindings from C headers.
