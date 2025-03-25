# Hello, World!

To run this website, we first compile the app that uses the Wasm platform:

- If you use the nightly roc release:
```bash
./roc build --target=wasm32 examples/platform-switching/roc_loves_web_assembly.roc
```
- If you start from the compiler source code:
```bash
# Build roc compiler if you have not done so already
cargo build
target/debug/roc build --target=wasm32 examples/platform-switching/roc_loves_web_assembly.roc
```
We then move the file:
```bash
# Go to the directory where index.html is
cd examples/platform-switching/web-assembly-platform/
# Move the .wasm file so that it's beside index.html
mv ../roc_loves_web_assembly.wasm .
```

In the directory where index.html is, run any web server on localhost.

For example if you have Python3 on your system, you can use `http.server`:
```bash
python3 -m http.server 8080
```

Or if you have npm, you can use `http-server`
```bash
npm install -g http-server
http-server
```

Now open your browser at <http://localhost:8080>

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
