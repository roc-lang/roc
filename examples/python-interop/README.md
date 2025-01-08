# Python Interop

This is a demo for calling Roc code from [Python](https://www.python.org/).

## Installation

The following was tested on NixOS, with `Python 3.10`, `clang 13.0.1`, `gcc 11.3.0` but should work with most recent versions of those on most modern Linux and MacOS.\
Of course you're welcome to test on your machine and tell me (via [Zulip](https://roc.zulipchat.com/#narrow/pm-with/583319-dank)) if you ran into any issues or limitations.

> Because of some rough edges, the linux installation may be a bit more involved (nothing too bad, mostly stuff like renames), so for your convenience I made a small shell script to help out.

Now in favor of users of all OSs, let's first do a step by step walkthrough on how the build process works, and later, a few key notes on the implementation.

## Building the Roc library

First, `cd` into this directory and run this in your terminal:

```sh
roc build --lib
```

This compiles your Roc code into a binary library in the current directory. The library's filename will be `libhello` plus an OS-specific extension (e.g. `libhello.dylib` on macOS).

## Some Linux Specific Prep Work
As of the time of writing this document, `roc build --lib` generates a shared object with the suffix `.so.1.0`.\
This `.0` suffix is not needed by neither the application nor Python, so we can simply rename it.

``` sh
mv libhello.so.1.0 libhello.so.1
```
But, one of which does expect plain libhello.so, so we symlink it:

```sh
ln -sf libhello.so.1 libhello.so
```

Also, one thing about dynamically linked applications like this one, is that they need to know where to look for its shared object dependencies, so we need to let CPython know that we hold libhello in this directory, so:

``` sh
export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
```

That wasn't so bad and we're already done with prep work, all that's left it to build our C extension.
## Building the C Extension 
``` sh
# If you want, you can set the environment variable cc, to compile with clang instead of gcc
python -m venv .interop_env
source .interop_env/bin/activate # activate.fish if you like fish
python setup.py install
```
For cleanness sake, we make virtual environment here, but do note you don't have to if you don't want to.
You can also run `python setup.py build` and grab the output shared object from the `build/lib.*` directory.\
Shared objects are simply importable in CPython (which is great!), so you can just start up an interpreter in the same directory as your new `demo.so` and get the same result.

**Note -** after all is said and done, for prolonged use, you may want to move your shared library (lib hello) to somewhere permanent on your `LD_LIBRARY_PATH`, or add your desired directory to `LD_LIBRARY_PATH` in some way (e.g put it in your shell .rc).

## Try it out!

Now we can see our work by entering an interactive shell and calling our function!

```py
❯ python -q
>>> import demo
>>> demo.call_roc(21)
'The number was 21, OH YEAH!!! 🤘🤘'
```

## Notes on implementation
The structure of python-interop is very similar to a C-Extension, in fact, it is one.\
We have:
- [`PyModuleDef demoModule`](https://docs.python.org/3/c-api/module.html#c.PyModuleDef) struct which declares the `demo` python module,
- [`PyMethodDef DemoMethods`](https://docs.python.org/3/c-api/structures.html#c.PyMethodDef) struct which declares `demo`'s methods,
- [`PyMODINIT_FUNC PyInit_demo`](https://docs.python.org/3/extending/extending.html) which is `demo`’s initialization function, and of course,
- [`PyObject * call_roc`] which is our demo function! Getting args and returning our string to the interpreter. The Roc-Python bridge lives here, all the above are merely CPython boilerplate to wrap up a C implementation into a valid Python module.

The first three are basically the backbone of any C-API extension.\
In addition, a couple more functions and notes you may want to pay attention to:
- [`void roc_panic`] - When creating such interpreter-dependent code, it is reasonable to make the implementation of this function fire up an interpreter Exception (e.g `RuntimeError` or whatever suits).
- When I first came across another implementation, I was a bit confused about `extern void roc__main_for_host_1_exposed_generic`, so let me clarify - this is an external function, implemented by the application, that goes (on the application side-) by the name `main_for_host`.

And one last thing -
- When writing such the C glue (here, `demo.c`), you need to pay attention to not only Python's reference counting, but also Roc's, so remember to wear seatbelts and decrement your ref counts.
