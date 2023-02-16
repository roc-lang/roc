# Python Interop

This is an example of calling Roc code from [Python](https://www.python.org/).

## Installation

The following was tested on NixOS, with Python 3.10, clang 13.0.1, gcc 11.3.0 but this should work on with most recent python3 and clang versions on most modern Linux and MacOS.\
Of course you're welcome to test on your machine and tell me if you ran into any issues or limitations.

For your convenience, I've created a shell script (linux specific, you'll see why in a second) to take care of some rough edges (nothing too bad, mostly stuff like renames).
But running random shell scripts may not be your cup of tea so let's first do a step by step walkthrough on how it works, and also provide general instructions for users of all operating systems:

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

``` sh
ln -sf libhello.so.1 libhello.so
```

Also, one thing about dynamically linked applications like this one, is that they need to know where to look for its shared object dependencies, so we need to let CPython know that we hold libhello in this directory, so:

``` sh
export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
```

That wasn't so bad and we're already done with prep work, all that's left it to build our C extension.

``` sh
# If you want, you can set the environment variable cc, to compile with clang instead of gcc
python -m venv .interop_env
source .interop_env/bin/activate # /activate.fish if you're on fish
python setup.py install
```
For cleanness sake, we make virtual environment here, but do note you don't have to if you don't to, you can also `setup.py build` and grab the output shared object from the build/ directory.\
Shared objects are simply importable in CPython (which is great!), so you would be good to go start an interpreter in the same directory as your new demo.so and get the same result as the following-

## Try it out!

Now we can see our work by entering an interactive shell and calling our function!

```sh
❯ py -q
>>> import demo
>>> demo.call_roc(21)
'"The number was 21, OH YEAH!!! 🤘🤘"'
```
