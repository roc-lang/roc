# TypeScript Interop

This is an example of calling Roc code from [TypeScript](https://www.typescriptlang.org/) on [Node.js](https://nodejs.org/en/).

## Installation

You'll need to have a C compiler installed, but most operating systems will have one already.
(e.g. macOS has `clang` installed by default, Linux usually has `gcc` by default, etc.)
All of these commands should be run from the same directory as this README file.


First, run this to install Node dependencies and generate the Makefile that will be
used by future commands. (You should only need to run this once.)

```
npm install
npx node-gyp configure
```

## Building the Roc library

First, `cd` into this directory and run this in your terminal:

```
roc build --lib
```

This compiles your Roc code into a shared library in the current directory. The library's filename will be libhello plus an OS-specific extension (e.g. libhello.dylib on macOS).

Next, run this to rebuild the C sources.

```
npx node-gyp build
```

Finally, run this to copy the generated TypeScript type definitions into the build directory:

```
cp addon.d.ts build/Release/
```

You can verify that TypeScript sees the correct types with:

```
npx tsc hello.ts
```

### Try it out!

Now that everything is built, you should be able to run the example with:

```
npx ts-node hello.ts
```

To rebuild after changing either the `demo.c` file or any `.roc` files, run:

```
roc build --lib && npx node-gyp build
```

## About this example

This was created by following the [NodeJS addons](https://nodejs.org/dist/latest/docs/api/addons.html) tutorial and switching from C++ to C, then creating the `addon.d.ts` file to add types to the generated native Node module.