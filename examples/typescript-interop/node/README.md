# TypeScript Interop

This is an example of calling Roc code from [TypeScript](https://www.typescriptlang.org/) on [Node.js](https://nodejs.org/en/).

You'll need to have a C compiler installed, but most operating systems will have one already.
(e.g. macOS has `clang` installed by default, Linux usually has `gcc` by default, etc.)
All of these commands should be run from the same directory as this README file.

## Building

First, `cd` into this directory's `platform/` subdirectory and run this in your terminal:

```
roc glue roc-to-node.roc glue/
```

This will generate the bindings between Node and Roc, as well as TypeScript type definitions.

Next, `cd ..` to get to the parent directory of `platform/` and then run:

```
roc build --lib
```

This compiles your Roc code into a shared library in the current directory. The library's filename will be `libhello` plus an OS-specific extension (e.g. `libhello.dylib` on macOS).

Finally, run this to install Node dependencies and build the behind-the-scenes C bindings.

```
npm install
```

You can verify that TypeScript now sees the correct types with:

```
npx tsc hello.ts
```

### Try it out!

Now that everything is built, you should be able to run the example with:

```
npx ts-node hello.ts
```

To change the bindings between Roc and TypeScript, `cd` into `platform/` and run:

```
rm -rf glue/ && roc glue roc-to-node.roc glue/
```

Then `cd` back into the parent directory and rebuild with:

```
roc build --lib && npm install
```

## About this example

This was created by following the [NodeJS addons](https://nodejs.org/dist/latest/docs/api/addons.html) tutorial and switching from C++ to C, then creating the `addon.d.ts` file to add types to the generated native Node module.
