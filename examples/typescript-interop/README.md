## Running the example

You'll need to have a C compiler installed, but most operating systems will have one already.
(e.g. macOS has `clang` installed by default, Linux usually has GCC by default, etc.)
All of these commands should be run from the same directory as this README file.

### Setup before first build

First, run this to install Node dependencies and generate the Makefile that will be
used by future commands. (You should only need to run this once.)

```
npm install
npx node-gyp configure
```

### Build

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

### Run

Now you should be able to run the example with:

```
npx ts-node hello.ts
```