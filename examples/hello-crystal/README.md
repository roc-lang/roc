# Hello, World!

When using [Crystal](https://crystal-lang.org/) as a host for Roc the build process is a bit more convoluted.

This is becase Crystal programs requires linking some libraries. This is a built-in feature of the Crystal compiler but here we need to:

1. Compile the Crystal host to an .o file, yet not link it.
2. Compile the Roc program
3. Link 1 + 2 + the libraries the Crystal compiler would normally do on 1.

The paths of the libraries depends on your setup, so we will see how to check what would the Crystal compiler normally do and use the [ROC_LINK_FLAGS](https://github.com/rtfeldman/roc/pull/2553) env variable to have that configured for the Roc compiler.

First, `cd` into this directory and run:

```bash
$ crystal build platform/host.cr
Undefined symbols for architecture x86_64:
  "_roc__mainForHost_1_exposed", referenced from:
      _*Roc::main:String in R-oc.o
ld: symbol(s) not found for architecture x86_64
clang-7: error: linker command failed with exit code 1 (use -v to see invocation)
Error: execution of command failed with code: 1: `clang "${@}" -o /Users/bcardiff/Projects/roc/roc/examples/hello-crystal/host  -rdynamic -L/nix/store/dq088v1hsxf5gmrr7dxni6k1ir9xfhz6-boehm-gc-8.0.6/lib -L/nix/store/wqhg4wd0m6w0bghiwdv33n43bzas7mng-libatomic_ops-7.6.12/lib -L/nix/store/xd44r5yk3ffa4q8za208wiwp3x2hni60-pcre-8.44/lib -L/nix/store/fxyjjjjfynjlv83n21in1x8qx304xgl8-libevent-2.1.12/lib -L/nix/store/q7hpsn55pqk18amavqn9piysli506d3p-libyaml-0.2.5/lib -L/nix/store/227hsg7nm2wlg9qpr9dk0jjhmfqljs9l-zlib-1.2.11/lib -L/nix/store/wqrdraqn01jk03mrlqbqjxa2b1xq096v-libxml2-2.9.12/lib -L/nix/store/06k9yri1dcmjh11k2zr1yzm58g2gd7yd-openssl-1.1.1m/lib -L/nix/store/mp6mfy7jwciz607xcsgn5j8q7xnbh3n2-libiconv-50/lib -lpcre -lgc -lpthread -levent -liconv -ldl`
```

This fails because the host expects to be linked also with the Roc program. Don't worry, we are looking for the error.

Grab all the arguments that follows the `-rdynamic` and set it as `ROC_LINK_FLAGS`.

```
$ export ROC_LINK_FLAGS="-L/nix/store/dq088v1hsxf5gmrr7dxni6k1ir9xfhz6-boehm-gc-8.0.6/lib -L/nix/store/wqhg4wd0m6w0bghiwdv33n43bzas7mng-libatomic_ops-7.6.12/lib -L/nix/store/xd44r5yk3ffa4q8za208wiwp3x2hni60-pcre-8.44/lib -L/nix/store/fxyjjjjfynjlv83n21in1x8qx304xgl8-libevent-2.1.12/lib -L/nix/store/q7hpsn55pqk18amavqn9piysli506d3p-libyaml-0.2.5/lib -L/nix/store/227hsg7nm2wlg9qpr9dk0jjhmfqljs9l-zlib-1.2.11/lib -L/nix/store/wqrdraqn01jk03mrlqbqjxa2b1xq096v-libxml2-2.9.12/lib -L/nix/store/06k9yri1dcmjh11k2zr1yzm58g2gd7yd-openssl-1.1.1m/lib -L/nix/store/mp6mfy7jwciz607xcsgn5j8q7xnbh3n2-libiconv-50/lib -lpcre -lgc -lpthread -levent -liconv -ldl"
```

Now you can run:

```bash
$ cargo run Hello.roc
🔨 Rebuilding host... Done!
⚠️ CAUTION: The ROC_LINK_FLAGS environment variable is a temporary workaround, and will no longer do anything once surgical linking lands! If you're concerned about what this means for your use case, please ask about it on Zulip.
Hello Crystal, meet Roc
```

You will find the executable as `./hello-crystal`

```bash
$ ./hello-crystal
Hello Crystal, meet Roc
```
