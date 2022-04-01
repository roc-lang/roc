# Hello, World!

When using [Crystal](https://crystal-lang.org/) as a host for Roc the build process is a bit more convoluted.

This is becase Crystal programs requires linking some libraries. This is a built-in feature of the Crystal compiler but here we need to:

1. Compile the Crystal host to an .o file, yet not link it.
2. Compile the Roc program
3. Link 1 + 2 + the libraries the Crystal compiler would normally do on 1.

The paths of the libraries depends on your setup, so we will see how to check what would the Crystal compiler normally do and use the [ROC_LINK_FLAGS](https://github.com/rtfeldman/roc/pull/2553) env variable to have that configured for the Roc compiler.

First, `cd` into this directory and run:

```bash
$ crystal build host.cr
Undefined symbols for architecture x86_64:
  "_roc__mainForHost_1_exposed", referenced from:
      _*Roc::main:String in R-oc.o
ld: symbol(s) not found for architecture x86_64
clang-7: error: linker command failed with exit code 1 (use -v to see invocation)
Error: execution of command failed with code: 1: `clang "${@}" -o /path/to/roc/examples/hello-world/crystal-platform/host  -rdynamic -L/nix/store/j9q8gj0kx6ls7mbhx6lzfg4jfvhsmdrc-boehm-gc-8.0.6/lib -L/nix/store/2vkd78g13vmz6hpiyh9q63gz8xqgns2w-libatomic_ops-7.6.12/lib -L/nix/store/h7jqs74pkb919b5h6z0cfqj978zsbxyx-pcre-8.44/lib -L/nix/store/ld2vwz3jx37a5msbn8y5876xvskcbcd1-libevent-2.1.12/lib -L/nix/store/21i92ffd0iy6bpq21wr6v4x75wqkdhl8-libyaml-0.2.5/lib -L/nix/store/620yajdyaw36c08xzj9npqsn2k8xwssv-zlib-1.2.11/lib -L/nix/store/xirxmdb2zkqbhmf2if6697arsw65i6ax-libxml2-2.9.12/lib -L/nix/store/fcajbl41fdrdqfv7in4xwslqy09xmkan-openssl-1.1.1m/lib -L/nix/store/imyk4zmz2i0x8c23gj0cxkjrkj0j0lff-libiconv-50/lib -lpcre -lgc -lpthread -levent -liconv -ldl`
```

This fails because the host expects to be linked also with the Roc program. Don't worry, we are looking for the error.

Grab all the arguments that follows the `-rdynamic` and set it as `ROC_LINK_FLAGS`.

```bash
$ export ROC_LINK_FLAGS="-L/nix/store/j9q8gj0kx6ls7mbhx6lzfg4jfvhsmdrc-boehm-gc-8.0.6/lib -L/nix/store/2vkd78g13vmz6hpiyh9q63gz8xqgns2w-libatomic_ops-7.6.12/lib -L/nix/store/h7jqs74pkb919b5h6z0cfqj978zsbxyx-pcre-8.44/lib -L/nix/store/ld2vwz3jx37a5msbn8y5876xvskcbcd1-libevent-2.1.12/lib -L/nix/store/21i92ffd0iy6bpq21wr6v4x75wqkdhl8-libyaml-0.2.5/lib -L/nix/store/620yajdyaw36c08xzj9npqsn2k8xwssv-zlib-1.2.11/lib -L/nix/store/xirxmdb2zkqbhmf2if6697arsw65i6ax-libxml2-2.9.12/lib -L/nix/store/fcajbl41fdrdqfv7in4xwslqy09xmkan-openssl-1.1.1m/lib -L/nix/store/imyk4zmz2i0x8c23gj0cxkjrkj0j0lff-libiconv-50/lib -lpcre -lgc -lpthread -levent -liconv -ldl"
```

Alternative you can execute the following to automate the process.

```bash
$ export ROC_LINK_FLAGS="$(./get-flags.sh)"
```

Now you can run:

```bash
$ cargo run helloCrystal.roc
🔨 Rebuilding host... Done!
⚠️ CAUTION: The ROC_LINK_FLAGS environment variable is a temporary workaround, and will no longer do anything once surgical linking lands! If you're concerned about what this means for your use case, please ask about it on Zulip.
Hello Crystal, meet Roc
```

You will find the executable as `./helloCrystal`

```bash
$ ./helloCrystal
Hello Crystal, meet Roc
```
