# Install Roc

Roc is a work in progress. It doesn't have a numbered release yet, but it does have nightly builds that you can download.

There are currently a few OS-specific issues:
* macOS: There are no known compatibility issues, but the compiler doesn't run as fast as it does on Linux or Windows, because we don't (yet) do our own linking like we do on those targets. (Linking works similarly on Linux and Windows, but the way macOS does it is both different and significantly more complicated.)
* Windows: There are some known Windows-specific compiler bugs, and probably some other unknown ones because more people have tried out Roc on Mac and Linux than on Windows.
* Linux: The nightlies are built with glibc, so they aren't usable on distros that don't use (dynamically linked) glibc, like Alpine or NixOS. In the future we plan to build Linux releases with [musl libc](https://wiki.musl-libc.org/) to address this, but this requires [building LLVM from source with musl](https://wiki.musl-libc.org/building-llvm.html).
* Other operating systems: Roc has not been built on any other operating systems. Building from source on them might work, but hasn't been tried.

<!-- TODO detect current OS with browser and only show link for that, provide other button for others  -->

- [Linux x86-64](https://github.com/roc-lang/roc/blob/main/getting_started/linux_x86_64.md)
- [MacOS Apple Silicon](https://github.com/roc-lang/roc/blob/main/getting_started/macos_apple_silicon.md)
- [MacOS x86-64](https://github.com/roc-lang/roc/blob/main/getting_started/macos_x86_64.md)
- [Windows](https://github.com/roc-lang/roc/blob/main/getting_started/windows.md)
- [Other Systems](https://github.com/roc-lang/roc/blob/main/getting_started/other.md)

## Nightly

<!-- link to nightly for currently detected OS(browser) and provide "other" button to reveal all -->

## Roc CLI

<!-- brief description on how to use the cli -->
- Script `roc myApp.roc`
- Develop `roc dev`
- Test `roc test`
- Run `roc run`
- Build `roc build`
- Format `roc format`
- Documentation `roc docs`

## Package Management

You can include packages using an URL:

```roc
app "hello"
    packages {
        # basic-cli platform
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        # json package
        # TODO update to json 0.3.0
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.2.0/gh4zvR8xyEsef0R961Fcv5vxFEZJ-GJF-7bQwgL2Xz8.tar.br",
    }
    imports [
        pf.Stdout,
        json.Core,
    ]
    provides [main] to pf
```

A full Roc package manager will be developed in the future.

<!--

explain package manager design is still a work in progress
the plan is to create a centralised index
- ergonomicly integration
- a fact that every language will have one

in the meantime you can use package URLs

TODO Add an explanation for the URLs with the SHA and the tar.br. This explanation should only be revealed on click.
-->

## Editor Support

<!--
explain that the high level design is a work in progress
- Design goals for editor - we want Roc to ship with an awesome editor
- Beginners learning to get up an running with an editor
- Want it to run really fast etc

- Also link to VScode plugin
-->

- Language Server
- Neo(Vim)
