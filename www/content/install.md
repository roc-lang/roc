# Install

Roc is a very young language with many incomplete features and known bugs. It doesn't even have a numbered release yet, but it does have [nightly builds](https://github.com/roc-lang/roc/releases) that you can download if you'd like to try it out without [building from source](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md)!

There are currently a few known OS-specific issues:
* **macOS:** There are no known compatibility issues, but the compiler doesn't run as fast as it does on Linux or Windows, because we don't (yet) do our own linking like we do on those targets. (Linking works similarly on Linux and Windows, but the way macOS does it is both different and significantly more complicated.)
* **Windows:** There are some known Windows-specific compiler bugs, and probably some other unknown ones because more people have tried out Roc on Mac and Linux than on Windows.
* **Linux:** The nightlies are built with glibc, so they aren't usable on distros that don't use (dynamically linked) glibc, like Alpine or NixOS. In the future we plan to build Linux releases with [musl libc](https://wiki.musl-libc.org/) to address this, but this requires [building LLVM from source with musl](https://wiki.musl-libc.org/building-llvm.html).
* **Other operating systems:** Roc has not been built on any other operating systems. [Building from source](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md) on another OS might work, but you might very well be the first person ever to try it!

### [Getting Started](#getting-started) {#getting-started}

Here are some Getting Started guides for different operating systems:
<!-- TODO detect current OS with browser and only show link for that, provide other button for others  -->

- [Linux x86-64](https://github.com/roc-lang/roc/blob/main/getting_started/linux_x86_64.md)
- [MacOS Apple Silicon](https://github.com/roc-lang/roc/blob/main/getting_started/macos_apple_silicon.md)
- [MacOS x86-64](https://github.com/roc-lang/roc/blob/main/getting_started/macos_x86_64.md)
- [Windows](https://github.com/roc-lang/roc/blob/main/getting_started/windows.md)
- [Other Systems](https://github.com/roc-lang/roc/blob/main/getting_started/other.md)

### [Editor Extensions](#editor-extensions) {#editor-extensions}

There is currently a [VS Code extension](https://marketplace.visualstudio.com/items?itemName=IvanDemchenko.roc-lang-unofficial) for Roc which includes instructions in its README for how to enable a Roc language server.

Currently that language server has to be built from source; it would be a fantastic contribution if anyone could get it incorporated it into the extension directly. If you'd like to help with this, just make a post in [the "new contributors" topic on Zulip](https://roc.zulipchat.com/#narrow/stream/316715-contributing/topic/new.20contributors) and say hello!

### [Tutorial](#tutorial) {#tutorial}

Once you've installed <code>roc</code>, check out the [tutorial](/tutorial) to learn how to Roc!

<a class="btn-small" href="/tutorial">Start Tutorial</a>
