# Install

Roc is a very young language with many incomplete features and known bugs. It doesn't even have a numbered release yet, but it does have [nightly builds](https://github.com/roc-lang/roc/releases) that you can download, if you'd like to try it out without [building from source](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md)! There are also [official Docker images](https://hub.docker.com/u/roclang) if you prefer to run Roc within a container.

<div class="banner">
    Roc is a <b>Work in Progress!</b> See the <a href="/plans">plans</a> page for more information.
</div>

There are currently a few known OS-specific issues:
* **macOS:** There are no known compatibility issues, but the compiler doesn't run as fast as it does on Linux or Windows, because we don't (yet) do our own linking like we do on those targets. (Linking works similarly on Linux and Windows, but the way macOS does it is both different and significantly more complicated.)
* **Windows:** There are some known Windows-specific compiler bugs, and probably some other unknown ones because more people have tried out Roc on Mac and Linux than on Windows.
* **Linux:** The nightlies are built with glibc, so they aren't usable on distros that don't use glibc, like Alpine. In the future we plan to build Linux releases with [musl libc](https://wiki.musl-libc.org/) to address this, but this requires [building LLVM from source with musl](https://wiki.musl-libc.org/building-llvm.html).
* **Other operating systems:** Roc has not been built on any other operating systems. [Building from source](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md) on another OS might work, but you might very well be the first person ever to try it!

### [Getting Started](#getting-started) {#getting-started}

Here are some Getting Started guides for different operating systems:
<!-- TODO detect current OS with browser and only show link for that, provide other button for others  -->

- [Linux x86-64](/install/linux_x86_64)
- [Nix Linux/MacOS](/install/nix)
- [MacOS Apple Silicon](/install/macos_apple_silicon)
- [MacOS x86-64](/install/macos_x86_64)
- [Windows](/install/windows)
- [Other Systems](/install/other)

### [Editor Extensions/Plugins](#editor-extensions) {#editor-extensions}

- [Visual Studio Code](https://visualstudio.microsoft.com/#vscode-section)
  - Features: syntax highlighting, completion, type hints, jump to source
  - syntax highlighting, completion, type hints, jump to source
  - install the [Roc Plugin](https://marketplace.visualstudio.com/items?itemName=IvanDemchenko.roc-lang-unofficial)
    - ❗make sure to follow ["Configuring language server"](https://github.com/ivan-demchenko/roc-vscode-unofficial?tab=readme-ov-file#configuring-language-server).
    - It would be a fantastic contribution for the language server to be set up automatically. If you'd like to help with this, just make a post in [the "new contributors" topic on Zulip](https://roc.zulipchat.com/#narrow/stream/316715-contributing/topic/new.20contributors) and say hello!
- [Zed](https://zed.dev/download), since Version 0.133.5
  - Features: syntax highlighting, completion, type hints, jump to source
  - search and install roc extension (action `zed: extensions`)
  - in case of errors look into the Zed log (action `zed: open log`)
- For other editors like Vim, Helix or Emacs [see](https://github.com/faldor20/tree-sitter-roc)

### [Tutorial](#tutorial) {#tutorial}

Once you've installed <code>roc</code>, check out the [tutorial](/tutorial) to learn how to Roc!

<a class="btn-small" href="/tutorial">Start Tutorial</a>

### [Additional Learning Resources](#additional-learning-resources) {#additional-learning-resources}

If you are looking for more resources to learn about Roc, check out the following:

- [Roc Examples](/examples)
- [Roc Guides](/docs#guides)
- [Roc Language Reference](/docs#language-reference)
- [Roc Exercism Track](https://exercism.org/tracks/roc)
