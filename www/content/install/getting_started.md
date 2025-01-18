# Getting Started

Roc is a language for making delightful software. It does not have an 0.1 release yet, and we
certainly don't recommend using it in production in its current state! However, it can be fun to
play around with as long as you have a tolerance for missing features and compiler bugs. :)

The [tutorial](/tutorial) is the best place to learn about how to use the language - it assumes no prior knowledge of Roc or similar languages.

If you have a specific question, the [FAQ](/faq) might have an answer, although [Roc Zulip chat](https://roc.zulipchat.com) is overall the best place to ask questions and get help! It's also where we discuss [ideas](https://roc.zulipchat.com/#narrow/stream/304641-ideas) for the language. If you want to get involved in contributing to the language, Zulip is also a great place to ask about good first projects.

## [Installation](#installation){#installation}

- [🐧 Linux x86_64](/install/linux_x86_64)
- [❄️ Nix](/install/nix)
- [🍏 MacOS Apple Silicon](/install/macos_apple_silicon)
- [🍏 MacOS x86_64](/install/macos_x86_64)
- [🟦 Windows](/install/windows)
- [Other](/install/other)

## Editor

- [Visual Studio Code](https://visualstudio.microsoft.com/#vscode-section)
  - syntax highlighting, completion, type hints 
  - install the [Roc Plugin](https://marketplace.visualstudio.com/items?itemName=IvanDemchenko.roc-lang-unofficial)
- [Zed](https://zed.dev/download), since Version 0.133.5
  - search and install roc extension (action `zed: extensions`)
  - in case of errors look into the Zed log (action `zed: open log`)
- For other editors like Vim, Helix or Emacs [see](https://github.com/faldor20/tree-sitter-roc)

## Running Examples

You can run [examples](https://github.com/roc-lang/examples) as follows:

```sh
git clone https://github.com/roc-lang/examples.git
cd examples
roc ./HelloWorld/main.roc
```

## Getting Involved

The number of people involved in Roc's development has been steadily increasing
over time - which has been great, because it's meant we've been able to onboard
people at a nice pace. (Most people who have contributed to Roc had previously
never done anything with Rust and also never worked on a compiler, but we've
been able to find beginner-friendly projects to get people up to speed gradually.)

If you're interested in getting involved, check out
[CONTRIBUTING.md](https://github.com/roc-lang/roc/blob/main/CONTRIBUTING.md)!

If you're interested in substantial implementation- or research-heavy projects
related to Roc, check out [Roc Project Ideas][project-ideas]!

[project-ideas]: https://docs.google.com/document/d/1mMaxIi7vxyUyNAUCs98d68jYj6C9Fpq4JIZRU735Kwg/edit?usp=sharing
