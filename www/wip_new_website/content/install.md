# Install Roc

## Getting Started Guides

- [Linux x86-64](https://github.com/roc-lang/roc/blob/main/getting_started/linux_x86_64.md)
- [MacOS Apple Silicon](https://github.com/roc-lang/roc/blob/main/getting_started/macos_apple_silicon.md)
- [MacOS x86-64](https://github.com/roc-lang/roc/blob/main/getting_started/macos_x86_64.md)
- [Windows](https://github.com/roc-lang/roc/blob/main/getting_started/windows.md)
- [Other Systems](https://github.com/roc-lang/roc/blob/main/getting_started/other.md)

## Nightly Builds

<!-- direct download links to builds should go here -->

[nightly builds](https://github.com/roc-lang/roc/releases)

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

The Roc package manager is still a work in progress. In the meantime, you can use packages by including the bundle URL in your module header.

Below is an example of a Roc application which uses the [roc-lang/basic-cli](https://github.com/roc-lang/basic-cli) platform and the [lukewilliamboswell/roc-json](https://github.com/lukewilliamboswell/roc-json) package by including the bundle URLs in the module header.

```roc
app "hello"
    packages { 
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br",
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.1.0/xbO9bXdHi7E9ja6upN5EJXpDoYm7lwmJ8VzL7a5zhYE.tar.br",
    }
    imports [
        pf.Stdout,
        json.Core,
    ]
    provides [main] to pf
```

<!-- 

explain package manager design is still a work in progress 
the plan is to create a centralised index
- ergonomicly integration
- a fact that every language will have one

in the meantime you can use package URLs
-->

## Editor Support

<!--
explain that the high level design is a work in progress
- Design goals for editor - we want Roc to ship with an awesome editor
- Beginners learning to get up an running with an editor
- Want it to run really fast etc
-->

- Language Server
- Neo(Vim)
