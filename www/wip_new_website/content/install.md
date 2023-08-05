# Install Roc

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
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0/DI4lqn7LIZs8ZrCDUgLK-tHHpQmxGF1ZrlevRKq5LXk.tar.br",
        # json package
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.1.0/xbO9bXdHi7E9ja6upN5EJXpDoYm7lwmJ8VzL7a5zhYE.tar.br",
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
