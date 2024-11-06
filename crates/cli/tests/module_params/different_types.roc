app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
}

import cli.Stdout
import cli.Stdin

import Alias { passed: Stdin.line } as In
import Alias { passed: Stdout.line } as Out

main =
    Out.exposed! "Write something:"
    input = In.exposed!
    Out.exposed! input
