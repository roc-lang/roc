app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
}

import cli.Stdout
import cli.Stdin

import Alias { passed: Stdin.line } as In
import Alias { passed: Stdout.line } as Out

main =
    Out.exposed! "Write something:"
    input = In.exposed!
    Out.exposed! input
