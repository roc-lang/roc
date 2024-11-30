app [main] {
  cli: "../basic-cli/platform/main.roc",
}

import cli.Stdout

main =
    # is this a valid statement?
    "Foo" |> A.x?

    # what about this?
    "Bar" |> B.y? 
        { config: "config" }

    C.z "Bar"

