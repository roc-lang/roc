app [main, Model] { pf: platform "platform/main.roc" }

import pf.Program exposing [Program]

Model : Str

main : Program Model
main = {
    init: \{} -> "Hello World",
    update: \model, new -> Str.concat(model, new),
    view: \model -> Str.concat(model, "!"),
}
