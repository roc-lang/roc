app "tui"
    packages { pf: "tui-platform/main.roc" }
    provides [main] { Model } to pf

import pf.Program exposing [Program]

Model : Str

main : Program Model
main = {
    init: \{} -> "Hello World",
    update: \model, new -> Str.concat model new,
    view: \model -> Str.concat model "!",
}
