app "tui"
    packages { pf: "tui-platform/main.roc" }
    imports [pf.Program.{ Program }]
    provides [main] { Model } to pf

Model : Str

main : Program Model
main = {
    init: \{} -> "Hello World",
    update: \model, new -> Str.concat model new,
    view: \model -> Str.concat model "!",
}
