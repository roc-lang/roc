app [main!] { pf: platform "../../fx-open/platform/main.roc" }
import pf.Stdout
import Generic

offset : U64
offset = {
    dbg "persistent interface observation"
    Generic.f7(2) + Generic.composite({}).value
}

unused : U64
unused = crash "unused failing interface root"

main! = |args| {
    total = Generic.f7(args.len().to_u64() + 10) + offset + Generic.composite({}).value
    Stdout.line!(total.to_str())
    Ok({})
}
