## Regression test for cross-module recursive nominal type resolution.
##
## The Elem type is a recursive nominal defined in another module:
##   Elem := [Div(List(Elem)), Text(Str)]
##
## Cross-module, the TypeId for Elem's children may contain `.rec`
## placeholder indirections (e.g. List(rec(0))) that differ from the
## canonical form (List(tag_union(N))) used by the importing module.
## The MIR lowerer's `remapMonotypeBetweenModules` resolves these so
## that polymorphic specialization and type comparisons work correctly.
app [main!] {
    pf: platform "./platform/main.roc",
    elem: "./elem_pkg/main.roc",
}

import pf.Stdout
import elem.Elem

main! = || {
    tree = Elem.div([])

    match tree {
        Div(_) => Stdout.line!("Div (correct)")
        Text(_) => Stdout.line!("Text (WRONG)")
    }
}
