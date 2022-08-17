app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

Use has
    string : fmt -> Str | fmt has Use
    indirect : (fmt -> elem) -> (fmt -> elem) | fmt has Use

SomeUse := {} has [Use {string: stringUse, indirect: indirectUse}]

stringUse = \@SomeUse {} -> "ab"

indirectUse = \forcer -> \@SomeUse {} -> forcer (@SomeUse {})

# theUnwrapper = indirect string
theUnwrapper = indirectUse stringUse

main = theUnwrapper (@SomeUse {})
