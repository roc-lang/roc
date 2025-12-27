app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Reproduction of issue #8765: Box.unbox loses type information when boxing without annotation
# When Box.box is called without a type annotation on the variable, the flex type variable
# for the record field's numeric type doesn't get resolved correctly, causing a layout mismatch.

Model : { count: I64 }
Handler : (Model -> [Update(Model), None])

main! = || {
    handler : Handler
    handler = |model| Update({ count: model.count + 1 })

    # Box WITHOUT type annotation - this should work but currently returns None due to the bug
    modelBox = Box.box({ count: 0 })
    m = Box.unbox(modelBox)
    result = handler(m)
    Stdout.line!(Str.inspect(result))
}
