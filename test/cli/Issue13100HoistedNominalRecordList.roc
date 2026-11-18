# https://github.com/roc-lang/roc/issues/13100
# Hoisted empty lists retain their nominal element type too.
Id := { bits : U128 }

add : List({ ids : List(Id) }) -> List({ ids : List(Id) })
add = |defs| defs.prepend({ ids: [] })

expect add([]).len() == 1
