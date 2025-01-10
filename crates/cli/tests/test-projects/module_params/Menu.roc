module { echo } -> [menu]

menu = \name ->
    indirect(name)

indirect = \name ->
    echo("Hi, $(name)!")
