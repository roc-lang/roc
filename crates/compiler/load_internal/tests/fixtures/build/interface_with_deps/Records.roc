interface Records
    exposes [intVal]
    imports []

intVal =
    foo = \{ x } -> x

    foo { x: 5 }
