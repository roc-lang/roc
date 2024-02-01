interface Records
    exposes [intVal]

intVal =
    foo = \{ x } -> x

    foo { x: 5 }
