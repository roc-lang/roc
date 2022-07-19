interface Principal
    exposes [identity, intVal]
    imports []

identity = \a -> a

intVal = identity "hi"
