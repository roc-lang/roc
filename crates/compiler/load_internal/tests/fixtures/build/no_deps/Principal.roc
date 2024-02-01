interface Principal
    exposes [identity, intVal]

identity = \a -> a

intVal = identity "hi"
