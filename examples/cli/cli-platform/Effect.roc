hosted Effect
    exposes [Effect, map, always, envVar]
    imports []
    generates Effect with [map, always]

envVar : Str -> Effect Str
