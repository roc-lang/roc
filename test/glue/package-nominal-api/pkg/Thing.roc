Thing :: {
    name : Str,
}.{
    new : Str -> Thing
    new = |name| { name: name }

    name : Thing -> Str
    name = |thing| thing.name
}
