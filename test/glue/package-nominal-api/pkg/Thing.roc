Thing :: {
    name : Str,
}.{
    new : Str -> Thing
    new = |text| { name: text }

    name : Thing -> Str
    name = |thing| thing.name
}
