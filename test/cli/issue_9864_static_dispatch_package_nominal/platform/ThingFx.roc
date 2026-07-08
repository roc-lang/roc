import thing.Thing

ThingFx := [].{
    is_even : Thing.Thing -> Bool
    is_even = |thing| thing.value() % 2 == 0
}
