# A worker whose failure outcome leaves its list untouched, so its module
# publishes an outcome-restitution variant of it whatever its importers do.
Lib := [].{
    poke : List(U64), U64, U64 -> Try(List(U64), [OutOfBounds])
    poke = |list, index, value|
        if index < list.len() {
            Ok(list.set(index, value) ?? list)
        } else {
            Err(OutOfBounds)
        }
}
