PolymorphicListReverseMod :: [].{
    reverse : List(a) -> List(a)
    reverse = |list| {
        destination = List.with_capacity(list.len())
        list.fold_rev(destination, |item, dest| dest.append(item))
    }
}

expect PolymorphicListReverseMod.reverse([2]) == [2]
