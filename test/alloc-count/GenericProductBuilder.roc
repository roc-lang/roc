GenericProductBuilder := {}.{
    Builder(a) : { items : List(a), depths : List(U64) }
    
    push : Builder(a), a -> Try({ builder : Builder(a), id : U64 }, [Full])
    push = |builder, item| {
        { items, depths } = builder
        id = items.len()
        if id > 1000000000 {
            Err(Full)
        } else {
            Ok({ builder: { items: items.append(item), depths: depths.append(id) }, id })
        }
    }
}
