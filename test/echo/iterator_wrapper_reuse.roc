# A deferred request for `inner` reuses the specialization that was lowered eagerly
# while lowering `outer`, and must compare against its evidence topology.
app [main!] {}

next_index = |state| {
	match List.get(state.bytes, state.offset) {
		Err(_) => Err(NoMore)
		Ok(_) => Ok((state.offset, { bytes: state.bytes, offset: state.offset + 1 }))
	}
}

inner : Str -> Iter(U64)
inner = |source| Iter.custom({ bytes: source.to_utf8(), offset: 0 }, Unknown, next_index)

outer : Str -> Iter(U64)
outer = |source| inner(source)

main! = |_| {
    var $sum = 0
    for index in outer("abc") {
        $sum = $sum + index
    }
    echo!("${Str.inspect($sum)}\n")
    Ok({})
}
