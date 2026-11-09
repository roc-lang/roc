Public := [Doc(Public.Store)].{
	## Opaque attribute store.
	Store :: { items : List(Str) }.{
		empty : Store
		empty = { items: [] }

		count : Store -> U64
		count = |store| store.items.len()
	}

	empty : Store
	empty = Store.empty
}
