Hidden :: [].{
	Store :: { items : List(Str) }.{
		empty : Store
		empty = { items: [] }

		count : Store -> U64
		count = |store| store.items.len()
	}
}
