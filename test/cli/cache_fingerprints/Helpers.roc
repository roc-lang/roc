Helpers := [].{
	# Stores its borrowed list in a new list, so its code updates the
	# caller's reference count.
	helper : List(U8), U8 -> U64
	helper = |list, extra| {
		wrapped = [list, list]
		wrapped.len() + extra.to_u64() + list.len()
	}

	grow : List(U8), U8 -> List(U8)
	grow = |list, extra| list.append(extra).append(extra)

	# Calls builtin wrappers, which every program inlines.
	measure : List(U8) -> U64
	measure = |list| if list.is_empty() { 0 } else { list.len() + 1 }
}
