Sublist :: {}.{
	sublist : List(U8), List(U8) -> [Equal, Sublist, Superlist, Unequal]
	sublist = |list1, list2| {
		match list1.len().order_relative_to(list2.len()) {
			After => {
				match sublist(list2, list1) {
					Sublist => Superlist
					Unequal => Unequal
					Superlist => crash "unreachable: second list is shorter"
					Equal => crash "unreachable: list lengths differ"
				}
			}

			Same => {
				if list1 == list2 {
					Equal
				} else {
					Unequal
				}
			}

			Before => {
				length_diff = list2.len() - list1.len()
				maybe_equal_index =
					(0..=length_diff)
						.iter()
						.fold([], |acc, x| acc.append(x))
						.find_first(
							|start| {
								sl = list2.sublist({ start, len: list1.len() })
								sl == list1
							},
						)

				match maybe_equal_index {
					Ok(_) => Sublist
					Err(NotFound) => Unequal
				}
			}
		}
	}
}
