import Tree

Check := [].{
	probe : Tree -> Str
	probe = |tree| {
		ids = tree.items() ?? []
		var $seen = Dict.empty()
		for id in ids {
			$seen = $seen.insert(id, {})
		}
		Str.join_with(ids.map(|id| if ($seen.contains(id)) "T" else "F"), "")
	}
}
