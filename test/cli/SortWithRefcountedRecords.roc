SortWithRefcountedRecords := {}

# repro for https://github.com/roc-lang/roc/issues/12775
# Sorting records that hold a refcounted field (a string, or a function) with
# `List.sort_with` compiles and passes under every optimization level.
by_key = |x, y| if x.key < y.key Before else if x.key > y.key After else Same

expect {
	items = [{ key: 3, s: "c" }, { key: 1, s: "a" }, { key: 2, s: "b" }]
	sorted = List.sort_with(items, |x, y| if x.key < y.key Before else if x.key > y.key After else Same)
	List.map(sorted, |r| r.s) == ["a", "b", "c"]
}

expect {
	items = [{ key: 3, f: |n| n + 3 }, { key: 1, f: |n| n + 1 }, { key: 2, f: |n| n + 2 }]
	sorted = List.sort_with(items, by_key)
	List.map(sorted, |r| (r.f)(10)) == [11, 12, 13]
}
