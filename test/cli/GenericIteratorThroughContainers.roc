GenericIteratorThroughContainers :: [].{}

## Unannotated generic helpers pass an iterator through a list literal, a tag
## and a capturing closure. Each helper is specialized at the iterator's
## generated representation, while its list item, tag payload and closure
## types are instantiated at the public `Iter` type. In the list helper one
## branch returns the list's item and the other the iterator itself.

via_list = |it| match [it].first() {
	Ok(x) => x
	Err(_) => it
}

via_tag = |it| match Wrap(it) {
	Wrap(x) => x
}

via_closure = |it| {
	get = |_| it
	get({})
}

count = |it| {
	var $n = 0
	for _ in it {
		$n = $n + 1
	}
	$n
}

expect count(via_list([1, 2, 3].iter())) == 3
expect count(via_tag([1, 2, 3].iter())) == 3
expect count(via_closure([1, 2, 3].iter())) == 3
