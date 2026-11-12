# Counter.roc does not import this module, so testing Counter.roc neither
# checks it nor runs this failing expect.
Unrelated := [].{
	value : U64
	value = 1
}

expect Unrelated.value == 2
