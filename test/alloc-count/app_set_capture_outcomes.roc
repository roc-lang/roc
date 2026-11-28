app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

setter : List(Str), Str -> (U64 -> List(Str))
setter = |xs, replacement| |i| xs.set(i, replacement) ?? xs

probe! : Str, Str, U64, Bool => U64
probe! = |old, new, index, keep| {
	xs = List.repeat(old, 4096)
	call = setter(xs, new)
	(result, bytes) = if keep {
		before = Host.alloc_bytes!()
		changed = call(index)
		allocated = Host.alloc_bytes!() - before
		if xs.get(0) != Ok(old) {
			crash "changed shared list"
		}
		(changed, allocated)
	} else {
		before = Host.alloc_bytes!()
		changed = call(index)
		(changed, Host.alloc_bytes!() - before)
	}
	if result.len() != 4096 or result.get(0) != Ok(if (index == 0) new else old) {
		crash "incorrect checked update"
	}
	bytes
}

run! : Str => Str
run! = |input| {
	old = "${input}${input}"
	new = "${old}${input}"
	unique_hit = probe!(old, new, 0, Bool.False)
	unique_miss = probe!(old, new, 4096, Bool.False)
	shared_hit = probe!(old, new, 0, Bool.True)
	shared_miss = probe!(old, new, 4096, Bool.True)
	# The shared success must copy the list; its header size is the
	# target's, not part of this test's contract.
	copied = if (shared_hit >= 4096) "copied" else "not copied"
	"unique ${unique_hit.to_str()}/${unique_miss.to_str()} bytes, shared ${copied}/${shared_miss.to_str()} bytes"
}
