app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# The loops of app_error_state_loop_called.roc, whose callee is also handed to
# a builtin. Cached builtin code calls it at the signature its types
# determine, and the app's own loops still call its outcome variant, so each
# loop allocates only as the list grows, with the object cache or without.
consume : List(U64), U64 -> Try(List(U64), [TooLong])
consume = |list, item| {
	a = item * 3 + 7
	b = a * a + item
	c = b % 11 + a % 13 + b % 17 + a % 19 + b % 23 + a % 29 + b % 31 + a % 37
	d = c * c + b * 3 + a * 5 + c % 41 + b % 43 + a % 47 + c % 53 + b % 59
	e = d * 7 + c * 11 + b * 13 + a * 17 + d % 61 + c % 67 + b % 71 + a % 73
	if list.len() > 1000000000 or e == 0 {
		Err(TooLong)
	} else {
		Ok(list.append(item + e - e))
	}
}

error_state_read_always : U64 -> U64
error_state_read_always = |n| {
	var $list = []
	var $failed = Bool.False
	var $index = 0
	while $index < n and !$failed {
		match consume($list, $index) {
			Ok(next) => {
				$list = next
			}
			Err(TooLong) => {
				$failed = Bool.True
			}
		}
		$index = $index + 1
	}
	$list.len()
}

error_state_read_on_success : U64 -> U64
error_state_read_on_success = |n| {
	var $list = []
	var $failed = Bool.False
	var $index = 0
	while $index < n and !$failed {
		match consume($list, $index) {
			Ok(next) => {
				$list = next
			}
			Err(TooLong) => {
				$failed = Bool.True
			}
		}
		$index = $index + 1
	}
	if $failed {
		0
	} else {
		$list.len()
	}
}

fallback : U64 -> U64
fallback = |n| {
	var $list = []
	var $index = 0
	while $index < n {
		$list = match consume($list, $index) {
			Ok(next) => next
			Err(TooLong) => $list
		}
		$index = $index + 1
	}
	$list.len()
}

early_return : U64 -> Try(U64, [TooLong])
early_return = |n| {
	var $list = []
	var $index = 0
	while $index < n {
		$list = consume($list, $index)?
		$index = $index + 1
	}
	Ok($list.len())
}

count! : (U64 -> U64), U64 => U64
count! = |f, n| {
	before = Host.alloc_count!()
	_ = f(n)
	Host.alloc_count!() - before
}

run! : Str => Str
run! = |_input| {
	n = 1024
	folded = [1, 2, 3].fold_try([], consume) ?? []
	counts = [
		count!(error_state_read_always, n),
		count!(error_state_read_on_success, n),
		count!(fallback, n),
		count!(|m| early_return(m) ?? 0, n),
	]
	# Growing a list to 1024 elements takes a handful of allocations; copying
	# it on every iteration takes one per element.
	linear = counts.all(|allocs| allocs < 64)
	shown = Str.join_with(counts.map(|allocs| allocs.to_str()), " ")
	"allocations: ${shown}, linear: ${if (linear) "yes" else "no"}, folded: ${folded.len().to_str()}"
}
