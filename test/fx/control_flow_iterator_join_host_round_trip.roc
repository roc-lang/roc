app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# Iterators the host holds come back as the ordinary public `Iter`: the reader
# names their type, and nothing in its body produced them. Each function below
# joins such an iterator with one its own body produces, in every control-flow
# form and in both orders, and iterates the result. `main!` runs each function
# over both seeds and both flags, so every branch of every join runs.

State : { items : Iter(U8), its : List(Iter(U8)), maybe : Try(Iter(U8), [None]), stream : Stream(U8) }

digits : Iter(U8) -> U64
digits = |items| {
	var $digits = 0
	for item in items {
		$digits = $digits * 10 + item.to_u64()
	}
	$digits
}

take! : () => State
take! = || {
	taken : Box(State)
	taken = Host.take_seed!()
	Box.unbox(taken)
}

seed! : Bool => {}
seed! = |full| {
	state : State
	state = if full {
		{
			items: [1.U8, 2].iter(),
			its: [[3.U8, 4].iter(), [5.U8].iter().map(|item| item + 1)],
			maybe: Ok([7.U8].iter()),
			stream: [1.U8, 2].iter().stream(),
		}
	} else {
		{
			items: [1.U8, 2].iter().map(|item| item + 2),
			its: [],
			maybe: Err(None),
			stream: [3.U8, 4].iter().stream(),
		}
	}
	Host.store_seed!(Box.box(state))
}

match_held_first! : Bool => U64
match_held_first! = |_use| {
	state = take!()
	digits(
		match state.its {
			[first, ..] => first
			_ => [9.U8].iter()
		},
	)
}

match_produced_first! : Bool => U64
match_produced_first! = |_use| {
	state = take!()
	digits(
		match state.its {
			[] => [9.U8].iter()
			[first, ..] => first
		},
	)
}

match_as_loop_iterable! : Bool => U64
match_as_loop_iterable! = |_use| {
	state = take!()
	var $digits = 0.U64
	for item in (
		match state.its {
			[first, ..] => first
			_ => [9.U8].iter()
		}
	) {
		$digits = $digits * 10 + item.to_u64()
	}
	$digits
}

match_guard! : Bool => U64
match_guard! = |use| {
	state = take!()
	digits(
		match state.its {
			[first, ..] if use => first
			[_, second, ..] => second
			_ => [9.U8].iter()
		},
	)
}

match_tuple! : Bool => U64
match_tuple! = |use| {
	state = take!()
	digits(
		match (use, state.its) {
			(Bool.True, [first, ..]) => first
			(Bool.False, _) => [9.U8].iter()
			_ => state.items
		},
	)
}

match_try_payload! : Bool => U64
match_try_payload! = |_use| {
	state = take!()
	digits(
		match state.maybe {
			Ok(held) => held
			Err(None) => [9.U8].iter().map(|item| item - 1)
		},
	)
}

try_default! : Bool => U64
try_default! = |_use| {
	state = take!()
	digits(state.maybe ?? [9.U8].iter())
}

if_held_first! : Bool => U64
if_held_first! = |use| {
	state = take!()
	held = match state.its {
		[first, ..] => first
		_ => state.items
	}
	digits(
		if use {
			held
		} else {
			[9.U8].iter()
		},
	)
}

if_produced_first! : Bool => U64
if_produced_first! = |use| {
	state = take!()
	held = match state.its {
		[first, ..] => first
		_ => state.items
	}
	digits(
		if !use {
			[9.U8].iter()
		} else {
			held
		},
	)
}

nested_held_inside! : Bool => U64
nested_held_inside! = |use| {
	state = take!()
	digits(
		if use {
			match state.its {
				[first, ..] => first
				_ => [8.U8].iter()
			}
		} else {
			[9.U8].iter().map(|item| item - 2)
		},
	)
}

nested_produced_inside! : Bool => U64
nested_produced_inside! = |use| {
	state = take!()
	digits(
		match state.its {
			[first, ..] => if use {
				first
			} else {
				[8.U8].iter()
			}
			_ => if use {
				[9.U8].iter().map(|item| item - 2)
			} else {
				state.items
			}
		},
	)
}

three_tiers! : Bool => U64
three_tiers! = |use| {
	state = take!()
	digits(
		match state.its {
			[first, ..] if use => first
			[] if use => [9.U8].iter()
			_ => [7.U8, 8].iter().map(|item| item + 1)
		},
	)
}

statement_block_branch! : Bool => U64
statement_block_branch! = |use| {
	state = take!()
	digits(
		match state.its {
			[first, ..] if use => first
			[_, second, ..] => second
			_ => {
				list = [9.U8]
				list.iter()
			}
		},
	)
}

adapter_over_join! : Bool => U64
adapter_over_join! = |_use| {
	state = take!()
	joined = match state.its {
		[first, ..] => first
		_ => [9.U8].iter()
	}
	digits(joined.map(|item| item - 1))
}

concat_of_joins! : Bool => U64
concat_of_joins! = |use| {
	state = take!()
	left = match state.maybe {
		Ok(held) => held
		Err(None) => [9.U8].iter()
	}
	right = match state.its {
		[first, ..] if use => first
		_ => [8.U8].iter()
	}
	digits(left.concat(right))
}

join_in_list! : Bool => U64
join_in_list! = |use| {
	state = take!()
	var $digits = 0.U64
	for held in (
		match state.its {
			[first, ..] if use => [first]
			_ => [[9.U8].iter()]
		}
	) {
		$digits = $digits * 100 + digits(held)
	}
	$digits
}

join_in_record! : Bool => U64
join_in_record! = |use| {
	state = take!()
	joined = match state.its {
		[first, ..] if use => { inner: first, count: 1.U64 }
		_ => { inner: [9.U8].iter(), count: 2.U64 }
	}
	digits(joined.inner) * 10 + joined.count
}

reassigned_in_branch! : Bool => U64
reassigned_in_branch! = |use| {
	state = take!()
	var $count = 0.U64
	joined = match state.its {
		[first, ..] if use => {
			$count = 1
			first
		}
		_ => {
			$count = 2
			[9.U8].iter()
		}
	}
	digits(joined) * 10 + $count
}

early_return_held! : Bool => U64
early_return_held! = |_use| {
	state = take!()
	pick = |its| {
		for held in its {
			return held
		}
		[9.U8].iter()
	}
	digits(pick(state.its))
}

early_return_produced! : Bool => U64
early_return_produced! = |use| {
	state = take!()
	pick = |its| {
		if !use {
			return [9.U8].iter()
		}
		match its {
			[first, ..] => first
			_ => [8.U8].iter().map(|item| item - 1)
		}
	}
	digits(pick(state.its))
}

early_return_two_produced! : Bool => U64
early_return_two_produced! = |use| {
	state = take!()
	pick = |its| {
		for held in its {
			if use {
				return held
			}
		}
		if use {
			return [9.U8].iter()
		}
		[7.U8, 8].iter().map(|item| item + 1)
	}
	digits(pick(state.its))
}

early_return_annotated : List(Iter(U8)), Bool -> Iter(U8)
early_return_annotated = |its, use| {
	for held in its {
		if use {
			return held
		}
	}
	[9.U8].iter()
}

early_return_top_level! : Bool => U64
early_return_top_level! = |use| digits(early_return_annotated(take!().its, use))

stream_join! : Bool => U64
stream_join! = |use| {
	state = take!()
	maybe : Try(Stream(U8), [None])
	maybe = if use {
		Ok(state.stream)
	} else {
		Err(None)
	}
	var $digits = 0.U64
	for! item in (
		match maybe {
			Ok(held) => held
			Err(None) => [9.U8].iter().stream()
		}
	) {
		$digits = $digits * 10 + item.to_u64()
	}
	$digits
}

Holder := { it : Iter(U8) }.{
	unwrap : Holder -> Iter(U8)
	unwrap = |Holder.{ it }| it
}

# The functions below construct, store and read back a nominal that holds an
# iterator in one body, so the value read back and the one a branch constructs
# are the same nominal with two representation witnesses. The last one reads
# the value back through another function instead.

take_holders! : () => List(Holder)
take_holders! = || {
	_state = take!()
	Host.store_seed!(Box.box([Holder.{ it: [1.U8, 2].iter() }]))
	taken : Box(List(Holder))
	taken = Host.take_seed!()
	Box.unbox(taken)
}

nominal_match! : Bool => U64
nominal_match! = |use| {
	_state = take!()
	Host.store_seed!(Box.box([Holder.{ it: [1.U8, 2].iter() }]))
	taken : Box(List(Holder))
	taken = Host.take_seed!()
	joined = match Box.unbox(taken) {
		[first, ..] if use => first
		_ => Holder.{ it: [9.U8].iter() }
	}
	digits(joined.unwrap())
}

nominal_nested! : Bool => U64
nominal_nested! = |use| {
	_state = take!()
	Host.store_seed!(Box.box([Holder.{ it: [1.U8, 2].iter() }]))
	taken : Box(List(Holder))
	taken = Host.take_seed!()
	joined = match Box.unbox(taken) {
		[] => Holder.{ it: [9.U8].iter() }
		[first, ..] => if use {
			first
		} else {
			Holder.{ it: [8.U8].iter().map(|item| item - 1) }
		}
	}
	digits(joined.unwrap())
}

nominal_early_return! : Bool => U64
nominal_early_return! = |use| {
	_state = take!()
	Host.store_seed!(Box.box([Holder.{ it: [1.U8, 2].iter() }]))
	taken : Box(List(Holder))
	taken = Host.take_seed!()
	pick = |holders| {
		for holder in holders {
			if use {
				return holder
			}
		}
		Holder.{ it: [9.U8].iter() }
	}
	digits(pick(Box.unbox(taken)).unwrap())
}

nominal_list_items! : Bool => U64
nominal_list_items! = |use| {
	_state = take!()
	Host.store_seed!(Box.box([Holder.{ it: [1.U8, 2].iter() }]))
	taken : Box(List(Holder))
	taken = Host.take_seed!()
	var $total = 0.U64
	match Box.unbox(taken) {
		[first, ..] if use => {
			for holder in [first, Holder.{ it: [9.U8].iter() }] {
				$total = $total * 100 + digits(holder.unwrap())
			}
		}
		_ => {}
	}
	$total
}

nominal_across_functions! : Bool => U64
nominal_across_functions! = |use| {
	joined = match take_holders!() {
		[first, ..] if use => first
		_ => Holder.{ it: [9.U8].iter() }
	}
	digits(joined.unwrap())
}

report! : Str, (Bool => U64) => {}
report! = |name, run!| {
	seed!(Bool.True)
	full_use = run!(Bool.True)
	seed!(Bool.True)
	full_skip = run!(Bool.False)
	seed!(Bool.False)
	empty_use = run!(Bool.True)
	seed!(Bool.False)
	empty_skip = run!(Bool.False)
	Stdout.line!("${name}: ${full_use.to_str()} ${full_skip.to_str()} ${empty_use.to_str()} ${empty_skip.to_str()}")
}

main! = || {
	report!("match held first", match_held_first!)
	report!("match produced first", match_produced_first!)
	report!("match as loop iterable", match_as_loop_iterable!)
	report!("match guard", match_guard!)
	report!("match tuple", match_tuple!)
	report!("match try payload", match_try_payload!)
	report!("try default", try_default!)
	report!("if held first", if_held_first!)
	report!("if produced first", if_produced_first!)
	report!("nested held inside", nested_held_inside!)
	report!("nested produced inside", nested_produced_inside!)
	report!("three tiers", three_tiers!)
	report!("statement block branch", statement_block_branch!)
	report!("adapter over join", adapter_over_join!)
	report!("concat of joins", concat_of_joins!)
	report!("join in list", join_in_list!)
	report!("join in record", join_in_record!)
	report!("reassigned in branch", reassigned_in_branch!)
	report!("early return held", early_return_held!)
	report!("early return produced", early_return_produced!)
	report!("early return two produced", early_return_two_produced!)
	report!("early return top level", early_return_top_level!)
	report!("stream join", stream_join!)
	report!("nominal match", nominal_match!)
	report!("nominal nested", nominal_nested!)
	report!("nominal early return", nominal_early_return!)
	report!("nominal list items", nominal_list_items!)
	report!("nominal across functions", nominal_across_functions!)
}
