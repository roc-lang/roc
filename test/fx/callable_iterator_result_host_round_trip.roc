app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# A function whose result holds a produced iterator has that iterator's
# representation in its own type. Each function below moves such a function
# through a place that knows it only as a type variable (a `Box`, a list, a
# record, a tag, a tuple, a nominal, or a generic function), calls it on the far
# side, and iterates the result. The first group also sends the box through the
# host. `main!` runs each one with both flags.

digits : Iter(U8) -> U64
digits = |items| {
	var $digits = 0
	for item in items {
		$digits = $digits * 10 + item.to_u64()
	}
	$digits
}

Maker := { make : () -> Iter(U8) }.{
	run : Maker -> Iter(U8)
	run = |Maker.{ make }| make()
}

id : a -> a
id = |x| x

boxed_function! : Bool => U64
boxed_function! = |use| {
	stored : () -> Iter(U8)
	stored = || [1.U8, 2].iter()
	Host.store_seed!(Box.box(stored))
	taken : Box(() -> Iter(U8))
	taken = Host.take_seed!()
	_u = use
	digits(Box.unbox(taken)())
}

boxed_unannotated_function! : Bool => U64
boxed_unannotated_function! = |use| {
	Host.store_seed!(Box.box(|n| [1.U8, n].iter().map(|x| x + 1)))
	taken : Box(U8 -> Iter(U8))
	taken = Host.take_seed!()
	digits(
		Box.unbox(taken)(
			if use {
				2
			} else {
				3
			},
		),
	)
}

boxed_function_returning_an_adapter! : Bool => U64
boxed_function_returning_an_adapter! = |use| {
	n = if use {
		1.U8
	} else {
		2.U8
	}
	Host.store_seed!(Box.box(|| [1.U8, 2].iter().map(|x| x + n)))
	taken : Box(() -> Iter(U8))
	taken = Host.take_seed!()
	digits(Box.unbox(taken)().map(|x| x + 1))
}

boxed_function_capturing_an_iterator! : Bool => U64
boxed_function_capturing_an_iterator! = |use| {
	it = if use {
		[1.U8, 2].iter()
	} else {
		[5.U8].iter().map(|x| x + 1)
	}
	Host.store_seed!(Box.box(|| it))
	taken : Box(() -> Iter(U8))
	taken = Host.take_seed!()
	digits(Box.unbox(taken)())
}

boxed_function_taking_an_iterator! : Bool => U64
boxed_function_taking_an_iterator! = |use| {
	stored : Iter(U8) -> Iter(U8)
	stored = |it| it.map(|x| x + 1)
	Host.store_seed!(Box.box(stored))
	taken : Box(Iter(U8) -> Iter(U8))
	taken = Host.take_seed!()
	f = Box.unbox(taken)
	digits(
		f(
			if use {
				[1.U8, 2].iter()
			} else {
				[5.U8].iter().map(|x| x + 1)
			},
		),
	)
}

boxed_function_consuming_an_iterator! : Bool => U64
boxed_function_consuming_an_iterator! = |use| {
	stored : Iter(U8), U8 -> U64
	stored = |it, n| digits(it.map(|x| x + n))
	Host.store_seed!(Box.box(stored))
	taken : Box((Iter(U8), U8 -> U64))
	taken = Host.take_seed!()
	f = Box.unbox(taken)
	if use {
		f([1.U8, 2].iter(), 1)
	} else {
		f([5.U8].iter().map(|x| x + 1), 2)
	}
}

boxed_function_returning_a_record! : Bool => U64
boxed_function_returning_a_record! = |use| {
	Host.store_seed!(Box.box(|n| { it: [1.U8, n].iter(), n: 3.U64 }))
	taken : Box(U8 -> { it : Iter(U8), n : U64 })
	taken = Host.take_seed!()
	rec = Box.unbox(taken)(
		if use {
			4
		} else {
			5
		},
	)
	digits(rec.it) * 10 + rec.n
}

boxed_function_returning_a_tag! : Bool => U64
boxed_function_returning_a_tag! = |use| {
	Host.store_seed!(
		Box.box(
			|ok| if ok {
				Ok([1.U8, 2].iter())
			} else {
				Err(7.U64)
			},
		),
	)
	taken : Box(Bool -> Try(Iter(U8), U64))
	taken = Host.take_seed!()
	match Box.unbox(taken)(use) {
		Ok(it) => digits(it)
		Err(n) => n
	}
}

boxed_function_returning_a_function! : Bool => U64
boxed_function_returning_a_function! = |use| {
	Host.store_seed!(Box.box(|| || [1.U8, 2].iter()))
	taken : Box(() -> (() -> Iter(U8)))
	taken = Host.take_seed!()
	_u = use
	digits(Box.unbox(taken)()())
}

boxed_function_returning_nested_iterators! : Bool => U64
boxed_function_returning_nested_iterators! = |use| {
	Host.store_seed!(Box.box(|| [[1.U8, 2].iter(), [3.U8].iter()].iter()))
	taken : Box(() -> Iter(Iter(U8)))
	taken = Host.take_seed!()
	var $t = 0.U64
	for inner in Box.unbox(taken)() {
		if use {
			$t = $t * 100 + digits(inner)
		}
	}
	$t
}

boxed_function_returning_a_stream! : Bool => U64
boxed_function_returning_a_stream! = |use| {
	Host.store_seed!(Box.box(|| [1.U8, 2].iter().stream()))
	taken : Box(() -> Stream(U8))
	taken = Host.take_seed!()
	_u = use
	var $total = 0.U64
	for! x in Box.unbox(taken)() {
		$total = $total * 10 + x.to_u64()
	}
	$total
}

functions_in_a_boxed_record! : Bool => U64
functions_in_a_boxed_record! = |use| {
	Host.store_seed!(Box.box({ f: || [1.U8, 2].iter(), g: |it| digits(it) + 1 }))
	taken : Box({ f : () -> Iter(U8), g : Iter(U8) -> U64 })
	taken = Host.take_seed!()
	rec = Box.unbox(taken)
	_u = use
	(rec.g)((rec.f)())
}

function_in_a_boxed_tag! : Bool => U64
function_in_a_boxed_tag! = |use| {
	stored : [Make(() -> Iter(U8)), Fixed(Iter(U8))]
	stored = if use {
		Make(|| [1.U8, 2].iter())
	} else {
		Fixed([3.U8].iter().map(|x| x + 1))
	}
	Host.store_seed!(Box.box(stored))
	taken : Box([Make(() -> Iter(U8)), Fixed(Iter(U8))])
	taken = Host.take_seed!()
	match Box.unbox(taken) {
		Make(f) => digits(f())
		Fixed(it) => digits(it)
	}
}

held_function_joins_a_local_one! : Bool => U64
held_function_joins_a_local_one! = |use| {
	stored : List(() -> Iter(U8))
	stored = [|| [1.U8, 2].iter()]
	Host.store_seed!(Box.box(stored))
	taken : Box(List(() -> Iter(U8)))
	taken = Host.take_seed!()
	joined = match Box.unbox(taken) {
		[first, ..] if use => first
		_ => || [9.U8].iter()
	}
	digits(joined())
}

held_nominal_function_joins_a_local_one! : Bool => U64
held_nominal_function_joins_a_local_one! = |use| {
	Host.store_seed!(Box.box(Maker.{ make: || [1.U8, 2].iter() }))
	taken : Box(Maker)
	taken = Host.take_seed!()
	maker = if use {
		Box.unbox(taken)
	} else {
		Maker.{ make: || [9.U8].iter() }
	}
	digits(maker.run())
}

boxed_functions_in_a_boxed_list! : Bool => U64
boxed_functions_in_a_boxed_list! = |use| {
	Host.store_seed!(Box.box([Box.box(|| [1.U8, 2].iter()), Box.box(|| [3.U8].iter().map(|x| x + 1))]))
	taken : Box(List(Box(() -> Iter(U8))))
	taken = Host.take_seed!()
	var $total = 0.U64
	for b in Box.unbox(taken) {
		if use {
			$total = $total * 100 + digits(Box.unbox(b)())
		}
	}
	$total
}

local_box! : Bool => U64
local_box! = |use| {
	n = if use {
		1.U8
	} else {
		5.U8
	}
	g = Box.unbox(Box.box(|| [n, 2].iter()))
	digits(g())
}

unboxed_function_joins_a_local_one! : Bool => U64
unboxed_function_joins_a_local_one! = |use| {
	g = Box.unbox(Box.box(|| [1.U8, 2].iter()))
	h = if use {
		g
	} else {
		|| [9.U8].iter().map(|x| x - 1)
	}
	digits(h())
}

two_boxes_join! : Bool => U64
two_boxes_join! = |use| {
	b = if use {
		Box.box(|| [1.U8, 2].iter())
	} else {
		Box.box(|| [9.U8].iter().map(|x| x - 1))
	}
	digits(Box.unbox(b)())
}

local_box_returning_a_chain! : Bool => U64
local_box_returning_a_chain! = |use| {
	g = Box.unbox(Box.box(|| [1.U8, 2].iter().map(|x| x + 1).concat([9.U8].iter()).keep_if(|x| x > 2)))
	_u = use
	digits(g())
}

local_box_returning_a_function! : Bool => U64
local_box_returning_a_function! = |use| {
	g = Box.unbox(Box.box(|| || [1.U8, 2].iter()))
	_u = use
	digits(g()())
}

local_box_taking_an_iterator! : Bool => U64
local_box_taking_an_iterator! = |use| {
	g = Box.unbox(Box.box(|it, n| it.map(|x| x + n)))
	digits(
		g(
			[1.U8, 2].iter(),
			if use {
				1
			} else {
				2
			},
		),
	)
}

local_box_returning_a_stream! : Bool => U64
local_box_returning_a_stream! = |use| {
	g = Box.unbox(Box.box(|| [1.U8, 2].iter().stream()))
	_u = use
	var $s = 0.U64
	for! x in g() {
		$s = $s * 10 + x.to_u64()
	}
	$s
}

list_of_functions! : Bool => U64
list_of_functions! = |use| {
	fs = [|| [1.U8, 2].iter(), || [3.U8].iter().map(|x| x + 1)]
	var $s = 0.U64
	for f in fs {
		if use {
			$s = $s * 100 + digits(f())
		}
	}
	$s
}

list_pattern_of_functions! : Bool => U64
list_pattern_of_functions! = |use| {
	fs = [|| [1.U8, 2].iter(), || [3.U8].iter().map(|x| x + 1)]
	match fs {
		[a, b] => digits(
			(
				if use {
					a
				} else {
					b
				}
			)(),
		)
		_ => 0
	}
}

mapped_list_of_functions! : Bool => U64
mapped_list_of_functions! = |use| {
	fs = [1.U8, 2].map(|n| || [n, n + 1].iter())
	var $s = 0.U64
	for f in fs {
		if use {
			$s = $s * 100 + digits(f())
		}
	}
	$s
}

iterator_of_functions! : Bool => U64
iterator_of_functions! = |use| {
	var $s = 0.U64
	for f in [|| [1.U8, 2].iter(), || [3.U8].iter()].iter() {
		if use {
			$s = $s * 100 + digits(f())
		}
	}
	$s
}

function_through_a_generic! : Bool => U64
function_through_a_generic! = |use| {
	f = id(|| [1.U8, 2].iter())
	_u = use
	digits(f())
}

record_of_a_function_through_a_generic! : Bool => U64
record_of_a_function_through_a_generic! = |use| {
	rec = id({ f: || [1.U8, 2].iter(), n: 3.U64 })
	_u = use
	digits((rec.f)()) * 10 + rec.n
}

tuple_of_a_function_through_a_generic! : Bool => U64
tuple_of_a_function_through_a_generic! = |use| {
	t = id((|| [1.U8, 2].iter(), 3.U64))
	_u = use
	digits((t.0)()) * 10 + t.1
}

tag_of_a_function_through_a_generic! : Bool => U64
tag_of_a_function_through_a_generic! = |use| {
	t = if use {
		Ok(|| [1.U8, 2].iter())
	} else {
		Err(5.U64)
	}
	match id(t) {
		Ok(f) => digits(f())
		Err(n) => n
	}
}

nominal_of_a_function_through_a_generic! : Bool => U64
nominal_of_a_function_through_a_generic! = |use| {
	m = id(Maker.{ make: || [1.U8, 2].iter() })
	_u = use
	digits(m.run())
}

function_taking_a_function! : Bool => U64
function_taking_a_function! = |use| {
	g = id(|f| digits(f()))
	_u = use
	g(|| [1.U8, 2].iter())
}

function_default! : Bool => U64
function_default! = |use| {
	r : Try(() -> Iter(U8), Str)
	r = if use {
		Ok(|| [1.U8, 2].iter())
	} else {
		Err("x")
	}
	f = r ?? (|| [9.U8].iter())
	digits(f())
}

report! : Str, (Bool => U64) => {}
report! = |name, run!| {
	used = run!(Bool.True)
	skipped = run!(Bool.False)
	Stdout.line!("${name}: ${used.to_str()} ${skipped.to_str()}")
}

main! = || {
	report!("boxed function", boxed_function!)
	report!("boxed unannotated function", boxed_unannotated_function!)
	report!("boxed function returning an adapter", boxed_function_returning_an_adapter!)
	report!("boxed function capturing an iterator", boxed_function_capturing_an_iterator!)
	report!("boxed function taking an iterator", boxed_function_taking_an_iterator!)
	report!("boxed function consuming an iterator", boxed_function_consuming_an_iterator!)
	report!("boxed function returning a record", boxed_function_returning_a_record!)
	report!("boxed function returning a tag", boxed_function_returning_a_tag!)
	report!("boxed function returning a function", boxed_function_returning_a_function!)
	report!("boxed function returning nested iterators", boxed_function_returning_nested_iterators!)
	report!("boxed function returning a stream", boxed_function_returning_a_stream!)
	report!("functions in a boxed record", functions_in_a_boxed_record!)
	report!("function in a boxed tag", function_in_a_boxed_tag!)
	report!("held function joins a local one", held_function_joins_a_local_one!)
	report!("held nominal function joins a local one", held_nominal_function_joins_a_local_one!)
	report!("boxed functions in a boxed list", boxed_functions_in_a_boxed_list!)
	report!("local box", local_box!)
	report!("unboxed function joins a local one", unboxed_function_joins_a_local_one!)
	report!("two boxes join", two_boxes_join!)
	report!("local box returning a chain", local_box_returning_a_chain!)
	report!("local box returning a function", local_box_returning_a_function!)
	report!("local box taking an iterator", local_box_taking_an_iterator!)
	report!("local box returning a stream", local_box_returning_a_stream!)
	report!("list of functions", list_of_functions!)
	report!("list pattern of functions", list_pattern_of_functions!)
	report!("mapped list of functions", mapped_list_of_functions!)
	report!("iterator of functions", iterator_of_functions!)
	report!("function through a generic", function_through_a_generic!)
	report!("record of a function through a generic", record_of_a_function_through_a_generic!)
	report!("tuple of a function through a generic", tuple_of_a_function_through_a_generic!)
	report!("tag of a function through a generic", tag_of_a_function_through_a_generic!)
	report!("nominal of a function through a generic", nominal_of_a_function_through_a_generic!)
	report!("function taking a function", function_taking_a_function!)
	report!("function default", function_default!)
}
