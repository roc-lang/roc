# Methods of nominal types declared in function bodies that capture locals of
# those bodies, so checking keeps them as local procedures, called directly:
# through a method call, `==` and `!=`, from closures nested in the declaring
# body, and from a type declared in a nested function body. Every call must
# use the captured values of the declaring body. A capturing `to_inspect` is
# an ordinary method, so inspection renders the value's default form.

total_of : U64, List(U64) -> U64
total_of = |base, counts| {
	offset = base * 10
	Counter := { count : U64 }.{
		value = |c| c.count + offset
	}

	counters = counts.map(|n| Counter.{ count: n })
	values = counters.map(|c| c.value())
	read_first = || match counters.first() {
		Ok(first) => first.value()
		Err(_) => 0
	}
	List.sum(values) + read_first()
}

main! = |args| {
	extra = List.len(args)
	prefix = "n="
	Counter := { count : U64 }.{
		value = |c| c.count + extra
		describe = |c, suffix| "${prefix}${(c.count + extra).to_str()}${suffix}"
		to_inspect = |c| "Counter(${(c.count + extra).to_str()})"
	}

	slack = extra + 1
	Approx := { n : U64 }.{
		is_eq = |a, b| a.n <= b.n + slack and b.n <= a.n + slack
	}

	counter = Counter.{ count: 1 }
	echo!("${counter.value().to_str()} ${counter.describe("!")} ${counter.to_inspect()} ${Str.inspect(counter)}\n")

	near = Approx.{ n: 3 }
	far = Approx.{ n: 10 }
	echo!("${Str.inspect(near == Approx.{ n: 4 })} ${Str.inspect(near != far)} ${Str.inspect(near == far)}\n")

	add = |n| {
		bump = n + extra
		Step := { by : U64 }.{
			apply = |s, x| s.by + x + bump
		}

		twice = |x| Step.{ by: 1 }.apply(Step.{ by: 2 }.apply(x))
		twice(100)
	}
	echo!("${total_of(1, [1, 2, 3]).to_str()} ${add(5).to_str()}\n")
	Ok({})
}
