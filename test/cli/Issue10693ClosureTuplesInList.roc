# Regression for https://github.com/roc-lang/roc/issues/10693: Boxy list
# operations must use the list's element descriptor when closures are erased.
Issue10693ClosureTuplesInList :: [].{
	render : { a : F64 } -> Str
	render = |row| {
		cols = [("a", |r| F64.to_str(r.a)), ("b", |r| F64.to_str(r.a))]
		Str.join_with(List.map(cols, |col| (col.1)(row)), ",")
	}

	render_with_headers : { a : F64 }, [Ef, Rpe] -> Str
	render_with_headers = |row, lens| {
		cols = match lens {
			Ef => [("a", |r| F64.to_str(r.a)), ("b", |r| F64.to_str(r.a))]
			Rpe => [("b", |r| F64.to_str(r.a))]
		}
		headers = List.map(cols, |col| col.0)
		cells = List.map(cols, |col| (col.1)(row))
		Str.join_with(List.concat(headers, cells), ",")
	}
}

expect Issue10693ClosureTuplesInList.render({ a: 1.0 }) == "1,1"
expect Issue10693ClosureTuplesInList.render_with_headers({ a: 1.0 }, Ef) == "a,b,1,1"
expect Issue10693ClosureTuplesInList.render_with_headers({ a: 1.0 }, Rpe) == "b,1"
