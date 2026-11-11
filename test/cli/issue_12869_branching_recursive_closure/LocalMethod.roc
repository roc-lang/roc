# The structural `is_eq` of `{ key : Key }` reaches the method of `Key`, a type
# declared in `col`'s body that names `col`'s type variable, so `Key.is_eq`
# stays a local procedure and `inst` specializes in `col`'s context. Each
# branch has several recursive calls.
LocalMethod := [].{
	Inst(ctx) := [Inst((ctx, U8 -> Inst(ctx)))]

	inst : (ctx -> List(item)), List(item) -> Inst(ctx)
		where [item.is_eq : item, item -> Bool]
	inst = |read, items| Inst(
		|_env, n| match n {
			0 => {
				_c0 = LocalMethod.inst(read, items)
				_c1 = LocalMethod.inst(read, items)
				_c2 = LocalMethod.inst(read, items)
				_c3 = LocalMethod.inst(read, items)
				LocalMethod.inst(read, items)
			}
			_ => {
				_c0 = LocalMethod.inst(read, items)
				_c1 = LocalMethod.inst(read, items)
				_c2 = LocalMethod.inst(read, items)
				_c3 = LocalMethod.inst(read, items)
				LocalMethod.inst(read, items)
			}
		},
	)

	each : (ctx -> List(item)) -> (ctx -> Inst(ctx))
		where [item.is_eq : item, item -> Bool]
	each = |read| |env| LocalMethod.inst(read, read(env))

	col : (ctx -> List(Str)) -> (ctx -> Inst(ctx))
	col = |rows| {
		Key := { title : Str, seen : List(ctx) }.{
			is_eq : Key, Key -> Bool
			is_eq = |a, b| a.title == b.title
		}

		LocalMethod.each(|env| rows(env).map(|title| { key: Key.{ title, seen: [] } }))
	}

	view : ({} -> Inst({}))
	view = LocalMethod.col(|_| [])
}
