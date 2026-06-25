app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/9801
#
# `roc check` accepts this program, but an optimized build (`--opt=size`, which
# runs the SpecConstr pass) crashed in spec_constr.zig's `collectCallPatterns`:
# the loop iterated a slice of `program.fns` captured before the loop, while
# materializing specialized callables during call-pattern collection appended to
# `program.fns`, reallocating the backing array and leaving the loop's slice
# dangling (panic "index out of bounds: index 2863311530" — 0xAAAAAAAA undefined
# memory — or a SIGSEGV). The build must not panic or segfault.

import pf.AnyValue exposing [AnyValue]
import pf.Stdout

Expr := [
	Ref(Box(U64)),
	ConstValue(Box(U64), Box(({} -> AnyValue)), Box((AnyValue, AnyValue -> Bool)), Box((AnyValue -> {}))),
	Map(Box(U64), Box(Expr), Box((AnyValue -> AnyValue)), Box((AnyValue, AnyValue -> Bool)), Box((AnyValue -> {}))),
]

Cell(a) := { expr : Box(Expr), tag : Box(AnyValue.TypeTag(a)) }.{
	from_expr : Expr, Box(AnyValue.TypeTag(a)) -> Cell(a)
	from_expr = |expr, tag| { expr: Box.box(expr), tag }

	to_expr : Cell(a) -> Box(Expr)
	to_expr = |cell| cell.expr

	const :
		a -> Cell(a)
			where [
				a.is_eq : a, a -> Bool,
			]
	const = |value| {
		token = Box.box(0)
		tag = AnyValue.new_tag({})
		init : {} -> AnyValue
		init = |_| AnyValue.store_tagged(Box.box(value), tag)
		eq : AnyValue, AnyValue -> Bool
		eq = |left_hv, right_hv| {
			left : a
			left = Box.unbox(AnyValue.get_tagged(left_hv, tag))
			right : a
			right = Box.unbox(AnyValue.get_tagged(right_hv, tag))
			left.is_eq(right)
		}
		drop : AnyValue -> {}
		drop = |host_value| {
			boxed : Box(a)
			boxed = AnyValue.take(host_value)
			_ = boxed
			{}
		}
		{ expr: Box.box(ConstValue(token, Box.box(init), Box.box(eq), Box.box(drop))), tag }
	}

	map :
		Cell(a), (a -> b) -> Cell(b)
			where [
				b.is_eq : b, b -> Bool,
			]
	map = |cell, f| {
		token = Box.box(0)
		output_tag = AnyValue.new_tag({})
		wrapped : AnyValue -> AnyValue
		wrapped = |input_hv| {
			typed_input : a
			typed_input = Box.unbox(AnyValue.get_tagged(input_hv, cell.tag))
			typed_output : b
			typed_output = f(typed_input)
			AnyValue.store_tagged(Box.box(typed_output), output_tag)
		}
		eq : AnyValue, AnyValue -> Bool
		eq = |left_hv, right_hv| {
			left : b
			left = Box.unbox(AnyValue.get_tagged(left_hv, output_tag))
			right : b
			right = Box.unbox(AnyValue.get_tagged(right_hv, output_tag))
			left.is_eq(right)
		}
		drop : AnyValue -> {}
		drop = |host_value| {
			boxed : Box(b)
			boxed = AnyValue.take(host_value)
			_ = boxed
			{}
		}
		{ expr: Box.box(Map(token, cell.expr, Box.box(wrapped), Box.box(eq), Box.box(drop))), tag: output_tag }
	}
}

Msg : {
	transform : Box((AnyValue, AnyValue -> AnyValue)),
}

Node(a) := { tag : Box(AnyValue.TypeTag(a)) }.{
	cell : Node(a) -> Cell(a)
	cell = |node| Cell.from_expr(Ref(Box.box(0)), node.tag)

	on_unit : Node(a), (a -> a) -> Msg
	on_unit = |node, f| {
		current_tag = node.tag
		wrapped : AnyValue, AnyValue -> AnyValue
		wrapped = |current_hv, _payload_hv| {
			current : a
			current = Box.unbox(AnyValue.get_tagged(current_hv, current_tag))
			next : a
			next = f(current)
			AnyValue.store_tagged(Box.box(next), current_tag)
		}
		{ transform: Box.box(wrapped) }
	}
}

Tree := [
	Branch({ children : List(Tree) }),
	Button({ label : Str, msg : Msg }),
	EachNode({ items : Box(Expr), items_to_values : Box((AnyValue -> List(AnyValue))), row : Box((AnyValue, AnyValue -> Tree)) }),
	StateNode({ initial : Box(({} -> AnyValue)), child : Box(Tree) }),
	Text(Str),
	TextCell({ cell : Box(Expr), read : Box((AnyValue -> Str)) }),
	WhenNode({ condition : Box(Expr), read : Box((AnyValue -> Bool)), when_true : Box(Tree), when_false : Box(Tree) }),
]

branch : List(Tree) -> Tree
branch = |children| Branch({ children: children })

paragraph : Str -> Tree
paragraph = |text| Text(text)

text_cell : Cell(Str) -> Tree
text_cell = |cell| {
	tag = cell.tag
	read : AnyValue -> Str
	read = |value| Box.unbox(AnyValue.get_tagged(value, tag))
	TextCell({ cell: Cell.to_expr(cell), read: Box.box(read) })
}

button : Str, Msg -> Tree
button = |label, msg| Button({ label, msg })

state :
	a, (Node(a) -> Tree) -> Tree
		where [
			a.is_eq : a, a -> Bool,
		]
state = |init, body| {
	tag = AnyValue.new_tag({})
	initial : {} -> AnyValue
	initial = |_| AnyValue.store_tagged(Box.box(init), tag)
	handle : Node(a)
	handle = { tag: tag }
	child = body(handle)
	StateNode({ initial: Box.box(initial), child: Box.box(child) })
}

when : Cell(Bool), ({} -> Tree), ({} -> Tree) -> Tree
when = |condition, when_true, when_false| {
	condition_tag = condition.tag
	read_condition : AnyValue -> Bool
	read_condition = |value| Box.unbox(AnyValue.get_tagged(value, condition_tag))
	WhenNode(
		{
			condition: Cell.to_expr(condition),
			read: Box.box(read_condition),
			when_true: Box.box(when_true({})),
			when_false: Box.box(when_false({})),
		},
	)
}

each :
	Cell(List(item)), (item -> key), (key, Cell(item) -> Tree) -> Tree
		where [
			item.is_eq : item, item -> Bool,
			key.is_eq : key, key -> Bool,
		]
each = |items, key_of, row| {
	items_tag = items.tag
	item_tag = AnyValue.new_tag({})

	items_to_values : AnyValue -> List(AnyValue)
	items_to_values = |items_hv| {
		typed_items : List(item)
		typed_items = Box.unbox(AnyValue.get_tagged(items_hv, items_tag))
		List.map(typed_items, |item| AnyValue.store_tagged(Box.box(item), item_tag))
	}

	row_hv : AnyValue, AnyValue -> Tree
	row_hv = |_key_hv, item_hv| {
		item : item
		item = Box.unbox(AnyValue.get_tagged(item_hv, item_tag))
		row_item : {} -> AnyValue
		row_item = |_| AnyValue.clone(item_hv)
		row_signal = Cell.from_expr(ConstValue(Box.box(0), Box.box(row_item), Box.box(|_, _| True), Box.box(|_| {})), item_tag)
		row(key_of(item), row_signal)
	}

	EachNode({ items: Cell.to_expr(items), items_to_values: Box.box(items_to_values), row: Box.box(row_hv) })
}

render : Tree -> Str
render = |tree| {
	match tree {
		Branch(record) =>
			List.fold(record.children, "", |acc, child| Str.concat(acc, render(child)))
		Button(record) => record.label
		EachNode(_) => "each"
		StateNode(record) => render(Box.unbox(record.child))
		Text(text) => text
		TextCell(_) => "text"
		WhenNode(record) => Str.concat(render(Box.unbox(record.when_true)), render(Box.unbox(record.when_false)))
	}
}

row : Str, Cell(Str) -> Tree
row = |label, _item| {
	state(
		1,
		|count| {
			count_text = Cell.map(count.cell(), |n| Str.concat(label, n.to_str()))
			branch(
				[
					paragraph(label),
					text_cell(count_text),
					button(Str.concat("Increase ", label), count.on_unit(|current| current + 1)),
				],
			)
		},
	)
}

main! = || {
	out =
		render(
			state(
				["first", "second"],
				|items| {
					is_ready = Cell.const(True)
					branch(
						[
							paragraph("Demo"),
							paragraph("This extra text keeps the surrounding constructor shape non-trivial."),
							when(
								is_ready,
								|_|
									branch(
										[
											each(items.cell(), |label| Str.concat("a-", label), |label, item| row(Str.concat("A ", label), item)),
											each(items.cell(), |label| Str.concat("b-", label), |label, item| row(Str.concat("B ", label), item)),
											each(items.cell(), |label| Str.concat("c-", label), |label, item| row(Str.concat("C ", label), item)),
											each(items.cell(), |label| Str.concat("d-", label), |label, item| row(Str.concat("D ", label), item)),
											each(items.cell(), |label| Str.concat("e-", label), |label, item| row(Str.concat("E ", label), item)),
										],
									),
								|_| paragraph("empty"),
							),
						],
					)
				},
			),
		)
	Stdout.line!(out)
	out
}
