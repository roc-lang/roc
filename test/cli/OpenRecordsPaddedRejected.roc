OpenRecordsPaddedRejected :: [].{}

# A use may not turn a function's open record into a padded nominal type, one
# declared with unnamed fields, in any position. Its values are stored in
# declared order with padding, unlike the record, so the record would have to
# be rebuilt at every such use. Wanting this is an extremely rare edge case
# whose only benefit is slightly more concise code, and the compiler runs
# faster for not doing the extra work required to support it. Building the
# record as `Row.{ ... }`, or annotating a function that takes a `Row`, works.

Row := { columns : U64, _ : U32, values : U64 }.{
	total : Row -> U64
	total = |row| row.columns + row.values
}

Holder := {}.{
	take : Holder, Row -> U64
	take = |_, row| row.total()
}

holder : Holder
holder = {}

make = |c, v| { columns: c, values: v }

apply = |f| f({ columns: 1, values: 2 })

product = |{ columns, values }| columns * values

give = |h, n| h.take({ columns: n, values: n })

build = |xs| xs.map(|x| { columns: x, values: x })

row : Row
row = { columns: 3, values: 4 }

as_result : Row
as_result = make(1, 2)

as_callback = apply(Row.total)

as_parameter = product(row)

in_method = give(holder, 5)

in_list : List(Row)
in_list = build([1, 2])

explicit : Row
explicit = Row.{ columns: 6, values: 7 }

annotated_take : Row -> U64
annotated_take = |{ columns, values }| columns * values

expect explicit.total() == 13 and annotated_take(row) == 12
