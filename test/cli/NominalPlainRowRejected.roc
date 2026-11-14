NominalPlainRowRejected :: [].{}

# A nominal type is never the same as a plain record or tag union, even one
# with exactly its fields or tags, and a plain record or tag union an
# annotation writes never becomes a nominal type. Each mismatch below meets a
# committed plain row, directly or nested, in an argument, a result, an
# annotated binding, or a record field; the report says which nominal type it
# is, whether it has anonymous fields, and the fixes that hold.

Pt := { x : U64 }

Packed := { x : U64, _ : U32 }

Only := { _ : U64 }

Color := [Red, Green]

pt : Pt
pt = Pt.{ x: 1 }

packed : Packed
packed = Packed.{ x: 1 }

color : Color
color = Red

plain : { x : U64 }
plain = { x: 1 }

empty : {}
empty = {}

plain_color : [Red, Green]
plain_color = Red

take_plain : { x : U64 } -> U64
take_plain = |r| r.x

take_plain_list : List({ x : U64 }) -> U64
take_plain_list = |l| List.len(l)

take_wrapped : [Wrap({ x : U64 })] -> U64
take_wrapped = |Wrap(r)| r.x

take_unit_field : { k : {} } -> U64
take_unit_field = |_| 1

take_pt : Pt -> U64
take_pt = |Pt.{ x }| x

take_packed : Packed -> U64
take_packed = |Packed.{ x }| x

take_only : Only -> U64
take_only = |_| 1

take_plain_color : [Red, Green] -> U64
take_plain_color = |c| match c {
	Red => 1
	Green => 2
}

take_color : Color -> U64
take_color = |c| match c {
	Red => 1
	Green => 2
}

take_plain_colors : List([Red, Green]) -> U64
take_plain_colors = |l| List.len(l)

only_field : { k : Only }
only_field = { k: {} }

# A nominal type where a plain record is expected.
nominal_argument = take_plain(pt)

padded_argument = take_plain(packed)

nominal_in_list = take_plain_list([pt])

nominal_in_payload = take_wrapped(Wrap(pt))

padded_in_field = take_unit_field(only_field)

make : U64 -> { x : U64 }
make = |n| Pt.{ x: n }

annotated_binding : { x : U64 }
annotated_binding = pt

annotated_field : { inner : { x : U64 } }
annotated_field = { inner: pt }

# A plain record where a nominal type is expected.
plain_argument = take_pt(plain)

plain_into_padded = take_packed(plain)

empty_into_padded = take_only(empty)

# Tag unions, both ways.
nominal_tags_argument = take_plain_color(color)

nominal_tags_in_list = take_plain_colors([color])

plain_tags_argument = take_color(plain_color)
