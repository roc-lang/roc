NominalPlainRowHintFixes :: [].{}

# Each fix the reports in NominalPlainRowRejected.roc suggest, compiled and
# run: write the nominal type in the annotation, build a nominal record from
# a plain record's fields, and take a nominal record apart to build a plain
# one.

Pt := { x : U64 }

Packed := { x : U64, _ : U32 }

Only := { _ : U64 }

Color := [Red, Green]

pt : Pt
pt = Pt.{ x: 1 }

packed : Packed
packed = Packed.{ x: 2 }

plain : { x : U64 }
plain = { x: 3 }

# The annotation says `Pt` instead of the record type.
take_pt : Pt -> U64
take_pt = |Pt.{ x }| x

take_packed : Packed -> U64
take_packed = |Packed.{ x }| x

take_only_field : { k : Only } -> U64
take_only_field = |_| 7

take_color : Color -> U64
take_color = |c| match c {
	Red => 1
	Green => 2
}

take_plain : { x : U64 } -> U64
take_plain = |r| r.x

only_field : { k : Only }
only_field = { k: {} }

color : Color
color = Green

# Take a `Pt` apart and build a plain record from its fields.
to_plain : Pt -> { x : U64 }
to_plain = |Pt.{ x }| { x: x }

expect take_pt(pt) == 1

expect take_packed(packed) == 2

expect take_only_field(only_field) == 7

expect take_color(color) == 2

# Build a `Pt` from a plain record's fields.
expect take_pt(Pt.{ x: plain.x }) == 3

# Build a `Packed` from a plain record's fields; it provides the anonymous
# field.
expect take_packed(Packed.{ x: plain.x }) == 3

expect take_plain(to_plain(pt)) == 1
