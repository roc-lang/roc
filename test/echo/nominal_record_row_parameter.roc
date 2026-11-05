# Functions generic over the row variable of a nominal record, called with a
# record picked for that row: a bare row, fields plus a row, and a row given by
# a record alias.
Ref(r) := { ..r }

Labeled(r) := { label : Str, ..r }

Size : { w : F64, h : F64 }

kind_of : Ref(r) -> Str
kind_of = |_ref| "ref"

label_of : Labeled(r) -> Str
label_of = |item| item.label

main! = |_args| {
	item : Labeled({ id : U64 })
	item = Labeled.{ label: "tag", id: 7 }
	sized : Labeled(Size)
	sized = Labeled.{ label: "sized", w: 3, h: 4 }
	echo!("${kind_of(Ref.{ id: 7.U64 })} ${label_of(item)} ${label_of(sized)}")
	Ok({})
}
