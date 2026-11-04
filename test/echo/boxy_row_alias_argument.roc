# A record alias whose argument is the record's extension row, used in a
# function's parameter type and called with a wider concrete record.
Ext(r) : { z : I64, ..r }

f : Ext({ tag : Str, ..r }) -> Str
f = |x| "${x.tag}:${x.z.to_str()}"

main! = |_args| {
	rec : { z : I64, tag : Str, extra : U8 }
	rec = { z: 3, tag: "t", extra: 1 }
	echo!(f(rec))
	Ok({})
}
