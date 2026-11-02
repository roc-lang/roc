ScalarReadThroughSpentPayload := {}

# A record taken out of a `Try` gives both of its list fields away, one to a
# call and one to `List.concat`, and a scalar inside the moved field is read
# afterwards: the record's own bytes still hold it.

Drawing : { height : U64, images : List(Str) }

validate : Drawing -> Try({ drawing : Drawing, images : List(Str) }, [Bad])
validate = |drawing| Ok({ drawing, images: drawing.images })

use_both : List(Drawing), List(Str), Drawing -> Try({ a : List(Drawing), b : List(Str), h : U64 }, [Bad])
use_both = |drawings, images, drawing| {
	validated = validate(drawing)?
	Ok({ a: drawings.append(validated.drawing), b: List.concat(images, validated.images), h: validated.drawing.height })
}

expect use_both([], ["z"], { height: 3, images: ["a", "b"] }).map_ok(|r| (r.a.len(), r.b, r.h)) == Ok((1, ["z", "a", "b"], 3))
