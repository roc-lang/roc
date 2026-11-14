Types := [].{
	## Owns an opaque box next to values the host can release.
	View(m) := { title : Str, model : Box(m), tags : List(Str) }

	## Owns nothing but an opaque box.
	Handle(m) := { state : Box(m), count : U64 }

	## Holds the application's type by value, so only a box or list of it can
	## cross the host boundary.
	Wrap(m) := { model : m, n : U64 }

	## Owns an opaque box in one variant.
	Slot(m) := [Empty, Full(Box(m)), Named(Str)]

	## Never mentions the application's type.
	Labeled := { label : Str, count : U64 }
}
