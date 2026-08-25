DispatchOnUnannotatedErrorBodyMethod := [Yes].{
	# Unannotated, and the body does not canonicalize: `Bool.true` does not
	# exist (the tag is `Bool.True`). The reported diagnostic must be that
	# missing associated value, even though the method is then dispatched on
	# below.
	is_yes = |_| Bool.true
}

out = DispatchOnUnannotatedErrorBodyMethod.(Yes).is_yes()
