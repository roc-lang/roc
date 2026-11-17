import Shared

# Opaque type: users of the type can not access its fields.
Secret :: {
	key : Str,
}.{
	new : Str -> Secret
	new = |k| { key: k }

	unlock : Secret, Str -> Str
	unlock = |secret, password| {
		if password == "open sesame" {
			Shared.label("The secret key is", secret.key)
		} else {
			"Wrong password!"
		}
	}
}
