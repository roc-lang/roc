# Widening a stored const through a nominal type's arguments (Try's error
# row) used to panic monotype postcheck: the produced-value relation only
# reconciled the backing, so the narrow args reached row unification.
#   postcheck invariant violated: instantiation widened a closed tag union
ok : Try(U8, [OutOfRange])
ok = Ok(1)

use1 : Try(U8, [OutOfRange, Other])
use1 = ok

first_err! : List(Str) => Try(U8, [OutOfRange, Other])
first_err! = |args| if List.len(args) > 90 Err(Other) else use1

main! = |args| {
	match first_err!(args) {
		Ok(_) => Ok({})
		Err(_) => Err(Exit(1))
	}
}
