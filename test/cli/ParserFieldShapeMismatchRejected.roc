ParserFieldShapeMismatchRejected :: [].{}

use_foo : Field({ foo : Str }) -> Str
use_foo = |field| Field.name(field)

use_bar : Field({ bar : Str }) -> Str
use_bar = |field| use_foo(field)

main : Str
main = ""
