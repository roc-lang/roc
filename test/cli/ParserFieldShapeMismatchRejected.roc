ParserFieldShapeMismatchRejected :: [].{}

use_foo : Str.FieldName({ foo : Str }) -> Str
use_foo = |field| Str.FieldName.name(field)

use_bar : Str.FieldName({ bar : Str }) -> Str
use_bar = |field| use_foo(field)

main : Str
main = ""
