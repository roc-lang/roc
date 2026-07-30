ParserFieldShapeMismatchRejected :: [].{}

use_foo : Encoding.FieldName({ foo : Str }) -> Str
use_foo = |field| Encoding.FieldName.name(field)

use_bar : Encoding.FieldName({ bar : Str }) -> Str
use_bar = |field| use_foo(field)

main : Str
main = ""
