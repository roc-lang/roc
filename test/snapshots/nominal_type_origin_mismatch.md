# META
~~~ini
description=Type mismatch showing nominal type origin from different module
type=snippet
~~~
# SOURCE
~~~roc
import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
~~~
# EXPECTED
MODULE NOT FOUND - nominal_type_origin_mismatch.md:1:1:1:30
UNDECLARED TYPE - nominal_type_origin_mismatch.md:3:17:3:23
UNUSED VARIABLE - nominal_type_origin_mismatch.md:4:18:4:19
# PROBLEMS
**MODULE NOT FOUND**
The module `Data` was not found in this Roc project.

You're attempting to use this module here:
**nominal_type_origin_mismatch.md:1:1:1:30:**
```roc
import Data exposing [Person]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Person_ is not declared in this scope.

This type is referenced here:
**nominal_type_origin_mismatch.md:3:17:3:23:**
```roc
expectsPerson : Person -> Str
```
                ^^^^^^


**UNUSED VARIABLE**
Variable `p` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_p` to suppress this warning.
The unused variable is declared here:
**nominal_type_origin_mismatch.md:4:18:4:19:**
```roc
expectsPerson = |p| "Got a person"
```
                 ^


# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "Data")
			(exposing
				(exposed-upper-ident (text "Person"))))
		(s-type-anno (name "expectsPerson")
			(ty-fn
				(ty (name "Person"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "expectsPerson"))
			(e-lambda
				(args
					(p-ident (raw "p")))
				(e-string
					(e-string-part (raw "Got a person")))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "expectsPerson"))
				(e-string
					(e-string-part (raw "not a person")))))))
~~~
# FORMATTED
~~~roc
import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main = 
# This will cause a type mismatch
	expectsPerson("not a person")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "expectsPerson"))
		(e-lambda
			(args
				(p-assign (ident "p")))
			(e-string
				(e-literal (string "Got a person"))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "main"))
		(e-call
			(e-lookup-local
				(p-assign (ident "expectsPerson")))
			(e-string
				(e-literal (string "not a person")))))
	(s-import (module "Data")
		(exposes
			(exposed (name "Person") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Error"))))
~~~
