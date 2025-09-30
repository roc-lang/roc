# META
~~~ini
description=Type mismatch showing nominal type origin from different module
type=file
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
MISSING MAIN! FUNCTION - nominal_type_origin_mismatch.md:1:1:8:34
MODULE NOT FOUND - nominal_type_origin_mismatch.md:1:1:1:30
UNDECLARED TYPE - nominal_type_origin_mismatch.md:3:17:3:23
UNUSED VARIABLE - nominal_type_origin_mismatch.md:4:18:4:19
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**nominal_type_origin_mismatch.md:1:1:8:34:**
```roc
import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
```


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
KwImport(1:1-1:7),UpperIdent(1:8-1:12),KwExposing(1:13-1:21),OpenSquare(1:22-1:23),UpperIdent(1:23-1:29),CloseSquare(1:29-1:30),
LowerIdent(3:1-3:14),OpColon(3:15-3:16),UpperIdent(3:17-3:23),OpArrow(3:24-3:26),UpperIdent(3:27-3:30),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpBar(4:17-4:18),LowerIdent(4:18-4:19),OpBar(4:19-4:20),StringStart(4:21-4:22),StringPart(4:22-4:34),StringEnd(4:34-4:35),
LowerIdent(6:1-6:5),OpAssign(6:6-6:7),
LowerIdent(8:5-8:18),NoSpaceOpenRound(8:18-8:19),StringStart(8:19-8:20),StringPart(8:20-8:32),StringEnd(8:32-8:33),CloseRound(8:33-8:34),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.34
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.30 (raw "Data")
			(exposing
				(exposed-upper-ident @1.23-1.29 (text "Person"))))
		(s-type-anno @3.1-3.30 (name "expectsPerson")
			(ty-fn @3.17-3.30
				(ty @3.17-3.23 (name "Person"))
				(ty @3.27-3.30 (name "Str"))))
		(s-decl @4.1-4.35
			(p-ident @4.1-4.14 (raw "expectsPerson"))
			(e-lambda @4.17-4.35
				(args
					(p-ident @4.18-4.19 (raw "p")))
				(e-string @4.21-4.35
					(e-string-part @4.22-4.34 (raw "Got a person")))))
		(s-decl @6.1-8.34
			(p-ident @6.1-6.5 (raw "main"))
			(e-apply @8.5-8.34
				(e-ident @8.5-8.18 (raw "expectsPerson"))
				(e-string @8.19-8.33
					(e-string-part @8.20-8.32 (raw "not a person")))))))
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
		(p-assign @4.1-4.14 (ident "expectsPerson"))
		(e-lambda @4.17-4.35
			(args
				(p-assign @4.18-4.19 (ident "p")))
			(e-string @4.21-4.35
				(e-literal @4.22-4.34 (string "Got a person"))))
		(annotation @4.1-4.14
			(declared-type
				(ty-fn @3.17-3.30 (effectful false)
					(ty @3.17-3.23 (name "Person"))
					(ty @3.27-3.30 (name "Str"))))))
	(d-let
		(p-assign @6.1-6.5 (ident "main"))
		(e-call @8.5-8.34
			(e-lookup-local @8.5-8.18
				(p-assign @4.1-4.14 (ident "expectsPerson")))
			(e-string @8.19-8.33
				(e-literal @8.20-8.32 (string "not a person")))))
	(s-import @1.1-1.30 (module "Data")
		(exposes
			(exposed (name "Person") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.14 (type "Error -> Str"))
		(patt @6.1-6.5 (type "Str")))
	(expressions
		(expr @4.17-4.35 (type "Error -> Str"))
		(expr @8.5-8.34 (type "Str"))))
~~~
