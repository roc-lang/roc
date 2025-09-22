# META
~~~ini
description=Type mismatch showing nominal type origin from different module
type=file
~~~
# SOURCE
~~~roc
module []

import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
~~~
# EXPECTED
MODULE NOT FOUND - nominal_type_origin_mismatch.md:3:1:3:30
UNDECLARED TYPE - nominal_type_origin_mismatch.md:5:17:5:23
UNUSED VARIABLE - nominal_type_origin_mismatch.md:6:18:6:19
# PROBLEMS
**MODULE NOT FOUND**
The module `Data` was not found in this Roc project.

You're attempting to use this module here:
**nominal_type_origin_mismatch.md:3:1:3:30:**
```roc
import Data exposing [Person]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Person_ is not declared in this scope.

This type is referenced here:
**nominal_type_origin_mismatch.md:5:17:5:23:**
```roc
expectsPerson : Person -> Str
```
                ^^^^^^


**UNUSED VARIABLE**
Variable `p` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_p` to suppress this warning.
The unused variable is declared here:
**nominal_type_origin_mismatch.md:6:18:6:19:**
```roc
expectsPerson = |p| "Got a person"
```
                 ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),UpperIdent(3:8-3:12),KwExposing(3:13-3:21),OpenSquare(3:22-3:23),UpperIdent(3:23-3:29),CloseSquare(3:29-3:30),
LowerIdent(5:1-5:14),OpColon(5:15-5:16),UpperIdent(5:17-5:23),OpArrow(5:24-5:26),UpperIdent(5:27-5:30),
LowerIdent(6:1-6:14),OpAssign(6:15-6:16),OpBar(6:17-6:18),LowerIdent(6:18-6:19),OpBar(6:19-6:20),StringStart(6:21-6:22),StringPart(6:22-6:34),StringEnd(6:34-6:35),
LowerIdent(8:1-8:5),OpAssign(8:6-8:7),
LowerIdent(10:5-10:18),NoSpaceOpenRound(10:18-10:19),StringStart(10:19-10:20),StringPart(10:20-10:32),StringEnd(10:32-10:33),CloseRound(10:33-10:34),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.34
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.30 (raw "Data")
			(exposing
				(exposed-upper-ident @3.23-3.29 (text "Person"))))
		(s-type-anno @5.1-5.30 (name "expectsPerson")
			(ty-fn @5.17-5.30
				(ty @5.17-5.23 (name "Person"))
				(ty @5.27-5.30 (name "Str"))))
		(s-decl @6.1-6.35
			(p-ident @6.1-6.14 (raw "expectsPerson"))
			(e-lambda @6.17-6.35
				(args
					(p-ident @6.18-6.19 (raw "p")))
				(e-string @6.21-6.35
					(e-string-part @6.22-6.34 (raw "Got a person")))))
		(s-decl @8.1-10.34
			(p-ident @8.1-8.5 (raw "main"))
			(e-apply @10.5-10.34
				(e-ident @10.5-10.18 (raw "expectsPerson"))
				(e-string @10.19-10.33
					(e-string-part @10.20-10.32 (raw "not a person")))))))
~~~
# FORMATTED
~~~roc
module []

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
		(p-assign @3.1-3.30 (ident "Person"))
		(e-lookup-external @3.1-3.30
			(module-idx "0")
			(target-node-idx "0")))
	(d-let
		(p-assign @6.1-6.14 (ident "expectsPerson"))
		(e-lambda @6.17-6.35
			(args
				(p-assign @6.18-6.19 (ident "p")))
			(e-string @6.21-6.35
				(e-literal @6.22-6.34 (string "Got a person"))))
		(annotation @6.1-6.14
			(declared-type
				(ty-fn @5.17-5.30 (effectful false)
					(ty @5.17-5.23 (name "Person"))
					(ty @5.27-5.30 (name "Str"))))))
	(d-let
		(p-assign @8.1-8.5 (ident "main"))
		(e-call @10.5-10.34
			(e-lookup-local @10.5-10.18
				(p-assign @6.1-6.14 (ident "expectsPerson")))
			(e-string @10.19-10.33
				(e-literal @10.20-10.32 (string "not a person")))))
	(s-import @3.1-3.30 (module "Data")
		(exposes
			(exposed (name "Person") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.30 (type "Error"))
		(patt @6.1-6.14 (type "Error -> Str"))
		(patt @8.1-8.5 (type "Str")))
	(expressions
		(expr @3.1-3.30 (type "Error"))
		(expr @6.17-6.35 (type "Error -> Str"))
		(expr @10.5-10.34 (type "Str"))))
~~~
