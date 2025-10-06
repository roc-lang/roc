# META
~~~ini
description=Import with exposing syntax test
type=file:ExposedItemsTest.roc
~~~
# SOURCE
~~~roc
ExposedItemsTest := {}

import pf.Stdout exposing [line!, write!]

main = 42
~~~
# EXPECTED
MODULE NOT FOUND - exposed_items_test.md:3:1:3:42
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**exposed_items_test.md:3:1:3:42:**
```roc
import pf.Stdout exposing [line!, write!]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:17),OpColonEqual(1:18-1:20),OpenCurly(1:21-1:22),CloseCurly(1:22-1:23),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:33),Comma(3:33-3:34),LowerIdent(3:35-3:41),CloseSquare(3:41-3:42),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Int(5:8-5:10),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.10
	(type-module @1.1-1.17)
	(statements
		(s-type-decl @1.1-1.23
			(header @1.1-1.17 (name "ExposedItemsTest")
				(args))
			(ty-record @1.21-1.23))
		(s-import @3.1-3.42 (raw "pf.Stdout")
			(exposing
				(exposed-lower-ident @3.28-3.33
					(text "line!"))
				(exposed-lower-ident @3.35-3.41
					(text "write!"))))
		(s-decl @5.1-5.10
			(p-ident @5.1-5.5 (raw "main"))
			(e-int @5.8-5.10 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "main"))
		(e-num @5.8-5.10 (value "42")))
	(s-nominal-decl @1.1-1.23
		(ty-header @1.1-1.17 (name "ExposedItemsTest"))
		(ty-record @1.21-1.23))
	(s-import @3.1-3.42 (module "pf.Stdout") (qualifier "pf")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "write!") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-1.23 (type "ExposedItemsTest")
			(ty-header @1.1-1.17 (name "ExposedItemsTest"))))
	(expressions
		(expr @5.8-5.10 (type "Num(_size)"))))
~~~
