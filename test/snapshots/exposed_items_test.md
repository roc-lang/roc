# META
~~~ini
description=Import with exposing syntax test
type=snippet
~~~
# SOURCE
~~~roc
import pf.Stdout exposing [line!, write!]

main = 42
~~~
# EXPECTED
MODULE NOT FOUND - exposed_items_test.md:1:1:1:42
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**exposed_items_test.md:1:1:1:42:**
```roc
import pf.Stdout exposing [line!, write!]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:10),NoSpaceDotUpperIdent(1:10-1:17),KwExposing(1:18-1:26),OpenSquare(1:27-1:28),LowerIdent(1:28-1:33),Comma(1:33-1:34),LowerIdent(1:35-1:41),CloseSquare(1:41-1:42),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),Int(3:8-3:10),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.10
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.42 (raw "pf.Stdout")
			(exposing
				(exposed-lower-ident @1.28-1.33
					(text "line!"))
				(exposed-lower-ident @1.35-1.41
					(text "write!"))))
		(s-decl @3.1-3.10
			(p-ident @3.1-3.5 (raw "main"))
			(e-int @3.8-3.10 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "main"))
		(e-num @3.8-3.10 (value "42")))
	(s-import @1.1-1.42 (module "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "write!") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Num(_size)")))
	(expressions
		(expr @3.8-3.10 (type "Num(_size)"))))
~~~
