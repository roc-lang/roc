# META
~~~ini
description=Import with exposing syntax test
type=file
~~~
# SOURCE
~~~roc
module [main]

import pf.Stdout exposing [line!, write!]

main = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:33),Comma(3:33-3:34),LowerIdent(3:35-3:41),CloseSquare(3:41-3:42),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Int(5:8-5:10),EndOfFile(5:10-5:10),
~~~
# PARSE
~~~clojure
(file @1.1-5.10
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13
				(text "main"))))
	(statements
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
	(def
		(pattern
			(p-assign @5.1-5.5 (ident "main")))
		(expr
			(e-int @5.8-5.10 (value "42"))))
	(s-import @3.1-3.42 (module "pf.Stdout") (qualifier "pf")
		(exposes
			(exposed-item (name "line!") (is_wildcard false))
			(exposed-item (name "write!") (is_wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Num(_size)")))
	(expressions
		(expr @5.8-5.10 (type "Num(_size)"))))
~~~
