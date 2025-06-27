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
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:33),Comma(3:33-3:34),LowerIdent(3:35-3:41),CloseSquare(3:41-3:42),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Int(5:8-5:10),EndOfFile(5:10-5:10),
~~~
# PARSE
~~~clojure
(file @1-1-5-10
	(module @1-1-1-14
		(exposes @1-8-1-14
			(exposed-lower-ident (text "main"))))
	(statements
		(s-import @3-1-3-42 (module ".Stdout") (qualifier "pf")
			(exposing
				(exposed-lower-ident (text "line!"))
				(exposed-lower-ident (text "write!"))))
		(s-decl @5-1-5-10
			(p-ident @5-1-5-5 (raw "main"))
			(e-int @5-8-5-10 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 78)
		(p-assign @5-1-5-5 (ident "main") (id 75))
		(e-int @5-8-5-10 (value "42") (id 77)))
	(s-import @3-1-3-42 (module "pf.Stdout") (id 74)
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "write!") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "main") (type "Num(*)")))
	(expressions
		(expr @5-8-5-10 (type "Num(*)"))))
~~~