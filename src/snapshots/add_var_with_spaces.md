# META
~~~ini
description=Add a variable with spaces
type=file
~~~
# SOURCE
~~~roc
module [add2]

add2 = x +      2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),LowerIdent(3:8-3:9),OpPlus(3:10-3:11),Int(3:17-3:18),EndOfFile(3:18-3:18),
~~~
# PARSE
~~~clojure
(file @1-1-3-18
	(module @1-1-1-14
		(exposes @1-8-1-14
			(exposed-lower-ident (text "add2"))))
	(statements
		(s-decl @3-1-3-18
			(p-ident @3-1-3-5 (raw "add2"))
			(e-binop @3-8-3-18 (op "+")
				(e-ident @3-8-3-9 (qaul "") (raw "x"))
				(e-int @3-17-3-18 (raw "2"))))))
~~~
# FORMATTED
~~~roc
module [add2]

add2 = x + 2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 78)
		(p-assign @3-1-3-5 (ident "add2") (id 72))
		(e-binop @3-8-3-18 (op "add") (id 77)
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-int @3-17-3-18 (num-var 76) (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "add2") (type "*")))
	(expressions
		(expr @3-8-3-18 (type "*"))))
~~~