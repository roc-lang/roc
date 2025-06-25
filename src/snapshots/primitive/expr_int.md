# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 42
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),Int(2:7-2:9),EndOfFile(2:9-2:9),
~~~
# PARSE
~~~clojure
(file @1-1-2-9
	(module @1-1-1-13
		(exposes @1-8-1-13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @2-1-2-9
			(p-ident @2-1-2-4 (raw "foo"))
			(e-int @2-7-2-9 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 76)
		(p-assign @2-1-2-4 (ident "foo") (id 72))
		(e-int @2-7-2-9 (num-var 75) (sign-needed "false") (bits-needed "7") (value "42") (id 75))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "foo") (type "Num(Int(*))")))
	(expressions
		(expr @2-7-2-9 (type "Num(Int(*))"))))
~~~