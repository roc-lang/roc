# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = FortyTwo
~~~
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),UpperIdent(2:7-2:15),EndOfFile(2:15-2:15),
~~~
# PARSE
~~~clojure
(file (1:1-2:15)
	(module (1:1-1:13)
		(exposes (1:8-1:13) (exposed_item (lower_ident "foo"))))
	(statements
		(decl (2:1-2:15)
			(ident (2:1-2:4) "foo")
			(tag (2:7-2:15) "FortyTwo"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (2:1-2:4)
				(pid 12)
				(ident "foo")))
		(def_expr
			(e_tag (2:7-2:15)
				(ext_var 0)
				(name "FortyTwo")
				(args "TODO")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "foo" 15 (type "[FortyTwo, * *]")))
	(expressions
		(expr (2:7-2:15) 14 (type "[FortyTwo, * *]"))))
~~~