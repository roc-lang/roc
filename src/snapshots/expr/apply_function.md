# META
~~~ini
description=Function application expression
type=expr
~~~
# SOURCE
~~~roc
foo(42, "hello")
~~~
# PROBLEMS
~~~txt
UNDEFINED VARIABLE
Nothing is named `foo` in this scope.
Is there an import or exposing missing up-top?
~~~
# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:7),Comma(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),CloseRound(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(apply (1:1-1:17)
	(ident (1:1-1:4) "" "foo")
	(int (1:5-1:7) "42")
	(string (1:9-1:16) (string_part (1:10-1:15) "hello")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_call (1:1-1:17)
	(e_runtime_error (1:1-1:4) "ident_not_in_scope")
	(e_int (1:5-1:7)
		(int_var 15)
		(precision_var 14)
		(literal "42")
		(value "TODO")
		(bound "u8"))
	(e_string (1:9-1:16) (e_literal (1:10-1:15) "hello")))
~~~
# TYPES
~~~clojure
(expr 19 (type "*"))
~~~