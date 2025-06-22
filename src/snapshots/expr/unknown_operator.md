# META
~~~ini
description=Unknown operator, should produce an error
type=expr
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# PROBLEMS
~~~txt
PARSER: expr_unexpected_token
UNKNOWN OPERATOR
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.
~~~
# TOKENS
~~~zig
Int(1:1-1:2),OpPlus(1:3-1:4),OpPlus(1:4-1:5),Int(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(binop (1:1-1:7)
	"+"
	(int (1:1-1:2) "1")
	(malformed_expr (1:4-1:5) "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc
1 + 
~~~
# CANONICALIZE
~~~clojure
(e_binop (1:1-1:7)
	"add"
	(e_int (1:1-1:2)
		(int_var 13)
		(precision_var 12)
		(literal "1")
		(value "TODO")
		(bound "u8"))
	(e_runtime_error (1:1-1:7) "expr_not_canonicalized"))
~~~
# TYPES
~~~clojure
(expr 17 (type "*"))
~~~