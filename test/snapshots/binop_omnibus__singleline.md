# META
~~~ini
description=Binop omnibus - singleline
type=expr
~~~
# SOURCE
~~~roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
~~~
# EXPECTED
UNDEFINED VARIABLE - binop_omnibus__singleline.md:1:5:1:8
NOT IMPLEMENTED - :0:0:0:0
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**binop_omnibus__singleline.md:1:5:1:8:**
```roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
    ^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: unsupported operator

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,Int,OpGreaterThan,Int,OpStar,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,OpBinaryMinus,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,OpSlash,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "or")
	(e-binop (op ">")
		(e-binop (op "??")
			(e-apply
				(e-tag (raw "Err"))
				(e-ident (raw "foo")))
			(e-int (raw "12")))
		(e-binop (op "*")
			(e-int (raw "5"))
			(e-int (raw "5"))))
	(e-binop (op "or")
		(e-binop (op "and")
			(e-binop (op "<")
				(e-binop (op "+")
					(e-int (raw "13"))
					(e-int (raw "2")))
				(e-int (raw "5")))
			(e-binop (op ">=")
				(e-binop (op "-")
					(e-int (raw "10"))
					(e-int (raw "1")))
				(e-int (raw "16"))))
		(e-binop (op "<=")
			(e-int (raw "12"))
			(e-binop (op "/")
				(e-int (raw "3"))
				(e-int (raw "5"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop (op "or")
	(e-binop (op "gt")
		(e-runtime-error (tag "not_implemented"))
		(e-binop (op "mul")
			(e-num (value "5"))
			(e-num (value "5"))))
	(e-binop (op "or")
		(e-binop (op "and")
			(e-binop (op "lt")
				(e-binop (op "add")
					(e-num (value "13"))
					(e-num (value "2")))
				(e-num (value "5")))
			(e-binop (op "ge")
				(e-binop (op "sub")
					(e-num (value "10"))
					(e-num (value "1")))
				(e-num (value "16"))))
		(e-binop (op "le")
			(e-num (value "12"))
			(e-binop (op "div")
				(e-num (value "3"))
				(e-num (value "5"))))))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
