# META
~~~ini
description=Binop omnibus - singleline - no spaces
type=expr
~~~
# SOURCE
~~~roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
~~~
# EXPECTED
UNDEFINED VARIABLE - binop_omnibus__single__no_spaces.md:1:5:1:8
INVALID BOOL OPERATION - binop_omnibus__single__no_spaces.md:1:21:1:21
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**binop_omnibus__single__no_spaces.md:1:5:1:8:**
```roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
```
    ^^^


**INVALID BOOL OPERATION**
I'm having trouble with this bool operation:
**binop_omnibus__single__no_spaces.md:1:21:**
```roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
```
                               ^^

Both sides of `and` must be _Bool_ values, but the right side is:
    _Num(_size)_

Note: Roc does not have "truthiness" where other values like strings, numbers or lists are automatically converted to bools. You must do that conversion yourself!

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,Int,OpGreaterThan,Int,OpStar,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,OpSlash,Int,
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
	(e-binop (op "and")
		(e-binop (op "<")
			(e-binop (op "+")
				(e-int (raw "13"))
				(e-int (raw "2")))
			(e-int (raw "5")))
		(e-int (raw "10"))))
~~~
# FORMATTED
~~~roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10
~~~
# CANONICALIZE
~~~clojure
(e-binop (op "or")
	(e-binop (op "gt")
		(e-binop (op "null_coalesce")
			(e-tag (name "Err")
				(args
					(e-runtime-error (tag "ident_not_in_scope"))))
			(e-num (value "12")))
		(e-binop (op "mul")
			(e-num (value "5"))
			(e-num (value "5"))))
	(e-binop (op "and")
		(e-binop (op "lt")
			(e-binop (op "add")
				(e-num (value "13"))
				(e-num (value "2")))
			(e-num (value "5")))
		(e-num (value "10"))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
