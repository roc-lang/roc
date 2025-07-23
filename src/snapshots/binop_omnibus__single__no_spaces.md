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

**Undefined Variable**
The variable 'foo' is not defined:
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
UpperIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:8),CloseRound(1:8-1:9),OpDoubleQuestion(1:9-1:11),Int(1:11-1:13),OpGreaterThan(1:13-1:14),Int(1:14-1:15),OpStar(1:15-1:16),Int(1:16-1:17),OpOr(1:18-1:20),Int(1:21-1:23),OpPlus(1:23-1:24),Int(1:24-1:25),OpLessThan(1:25-1:26),Int(1:26-1:27),OpAnd(1:28-1:31),Int(1:32-1:34),Int(1:34-1:36),OpGreaterThanOrEq(1:36-1:38),Int(1:38-1:40),OpOr(1:41-1:43),Int(1:44-1:46),OpLessThanOrEq(1:46-1:48),Int(1:48-1:49),OpSlash(1:49-1:50),Int(1:50-1:51),EndOfFile(1:51-1:51),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.34 (op "or")
	(e-binop @1.1-1.17 (op ">")
		(e-binop @1.1-1.13 (op "??")
			(e-apply @1.1-1.9
				(e-tag @1.1-1.4 (raw "Err"))
				(e-ident @1.5-1.8 (raw "foo")))
			(e-int @1.11-1.13 (raw "12")))
		(e-binop @1.14-1.17 (op "*")
			(e-int @1.14-1.15 (raw "5"))
			(e-int @1.16-1.17 (raw "5"))))
	(e-binop @1.21-1.34 (op "and")
		(e-binop @1.21-1.27 (op "<")
			(e-binop @1.21-1.25 (op "+")
				(e-int @1.21-1.23 (raw "13"))
				(e-int @1.24-1.25 (raw "2")))
			(e-int @1.26-1.27 (raw "5")))
		(e-int @1.32-1.34 (raw "10"))))
~~~
# FORMATTED
~~~roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.34 (op "or")
	(e-binop @1.1-1.17 (op "gt")
		(e-binop @1.1-1.13 (op "null_coalesce")
			(e-tag @1.1-1.4 (name "Err")
				(args
					(e-runtime-error (tag "ident_not_in_scope"))))
			(e-int @1.11-1.13 (value "12")))
		(e-binop @1.14-1.17 (op "mul")
			(e-int @1.14-1.15 (value "5"))
			(e-int @1.16-1.17 (value "5"))))
	(e-binop @1.21-1.34 (op "and")
		(e-binop @1.21-1.27 (op "lt")
			(e-binop @1.21-1.25 (op "add")
				(e-int @1.21-1.23 (value "13"))
				(e-int @1.24-1.25 (value "2")))
			(e-int @1.26-1.27 (value "5")))
		(e-int @1.32-1.34 (value "10"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.34 (type "_a"))
~~~
