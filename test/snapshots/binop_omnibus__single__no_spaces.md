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
NOT IMPLEMENTED - binop_omnibus__single__no_spaces.md:1:1:1:13
MISSING METHOD - binop_omnibus__single__no_spaces.md:1:32:1:34
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**binop_omnibus__single__no_spaces.md:1:5:1:8:**
```roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
```
    ^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: unsupported operator

**binop_omnibus__single__no_spaces.md:1:1:1:13:**
```roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
```
^^^^^^^^^^^^

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!


**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**binop_omnibus__single__no_spaces.md:1:32:1:34:**
```roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
```
                               ^^

The value's type, which does not have a method named **from_numeral**, is:

    Bool

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

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
		(e-runtime-error (tag "not_implemented"))
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
(expr (type "Bool"))
~~~
