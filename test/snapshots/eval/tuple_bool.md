# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
~~~
# EXPECTED
UNDECLARED TYPE - tuple_bool.md:1:15:1:19
UNDECLARED TYPE - tuple_bool.md:1:26:1:30
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**tuple_bool.md:1:15:1:19:**
```roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
```
              ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**tuple_bool.md:1:26:1:30:**
```roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
```
                         ^^^^


# TOKENS
~~~zig
OpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,OpBang,UpperIdent,Comma,OpBang,UpperIdent,Comma,UpperIdent,OpAnd,UpperIdent,Comma,OpBang,UpperIdent,OpOr,OpBang,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-tag (raw "True"))
	(e-tag (raw "False"))
	(e-tag (raw "Bool.True"))
	(e-tag (raw "Bool.False"))
	(unary "!"
		(e-tag (raw "True")))
	(unary "!"
		(e-tag (raw "False")))
	(e-binop (op "and")
		(e-tag (raw "True"))
		(e-tag (raw "False")))
	(e-binop (op "or")
		(unary "!"
			(e-tag (raw "True")))
		(unary "!"
			(e-tag (raw "True")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-tag (name "True"))
		(e-tag (name "False"))
		(e-runtime-error (tag "undeclared_type"))
		(e-runtime-error (tag "undeclared_type"))
		(e-unary-not
			(e-tag (name "True")))
		(e-unary-not
			(e-tag (name "False")))
		(e-binop (op "and")
			(e-tag (name "True"))
			(e-tag (name "False")))
		(e-binop (op "or")
			(e-unary-not
				(e-tag (name "True")))
			(e-unary-not
				(e-tag (name "True"))))))
~~~
# TYPES
~~~clojure
(expr (type "([True]_others, [False]_others2, Error, Error, [True]_others3, [False]_others4, [True, False]_others5, [True]_others6)"))
~~~
