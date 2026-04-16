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
MISSING MEMBER - tuple_bool.md:1:38:1:43
MISSING MEMBER - tuple_bool.md:1:45:1:51
MISSING MEMBER - tuple_bool.md:1:69:1:74
MISSING MEMBER - tuple_bool.md:1:78:1:83
# PROBLEMS
**MISSING MEMBER**
This **not** member is being used on a value whose type doesn't provide that member:
**tuple_bool.md:1:38:1:43:**
```roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
```
                                     ^^^^^

The value's type, which does not have a member named **not**, is:

    [True, ..]

**MISSING MEMBER**
This **not** member is being used on a value whose type doesn't provide that member:
**tuple_bool.md:1:45:1:51:**
```roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
```
                                            ^^^^^^

The value's type, which does not have a member named **not**, is:

    [False, ..]

**MISSING MEMBER**
This **not** member is being used on a value whose type doesn't provide that member:
**tuple_bool.md:1:69:1:74:**
```roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
```
                                                                    ^^^^^

The value's type, which does not have a member named **not**, is:

    [True, ..]

**MISSING MEMBER**
This **not** member is being used on a value whose type doesn't provide that member:
**tuple_bool.md:1:78:1:83:**
```roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
```
                                                                             ^^^^^

The value's type, which does not have a member named **not**, is:

    [True, ..]

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
		(e-nominal-external
			(builtin)
			(e-tag (name "True")))
		(e-nominal-external
			(builtin)
			(e-tag (name "False")))
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
(expr (type "([True, ..], [False, ..], Bool, Bool, Error, Error, Bool, Error)"))
~~~
