# META
~~~ini
description=Boolean closure type checking - should have no errors
type=expr
~~~
# SOURCE
~~~roc
(|x| !x)(True)
~~~
# EXPECTED
MISSING MEMBER - bool_closure_type_check.md:1:6:1:8
# PROBLEMS
**MISSING MEMBER**
This **not** member is being used on a value whose type doesn't provide that member:
**bool_closure_type_check.md:1:6:1:8:**
```roc
(|x| !x)(True)
```
     ^^

The value's type, which does not have a member named **not**, is:

    [True, ..]

# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBang,LowerIdent,CloseRound,NoSpaceOpenRound,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-ident (raw "x")))
			(unary "!"
				(e-ident (raw "x")))))
	(e-tag (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-unary-not
			(e-lookup-local
				(p-assign (ident "x")))))
	(e-tag (name "True")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
