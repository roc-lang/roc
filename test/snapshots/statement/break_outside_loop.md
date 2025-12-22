# META
~~~ini
description=For loop with break
type=snippet
~~~
# SOURCE
~~~roc
result : Bool
result = {
	var $var = True
	break
	$var = False
	$var
}
~~~
# EXPECTED
BREAK OUTSIDE LOOP - break_outside_loop.md:4:2:4:7
# PROBLEMS
**BREAK OUTSIDE LOOP**
The `break` statement can only be used inside loops like `while` or `for` to exit the loop early.

**break_outside_loop.md:4:2:4:7:**
```roc
	break
```
	^^^^^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,UpperIdent,
KwBreak,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "result")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "$var")
						(e-tag (raw "True")))
					(s-break)
					(s-decl
						(p-ident (raw "$var"))
						(e-tag (raw "False")))
					(e-ident (raw "$var")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-block
			(s-var
				(p-assign (ident "$var"))
				(e-tag (name "True")))
			(s-break)
			(s-reassign
				(p-assign (ident "$var"))
				(e-tag (name "False")))
			(e-lookup-local
				(p-assign (ident "$var"))))
		(annotation
			(ty-lookup (name "Bool") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
