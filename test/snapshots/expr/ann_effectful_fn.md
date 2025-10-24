# META
~~~ini
description=Annotated effectful function
type=expr
~~~
# SOURCE
~~~roc
{
    launchTheNukes : {} => Result Bool LaunchNukeErr
    launchTheNukes = |{}| ...

    launchTheNukes({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VALUE**
This expression produces a value, but it's not being used:
**ann_effectful_fn.md:2:35:2:39:**
```roc
    launchTheNukes : {} => Result Bool LaunchNukeErr
```
                                  ^^^^

It has the type:
    _[Bool]_others_

**UNUSED VALUE**
This expression produces a value, but it's not being used:
**ann_effectful_fn.md:2:40:2:53:**
```roc
    launchTheNukes : {} => Result Bool LaunchNukeErr
```
                                       ^^^^^^^^^^^^^

It has the type:
    _[LaunchNukeErr]_others_

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpFatArrow,UpperIdent,UpperIdent,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,TripleDot,
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "launchTheNukes")
			(ty-fn
				(ty-record)
				(ty (name "Result"))))
		(e-tag (raw "Bool"))
		(e-tag (raw "LaunchNukeErr"))
		(s-decl
			(p-ident (raw "launchTheNukes"))
			(e-lambda
				(args
					(p-record))
				(e-ellipsis)))
		(e-apply
			(e-ident (raw "launchTheNukes"))
			(e-record))))
~~~
# FORMATTED
~~~roc
{
	launchTheNukes : {} => Result
	Bool
	LaunchNukeErr
	launchTheNukes = |{}| ...

	launchTheNukes({})
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-type-anno (name "launchTheNukes")
		(ty-fn (effectful true)
			(ty-record)
			(ty-lookup (name "Result") (external-module "Result"))))
	(s-expr
		(e-tag (name "Bool")))
	(s-expr
		(e-tag (name "LaunchNukeErr")))
	(s-let
		(p-assign (ident "launchTheNukes"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs)))
			(e-not-implemented)))
	(e-call
		(e-lookup-local
			(p-assign (ident "launchTheNukes")))
		(e-empty_record)))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
