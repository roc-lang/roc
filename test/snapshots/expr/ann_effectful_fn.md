# META
~~~ini
description=Annotated effectful function
type=expr
~~~
# SOURCE
~~~roc
{
    launchTheNukes : {} => Try Bool LaunchNukeErr
    launchTheNukes = |{}| ...

    launchTheNukes({})
}
~~~
# EXPECTED
DUPLICATE DEFINITION - ann_effectful_fn.md:3:5:3:19
TYPE MISMATCH - ann_effectful_fn.md:2:32:2:36
TYPE MISMATCH - ann_effectful_fn.md:2:37:2:50
# PROBLEMS
**DUPLICATE DEFINITION**
The name `launchTheNukes` is being redeclared in this scope.

The redeclaration is here:
**ann_effectful_fn.md:3:5:3:19:**
```roc
    launchTheNukes = |{}| ...
```
    ^^^^^^^^^^^^^^

But `launchTheNukes` was already defined here:
**ann_effectful_fn.md:2:5:2:31:**
```roc
    launchTheNukes : {} => Try Bool LaunchNukeErr
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE MISMATCH**
This expression produces a value, but it's not being used:
**ann_effectful_fn.md:2:32:2:36:**
```roc
    launchTheNukes : {} => Try Bool LaunchNukeErr
```
                               ^^^^

It has the type:

    [Bool, ..]

Since this expression is used as a statement, it must evaluate to `{}`.
If you don't need the value, you can ignore it with `_ =`.

**TYPE MISMATCH**
This expression produces a value, but it's not being used:
**ann_effectful_fn.md:2:37:2:50:**
```roc
    launchTheNukes : {} => Try Bool LaunchNukeErr
```
                                    ^^^^^^^^^^^^^

It has the type:

    [LaunchNukeErr, ..]

Since this expression is used as a statement, it must evaluate to `{}`.
If you don't need the value, you can ignore it with `_ =`.

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
				(ty (name "Try"))))
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
	launchTheNukes : {} => Try
	Bool
	LaunchNukeErr
	launchTheNukes = |{}| ...

	launchTheNukes({})
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "launchTheNukes"))
		(e-anno-only))
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
