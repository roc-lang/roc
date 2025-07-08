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
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:19),OpColon(2:20-2:21),OpenCurly(2:22-2:23),CloseCurly(2:23-2:24),OpFatArrow(2:25-2:27),UpperIdent(2:28-2:34),UpperIdent(2:35-2:39),UpperIdent(2:40-2:53),Newline(1:1-1:1),
LowerIdent(3:5-3:19),OpAssign(3:20-3:21),OpBar(3:22-3:23),OpenCurly(3:23-3:24),CloseCurly(3:24-3:25),OpBar(3:25-3:26),TripleDot(3:27-3:30),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:5-5:19),NoSpaceOpenRound(5:19-5:20),OpenCurly(5:20-5:21),CloseCurly(5:21-5:22),CloseRound(5:22-5:23),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.5-2.39 (name "launchTheNukes")
			(ty-fn @2.22-2.34
				(ty-record @2.22-2.24)
				(ty @2.28-2.34 (name "Result"))))
		(e-tag @2.35-2.39 (raw "Bool"))
		(e-tag @2.40-2.53 (raw "LaunchNukeErr"))
		(s-decl @3.5-3.30
			(p-ident @3.5-3.19 (raw "launchTheNukes"))
			(e-lambda @3.22-3.30
				(args
					(p-record @3.23-3.25))
				(e-ellipsis)))
		(e-apply @5.5-5.23
			(e-ident @5.5-5.19 (raw "launchTheNukes"))
			(e-record @5.20-5.22))))
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
(e-block @1.1-6.2
	(s-type-anno @2.5-2.39 (name "launchTheNukes")
		(ty-func @2.22-2.34 (effectful true)
			(ty-record @2.22-2.24)
			(ty-type @2.28-2.34 (name "Result"))))
	(s-expr @2.35-2.53
		(e-tag @2.35-2.39 (name "Bool")))
	(s-expr @1.1-1.1
		(e-tag @2.40-2.53 (name "LaunchNukeErr")))
	(s-var @3.5-3.30
		(p-assign @3.5-3.19 (ident "launchTheNukes"))
		(e-lambda @3.22-3.30
			(args
				(p-record-destructure @3.23-3.25
					(destructs)))
			(e-not-implemented @3.27-3.30)))
	(e-call @5.5-5.23
		(e-lookup-local @5.5-5.19
			(p-assign @3.5-3.19 (ident "launchTheNukes")))
		(e-empty-record @5.20-5.22)))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "*"))
~~~
