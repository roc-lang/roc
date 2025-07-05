# META
~~~ini
description=ann_effectful_fn
type=expr
~~~
# SOURCE
~~~roc
{
    launchTheNukes : {} => Result Bool LaunchNukeErr
    launchTheNukes = |{}| {
        crash "todo"
    }
    launchTheNukes({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: crash statement
Let us know if you want to help!

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:19),OpColon(2:20-2:21),OpenCurly(2:22-2:23),CloseCurly(2:23-2:24),OpFatArrow(2:25-2:27),UpperIdent(2:28-2:34),UpperIdent(2:35-2:39),UpperIdent(2:40-2:53),Newline(1:1-1:1),
LowerIdent(3:5-3:19),OpAssign(3:20-3:21),OpBar(3:22-3:23),OpenCurly(3:23-3:24),CloseCurly(3:24-3:25),OpBar(3:25-3:26),OpenCurly(3:27-3:28),Newline(1:1-1:1),
KwCrash(4:9-4:14),StringStart(4:15-4:16),StringPart(4:16-4:20),StringEnd(4:20-4:21),Newline(1:1-1:1),
CloseCurly(5:5-5:6),Newline(1:1-1:1),
LowerIdent(6:5-6:19),NoSpaceOpenRound(6:19-6:20),OpenCurly(6:20-6:21),CloseCurly(6:21-6:22),CloseRound(6:22-6:23),Newline(1:1-1:1),
CloseCurly(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-7.2
	(statements
		(s-type-anno @2.5-2.39 (name "launchTheNukes")
			(ty-fn @2.22-2.34
				(ty-record @2.22-2.24)
				(ty @2.28-2.34 (name "Result"))))
		(e-tag @2.35-2.39 (raw "Bool"))
		(e-tag @2.40-2.53 (raw "LaunchNukeErr"))
		(s-decl @3.5-5.6
			(p-ident @3.5-3.19 (raw "launchTheNukes"))
			(e-lambda @3.22-5.6
				(args
					(p-record @3.23-3.25))
				(e-block @3.27-5.6
					(statements
						(s-crash @1.1-1.1
							(e-string @4.15-4.21
								(e-string-part @4.16-4.20 (raw "todo"))))))))
		(e-apply @6.5-6.23
			(e-ident @6.5-6.19 (raw "launchTheNukes"))
			(e-record @6.20-6.22))))
~~~
# FORMATTED
~~~roc
{
	launchTheNukes : {} => Result
	Bool
	LaunchNukeErr
	launchTheNukes = |{}| {
		crash "todo"
	}
	launchTheNukes({})
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-7.2
	(s-type-anno @2.5-2.39 (name "launchTheNukes")
		(ty-fn @2.22-2.34 (effectful true)
			(ty-record @2.22-2.24)
			(ty @2.28-2.34 (name "Result"))))
	(s-expr @2.35-2.53
		(e-tag @2.35-2.39 (name "Bool")))
	(s-expr @1.1-1.1
		(e-tag @2.40-2.53 (name "LaunchNukeErr")))
	(s-let @3.5-5.6
		(p-assign @3.5-3.19 (ident "launchTheNukes"))
		(e-lambda @3.22-5.6
			(args
				(p-record-destructure @3.23-3.25
					(destructs)))
			(e-block @3.27-5.6
				(e-runtime-error (tag "not_implemented")))))
	(e-call @6.5-6.23
		(e-lookup-local @6.5-6.19
			(pattern @3.5-3.19))
		(e-empty_record @6.20-6.22)))
~~~
# TYPES
~~~clojure
(expr @1.1-7.2 (type "*"))
~~~
