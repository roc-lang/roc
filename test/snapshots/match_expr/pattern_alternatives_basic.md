# META
~~~ini
description=Basic pattern alternatives with multiple tag patterns
type=snippet
~~~
# SOURCE
~~~roc
Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
    Red | Green | Blue => "primary"
    Yellow | Orange | Purple => "secondary"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:6),OpColon(1:7-1:8),OpenSquare(1:9-1:10),UpperIdent(1:10-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:20),Comma(1:20-1:21),UpperIdent(1:22-1:26),Comma(1:26-1:27),UpperIdent(1:28-1:34),Comma(1:34-1:35),UpperIdent(1:36-1:42),Comma(1:42-1:43),UpperIdent(1:44-1:50),CloseSquare(1:50-1:51),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),UpperIdent(3:8-3:13),OpArrow(3:14-3:16),UpperIdent(3:17-3:20),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),LowerIdent(4:9-4:14),OpBar(4:14-4:15),KwMatch(4:16-4:21),LowerIdent(4:22-4:27),OpenCurly(4:28-4:29),
UpperIdent(5:5-5:8),OpBar(5:9-5:10),UpperIdent(5:11-5:16),OpBar(5:17-5:18),UpperIdent(5:19-5:23),OpFatArrow(5:24-5:26),StringStart(5:27-5:28),StringPart(5:28-5:35),StringEnd(5:35-5:36),
UpperIdent(6:5-6:11),OpBar(6:12-6:13),UpperIdent(6:14-6:20),OpBar(6:21-6:22),UpperIdent(6:23-6:29),OpFatArrow(6:30-6:32),StringStart(6:33-6:34),StringPart(6:34-6:43),StringEnd(6:43-6:44),
CloseCurly(7:1-7:2),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(type-module @1.1-1.6)
	(statements
		(s-type-decl @1.1-1.51
			(header @1.1-1.6 (name "Color")
				(args))
			(ty-tag-union @1.9-1.51
				(tags
					(ty @1.10-1.13 (name "Red"))
					(ty @1.15-1.20 (name "Green"))
					(ty @1.22-1.26 (name "Blue"))
					(ty @1.28-1.34 (name "Yellow"))
					(ty @1.36-1.42 (name "Orange"))
					(ty @1.44-1.50 (name "Purple")))))
		(s-type-anno @3.1-3.20 (name "kind")
			(ty-fn @3.8-3.20
				(ty @3.8-3.13 (name "Color"))
				(ty @3.17-3.20 (name "Str"))))
		(s-decl @4.1-7.2
			(p-ident @4.1-4.5 (raw "kind"))
			(e-lambda @4.8-7.2
				(args
					(p-ident @4.9-4.14 (raw "color")))
				(e-match
					(e-ident @4.22-4.27 (raw "color"))
					(branches
						(branch @5.5-5.36
							(p-alternatives
								(p-tag @5.5-5.8 (raw "Red"))
								(p-tag @5.11-5.16 (raw "Green"))
								(p-tag @5.19-5.23 (raw "Blue")))
							(e-string @5.27-5.36
								(e-string-part @5.28-5.35 (raw "primary"))))
						(branch @6.5-6.44
							(p-alternatives
								(p-tag @6.5-6.11 (raw "Yellow"))
								(p-tag @6.14-6.20 (raw "Orange"))
								(p-tag @6.23-6.29 (raw "Purple")))
							(e-string @6.33-6.44
								(e-string-part @6.34-6.43 (raw "secondary"))))))))))
~~~
# FORMATTED
~~~roc
Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
	Red | Green | Blue => "primary"
	Yellow | Orange | Purple => "secondary"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.5 (ident "kind"))
		(e-lambda @4.8-7.2
			(args
				(p-assign @4.9-4.14 (ident "color")))
			(e-match @4.16-7.2
				(match @4.16-7.2
					(cond
						(e-lookup-local @4.22-4.27
							(p-assign @4.9-4.14 (ident "color"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @5.5-5.8))
								(pattern (degenerate false)
									(p-applied-tag @5.11-5.16))
								(pattern (degenerate false)
									(p-applied-tag @5.19-5.23)))
							(value
								(e-string @5.27-5.36
									(e-literal @5.28-5.35 (string "primary")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @6.5-6.11))
								(pattern (degenerate false)
									(p-applied-tag @6.14-6.20))
								(pattern (degenerate false)
									(p-applied-tag @6.23-6.29)))
							(value
								(e-string @6.33-6.44
									(e-literal @6.34-6.43 (string "secondary")))))))))
		(annotation @4.1-4.5
			(declared-type
				(ty-fn @3.8-3.20 (effectful false)
					(ty-lookup @3.8-3.13 (name "Color") (local))
					(ty-lookup @3.17-3.20 (name "Str") (builtin))))))
	(s-alias-decl @1.1-1.51
		(ty-header @1.1-1.6 (name "Color"))
		(ty-tag-union @1.9-1.51
			(ty-tag-name @1.10-1.13 (name "Red"))
			(ty-tag-name @1.15-1.20 (name "Green"))
			(ty-tag-name @1.22-1.26 (name "Blue"))
			(ty-tag-name @1.28-1.34 (name "Yellow"))
			(ty-tag-name @1.36-1.42 (name "Orange"))
			(ty-tag-name @1.44-1.50 (name "Purple")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.5 (type "Color -> Str")))
	(type_decls
		(alias @1.1-1.51 (type "Color")
			(ty-header @1.1-1.6 (name "Color"))))
	(expressions
		(expr @4.8-7.2 (type "Color -> Str"))))
~~~
