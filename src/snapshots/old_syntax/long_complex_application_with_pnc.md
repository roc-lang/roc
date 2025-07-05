# META
~~~ini
description=long_complex_application_with_pnc
type=expr
~~~
# SOURCE
~~~roc
combine(mix(vodka, gin), Juices({
    color: Colors.orange,
    flavor: Flavors.orange,
    amount: 1 + 2
}))
~~~
# EXPECTED
UNDEFINED VARIABLE - long_complex_application_with_pnc.md:1:1:1:8
UNDEFINED VARIABLE - long_complex_application_with_pnc.md:1:9:1:12
UNDEFINED VARIABLE - long_complex_application_with_pnc.md:1:13:1:18
UNDEFINED VARIABLE - long_complex_application_with_pnc.md:1:20:1:23
UNDEFINED VARIABLE - long_complex_application_with_pnc.md:2:12:2:25
UNDEFINED VARIABLE - long_complex_application_with_pnc.md:3:13:3:27
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:8),NoSpaceOpenRound(1:8-1:9),LowerIdent(1:9-1:12),NoSpaceOpenRound(1:12-1:13),LowerIdent(1:13-1:18),Comma(1:18-1:19),LowerIdent(1:20-1:23),CloseRound(1:23-1:24),Comma(1:24-1:25),UpperIdent(1:26-1:32),NoSpaceOpenRound(1:32-1:33),OpenCurly(1:33-1:34),Newline(1:1-1:1),
LowerIdent(2:5-2:10),OpColon(2:10-2:11),UpperIdent(2:12-2:18),NoSpaceDotLowerIdent(2:18-2:25),Comma(2:25-2:26),Newline(1:1-1:1),
LowerIdent(3:5-3:11),OpColon(3:11-3:12),UpperIdent(3:13-3:20),NoSpaceDotLowerIdent(3:20-3:27),Comma(3:27-3:28),Newline(1:1-1:1),
LowerIdent(4:5-4:11),OpColon(4:11-4:12),Int(4:13-4:14),OpPlus(4:15-4:16),Int(4:17-4:18),Newline(1:1-1:1),
CloseCurly(5:1-5:2),CloseRound(5:2-5:3),CloseRound(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-apply @1.1-5.4
	(e-ident @1.1-1.8 (raw "combine"))
	(e-apply @1.9-1.24
		(e-ident @1.9-1.12 (raw "mix"))
		(e-ident @1.13-1.18 (raw "vodka"))
		(e-ident @1.20-1.23 (raw "gin")))
	(e-apply @1.26-5.3
		(e-tag @1.26-1.32 (raw "Juices"))
		(e-record @1.33-5.2
			(field (field "color") (optional false)
				(e-ident @2.12-2.25 (raw "Colors.orange")))
			(field (field "flavor") (optional false)
				(e-ident @3.13-3.27 (raw "Flavors.orange")))
			(field (field "amount") (optional false)
				(e-binop @4.13-5.2 (op "+")
					(e-int @4.13-4.14 (raw "1"))
					(e-int @4.17-4.18 (raw "2")))))))
~~~
# FORMATTED
~~~roc
combine(
	mix(vodka, gin),
	Juices(
		{

			color: Colors.orange,
			flavor: Flavors.orange,
			amount: 1 + 2

		},
	),
)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-5.4
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-call @1.9-1.24
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-tag @1.26-5.3 (name "Juices")
		(args
			(e-record @1.33-5.2
				(fields
					(field (name "color")
						(e-runtime-error (tag "ident_not_in_scope")))
					(field (name "flavor")
						(e-runtime-error (tag "ident_not_in_scope")))
					(field (name "amount")
						(e-binop @4.13-5.2 (op "add")
							(e-int @4.13-4.14 (value "1"))
							(e-int @4.17-4.18 (value "2")))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.4 (type "*"))
~~~
