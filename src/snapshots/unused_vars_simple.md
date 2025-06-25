# META
~~~ini
description=Simple unused and used underscore variable test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
    a = unused_regular(5)
    b = used_underscore(10)
    c = unused_underscore(15)
    d = used_regular(20)
    a + b + c + d
}
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**unused_vars_simple.md:4:19:4:20:**
```roc
unused_regular = |x| 42
```


**UNDERSCORE VARIABLE USED**
Variable ``_value`` is prefixed with an underscore but is actually used.

Variables prefixed with `_` are intended to be unused. Remove the underscore prefix: `value`.
The underscore variable is declared here:
**unused_vars_simple.md:7:28:7:34:**
```roc
used_underscore = |_value| _value
```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:48),
LowerIdent(4:1-4:15),OpAssign(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),Int(4:22-4:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:49),
LowerIdent(7:1-7:16),OpAssign(7:17-7:18),OpBar(7:19-7:20),NamedUnderscore(7:20-7:26),OpBar(7:26-7:27),NamedUnderscore(7:28-7:34),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:2-9:54),
LowerIdent(10:1-10:18),OpAssign(10:19-10:20),OpBar(10:21-10:22),NamedUnderscore(10:22-10:30),OpBar(10:30-10:31),Int(10:32-10:35),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:2-12:49),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),OpBar(13:16-13:17),LowerIdent(13:17-13:23),OpBar(13:23-13:24),LowerIdent(13:25-13:31),OpPlus(13:32-13:33),Int(13:34-13:35),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(15:1-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),OpenCurly(15:13-15:14),Newline(1:1-1:1),
LowerIdent(16:5-16:6),OpAssign(16:7-16:8),LowerIdent(16:9-16:23),NoSpaceOpenRound(16:23-16:24),Int(16:24-16:25),CloseRound(16:25-16:26),Newline(1:1-1:1),
LowerIdent(17:5-17:6),OpAssign(17:7-17:8),LowerIdent(17:9-17:24),NoSpaceOpenRound(17:24-17:25),Int(17:25-17:27),CloseRound(17:27-17:28),Newline(1:1-1:1),
LowerIdent(18:5-18:6),OpAssign(18:7-18:8),LowerIdent(18:9-18:26),NoSpaceOpenRound(18:26-18:27),Int(18:27-18:29),CloseRound(18:29-18:30),Newline(1:1-1:1),
LowerIdent(19:5-19:6),OpAssign(19:7-19:8),LowerIdent(19:9-19:21),NoSpaceOpenRound(19:21-19:22),Int(19:22-19:24),CloseRound(19:24-19:25),Newline(1:1-1:1),
LowerIdent(20:5-20:6),OpPlus(20:7-20:8),LowerIdent(20:9-20:10),OpPlus(20:11-20:12),LowerIdent(20:13-20:14),OpPlus(20:15-20:16),LowerIdent(20:17-20:18),Newline(1:1-1:1),
CloseCurly(21:1-21:2),EndOfFile(21:2-21:2),
~~~
# PARSE
~~~clojure
(file @1-1-21-2
	(app @1-1-1-53
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-53 (name "pf")
			(e-string @1-28-1-51
				(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))
		(packages @1-13-1-53
			(record-field @1-15-1-53 (name "pf")
				(e-string @1-28-1-51
					(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl @4-1-4-24
			(p-ident @4-1-4-15 (raw "unused_regular"))
			(e-lambda @4-18-4-24
				(args
					(p-ident @4-19-4-20 (raw "x")))
				(e-int @4-22-4-24 (raw "42"))))
		(s-decl @7-1-7-34
			(p-ident @7-1-7-16 (raw "used_underscore"))
			(e-lambda @7-19-7-34
				(args
					(p-ident @7-20-7-26 (raw "_value")))
				(e-ident @7-28-7-34 (qaul "") (raw "_value"))))
		(s-decl @10-1-10-35
			(p-ident @10-1-10-18 (raw "unused_underscore"))
			(e-lambda @10-21-10-35
				(args
					(p-ident @10-22-10-30 (raw "_ignored")))
				(e-int @10-32-10-35 (raw "100"))))
		(s-decl @13-1-15-6
			(p-ident @13-1-13-13 (raw "used_regular"))
			(e-lambda @13-16-15-6
				(args
					(p-ident @13-17-13-23 (raw "number")))
				(e-binop @13-25-15-6 (op "+")
					(e-ident @13-25-13-31 (qaul "") (raw "number"))
					(e-int @13-34-13-35 (raw "1")))))
		(s-decl @15-1-21-2
			(p-ident @15-1-15-6 (raw "main!"))
			(e-lambda @15-9-21-2
				(args
					(p-underscore))
				(e-block @15-13-21-2
					(statements
						(s-decl @16-5-16-26
							(p-ident @16-5-16-6 (raw "a"))
							(e-apply @16-9-16-26
								(e-ident @16-9-16-23 (qaul "") (raw "unused_regular"))
								(e-int @16-24-16-25 (raw "5"))))
						(s-decl @17-5-17-28
							(p-ident @17-5-17-6 (raw "b"))
							(e-apply @17-9-17-28
								(e-ident @17-9-17-24 (qaul "") (raw "used_underscore"))
								(e-int @17-25-17-27 (raw "10"))))
						(s-decl @18-5-18-30
							(p-ident @18-5-18-6 (raw "c"))
							(e-apply @18-9-18-30
								(e-ident @18-9-18-26 (qaul "") (raw "unused_underscore"))
								(e-int @18-27-18-29 (raw "15"))))
						(s-decl @19-5-19-25
							(p-ident @19-5-19-6 (raw "d"))
							(e-apply @19-9-19-25
								(e-ident @19-9-19-21 (qaul "") (raw "used_regular"))
								(e-int @19-22-19-24 (raw "20"))))
						(e-binop @20-5-21-2 (op "+")
							(e-ident @20-5-20-6 (qaul "") (raw "a"))
							(e-binop @20-9-21-2 (op "+")
								(e-ident @20-9-20-10 (qaul "") (raw "b"))
								(e-binop @20-13-21-2 (op "+")
									(e-ident @20-13-20-14 (qaul "") (raw "c"))
									(e-ident @20-17-20-18 (qaul "") (raw "d")))))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
	a = unused_regular(5)
	b = used_underscore(10)
	c = unused_underscore(15)
	d = used_regular(20)
	a + b + c + d
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 79)
		(p-assign @4-1-4-15 (ident "unused_regular") (id 72))
		(e-lambda @4-18-4-24 (id 77)
			(args
				(p-assign @4-19-4-20 (ident "x") (id 73)))
			(e-int @4-22-4-24 (num-var 76) (sign-needed "false") (bits-needed "7") (value "42"))))
	(d-let (id 85)
		(p-assign @7-1-7-16 (ident "used_underscore") (id 80))
		(e-lambda @7-19-7-34 (id 84)
			(args
				(p-assign @7-20-7-26 (ident "_value") (id 81)))
			(e-lookup-local @7-28-7-34
				(pattern (id 81)))))
	(d-let (id 92)
		(p-assign @10-1-10-18 (ident "unused_underscore") (id 86))
		(e-lambda @10-21-10-35 (id 91)
			(args
				(p-assign @10-22-10-30 (ident "_ignored") (id 87)))
			(e-int @10-32-10-35 (num-var 90) (sign-needed "false") (bits-needed "7") (value "100"))))
	(d-let (id 101)
		(p-assign @13-1-13-13 (ident "used_regular") (id 93))
		(e-lambda @13-16-15-6 (id 100)
			(args
				(p-assign @13-17-13-23 (ident "number") (id 94)))
			(e-binop @13-25-15-6 (op "add")
				(e-lookup-local @13-25-13-31
					(pattern (id 94)))
				(e-int @13-34-13-35 (num-var 98) (sign-needed "false") (bits-needed "7") (value "1")))))
	(d-let (id 141)
		(p-assign @15-1-15-6 (ident "main!") (id 102))
		(e-lambda @15-9-21-2 (id 140)
			(args
				(p-underscore @15-10-15-11 (id 103)))
			(e-block @15-13-21-2
				(s-let @16-5-16-26
					(p-assign @16-5-16-6 (ident "a") (id 104))
					(e-call @16-9-16-26 (id 109)
						(e-lookup-local @16-9-16-23
							(pattern (id 72)))
						(e-int @16-24-16-25 (num-var 108) (sign-needed "false") (bits-needed "7") (value "5"))))
				(s-let @17-5-17-28
					(p-assign @17-5-17-6 (ident "b") (id 111))
					(e-call @17-9-17-28 (id 116)
						(e-lookup-local @17-9-17-24
							(pattern (id 80)))
						(e-int @17-25-17-27 (num-var 115) (sign-needed "false") (bits-needed "7") (value "10"))))
				(s-let @18-5-18-30
					(p-assign @18-5-18-6 (ident "c") (id 118))
					(e-call @18-9-18-30 (id 123)
						(e-lookup-local @18-9-18-26
							(pattern (id 86)))
						(e-int @18-27-18-29 (num-var 122) (sign-needed "false") (bits-needed "7") (value "15"))))
				(s-let @19-5-19-25
					(p-assign @19-5-19-6 (ident "d") (id 125))
					(e-call @19-9-19-25 (id 130)
						(e-lookup-local @19-9-19-21
							(pattern (id 93)))
						(e-int @19-22-19-24 (num-var 129) (sign-needed "false") (bits-needed "7") (value "20"))))
				(e-binop @20-5-21-2 (op "add")
					(e-lookup-local @20-5-20-6
						(pattern (id 104)))
					(e-binop @20-9-21-2 (op "add")
						(e-lookup-local @20-9-20-10
							(pattern (id 111)))
						(e-binop @20-13-21-2 (op "add")
							(e-lookup-local @20-13-20-14
								(pattern (id 118)))
							(e-lookup-local @20-17-20-18
								(pattern (id 125))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "unused_regular") (type "*"))
		(def (name "used_underscore") (type "*"))
		(def (name "unused_underscore") (type "*"))
		(def (name "used_regular") (type "*"))
		(def (name "main!") (type "*")))
	(expressions
		(expr @4-18-4-24 (type "*"))
		(expr @7-19-7-34 (type "*"))
		(expr @10-21-10-35 (type "*"))
		(expr @13-16-15-6 (type "*"))
		(expr @15-9-21-2 (type "*"))))
~~~