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
# EXPECTED
UNUSED VARIABLE - unused_vars_simple.md:4:19:4:20
UNDERSCORE VARIABLE USED - unused_vars_simple.md:7:28:7:34
# PROBLEMS
**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**unused_vars_simple.md:4:19:4:20:**
```roc
unused_regular = |x| 42
```
                  ^


**UNDERSCORE VARIABLE USED**
Variable `_value` is prefixed with an underscore but is actually used.

Variables prefixed with an underscore are supposed to be ignored. But `_value` is used here:
**unused_vars_simple.md:7:28:7:34:**
```roc
used_underscore = |_value| _value
```
                           ^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(4:1-4:15),OpAssign(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),Int(4:22-4:24),
LowerIdent(7:1-7:16),OpAssign(7:17-7:18),OpBar(7:19-7:20),NamedUnderscore(7:20-7:26),OpBar(7:26-7:27),NamedUnderscore(7:28-7:34),
LowerIdent(10:1-10:18),OpAssign(10:19-10:20),OpBar(10:21-10:22),NamedUnderscore(10:22-10:30),OpBar(10:30-10:31),Int(10:32-10:35),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),OpBar(13:16-13:17),LowerIdent(13:17-13:23),OpBar(13:23-13:24),LowerIdent(13:25-13:31),OpPlus(13:32-13:33),Int(13:34-13:35),
LowerIdent(15:1-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),OpenCurly(15:13-15:14),
LowerIdent(16:5-16:6),OpAssign(16:7-16:8),LowerIdent(16:9-16:23),NoSpaceOpenRound(16:23-16:24),Int(16:24-16:25),CloseRound(16:25-16:26),
LowerIdent(17:5-17:6),OpAssign(17:7-17:8),LowerIdent(17:9-17:24),NoSpaceOpenRound(17:24-17:25),Int(17:25-17:27),CloseRound(17:27-17:28),
LowerIdent(18:5-18:6),OpAssign(18:7-18:8),LowerIdent(18:9-18:26),NoSpaceOpenRound(18:26-18:27),Int(18:27-18:29),CloseRound(18:29-18:30),
LowerIdent(19:5-19:6),OpAssign(19:7-19:8),LowerIdent(19:9-19:21),NoSpaceOpenRound(19:21-19:22),Int(19:22-19:24),CloseRound(19:24-19:25),
LowerIdent(20:5-20:6),OpPlus(20:7-20:8),LowerIdent(20:9-20:10),OpPlus(20:11-20:12),LowerIdent(20:13-20:14),OpPlus(20:15-20:16),LowerIdent(20:17-20:18),
CloseCurly(21:1-21:2),EndOfFile(21:2-21:2),
~~~
# PARSE
~~~clojure
(file @1.1-21.2
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl @4.1-4.24
			(p-ident @4.1-4.15 (raw "unused_regular"))
			(e-lambda @4.18-4.24
				(args
					(p-ident @4.19-4.20 (raw "x")))
				(e-int @4.22-4.24 (raw "42"))))
		(s-decl @7.1-7.34
			(p-ident @7.1-7.16 (raw "used_underscore"))
			(e-lambda @7.19-7.34
				(args
					(p-ident @7.20-7.26 (raw "_value")))
				(e-ident @7.28-7.34 (raw "_value"))))
		(s-decl @10.1-10.35
			(p-ident @10.1-10.18 (raw "unused_underscore"))
			(e-lambda @10.21-10.35
				(args
					(p-ident @10.22-10.30 (raw "_ignored")))
				(e-int @10.32-10.35 (raw "100"))))
		(s-decl @13.1-13.35
			(p-ident @13.1-13.13 (raw "used_regular"))
			(e-lambda @13.16-13.35
				(args
					(p-ident @13.17-13.23 (raw "number")))
				(e-binop @13.25-13.35 (op "+")
					(e-ident @13.25-13.31 (raw "number"))
					(e-int @13.34-13.35 (raw "1")))))
		(s-decl @15.1-21.2
			(p-ident @15.1-15.6 (raw "main!"))
			(e-lambda @15.9-21.2
				(args
					(p-underscore))
				(e-block @15.13-21.2
					(statements
						(s-decl @16.5-16.26
							(p-ident @16.5-16.6 (raw "a"))
							(e-apply @16.9-16.26
								(e-ident @16.9-16.23 (raw "unused_regular"))
								(e-int @16.24-16.25 (raw "5"))))
						(s-decl @17.5-17.28
							(p-ident @17.5-17.6 (raw "b"))
							(e-apply @17.9-17.28
								(e-ident @17.9-17.24 (raw "used_underscore"))
								(e-int @17.25-17.27 (raw "10"))))
						(s-decl @18.5-18.30
							(p-ident @18.5-18.6 (raw "c"))
							(e-apply @18.9-18.30
								(e-ident @18.9-18.26 (raw "unused_underscore"))
								(e-int @18.27-18.29 (raw "15"))))
						(s-decl @19.5-19.25
							(p-ident @19.5-19.6 (raw "d"))
							(e-apply @19.9-19.25
								(e-ident @19.9-19.21 (raw "used_regular"))
								(e-int @19.22-19.24 (raw "20"))))
						(e-binop @20.5-20.18 (op "+")
							(e-ident @20.5-20.6 (raw "a"))
							(e-binop @20.9-20.18 (op "+")
								(e-ident @20.9-20.10 (raw "b"))
								(e-binop @20.13-20.18 (op "+")
									(e-ident @20.13-20.14 (raw "c"))
									(e-ident @20.17-20.18 (raw "d")))))))))))
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
	(def
		(pattern
			(p-assign @4.1-4.15 (ident "unused_regular")))
		(expr
			(e-lambda @4.18-4.24
				(args
					(p-assign @4.19-4.20 (ident "x")))
				(e-int @4.22-4.24 (value "42")))))
	(def
		(pattern
			(p-assign @7.1-7.16 (ident "used_underscore")))
		(expr
			(e-lambda @7.19-7.34
				(args
					(p-assign @7.20-7.26 (ident "_value")))
				(e-lookup-local @7.28-7.34
					(p-assign @7.20-7.26 (ident "_value"))))))
	(def
		(pattern
			(p-assign @10.1-10.18 (ident "unused_underscore")))
		(expr
			(e-lambda @10.21-10.35
				(args
					(p-assign @10.22-10.30 (ident "_ignored")))
				(e-int @10.32-10.35 (value "100")))))
	(def
		(pattern
			(p-assign @13.1-13.13 (ident "used_regular")))
		(expr
			(e-lambda @13.16-13.35
				(args
					(p-assign @13.17-13.23 (ident "number")))
				(e-binop @13.25-13.35 (op "add")
					(e-lookup-local @13.25-13.31
						(p-assign @13.17-13.23 (ident "number")))
					(e-int @13.34-13.35 (value "1"))))))
	(def
		(pattern
			(p-assign @15.1-15.6 (ident "main!")))
		(expr
			(e-lambda @15.9-21.2
				(args
					(p-underscore @15.10-15.11))
				(e-block @15.13-21.2
					(s-let @16.5-16.26
						(p-assign @16.5-16.6 (ident "a"))
						(e-call @16.9-16.26
							(e-lookup-local @16.9-16.23
								(p-assign @4.1-4.15 (ident "unused_regular")))
							(e-int @16.24-16.25 (value "5"))))
					(s-let @17.5-17.28
						(p-assign @17.5-17.6 (ident "b"))
						(e-call @17.9-17.28
							(e-lookup-local @17.9-17.24
								(p-assign @7.1-7.16 (ident "used_underscore")))
							(e-int @17.25-17.27 (value "10"))))
					(s-let @18.5-18.30
						(p-assign @18.5-18.6 (ident "c"))
						(e-call @18.9-18.30
							(e-lookup-local @18.9-18.26
								(p-assign @10.1-10.18 (ident "unused_underscore")))
							(e-int @18.27-18.29 (value "15"))))
					(s-let @19.5-19.25
						(p-assign @19.5-19.6 (ident "d"))
						(e-call @19.9-19.25
							(e-lookup-local @19.9-19.21
								(p-assign @13.1-13.13 (ident "used_regular")))
							(e-int @19.22-19.24 (value "20"))))
					(e-binop @20.5-20.18 (op "add")
						(e-lookup-local @20.5-20.6
							(p-assign @16.5-16.6 (ident "a")))
						(e-binop @20.9-20.18 (op "add")
							(e-lookup-local @20.9-20.10
								(p-assign @17.5-17.6 (ident "b")))
							(e-binop @20.13-20.18 (op "add")
								(e-lookup-local @20.13-20.14
									(p-assign @18.5-18.6 (ident "c")))
								(e-lookup-local @20.17-20.18
									(p-assign @19.5-19.6 (ident "d")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.15 (type "_arg -> Num(_size)"))
		(patt @7.1-7.16 (type "_arg -> _ret"))
		(patt @10.1-10.18 (type "_arg -> Num(_size)"))
		(patt @13.1-13.13 (type "Num(_size) -> Num(_size2)"))
		(patt @15.1-15.6 (type "_arg -> Num(_size)")))
	(expressions
		(expr @4.18-4.24 (type "_arg -> Num(_size)"))
		(expr @7.19-7.34 (type "_arg -> _ret"))
		(expr @10.21-10.35 (type "_arg -> Num(_size)"))
		(expr @13.16-13.35 (type "Num(_size) -> Num(_size2)"))
		(expr @15.9-21.2 (type "_arg -> Num(_size)"))))
~~~
