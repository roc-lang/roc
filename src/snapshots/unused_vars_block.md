# META
~~~ini
description=Block variables with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
    # Regular unused variable - should warn
    unused_var = 42

    # Regular used variable - should be fine
    used_var = 100

    # Another unused variable - should warn
    another_unused = "hello"

    # Underscore variable that is unused - should be fine
    _ignored = 999

    # Use only the used_var
    result = used_var + 10
    result
}
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``unused_var`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused_var` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:5:5:5:15:**
```roc
    unused_var = 42
```


**UNUSED VARIABLE**
Variable ``another_unused`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_another_unused` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:11:5:11:19:**
```roc
    another_unused = "hello"
```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:6),OpAssign(3:7-3:8),OpBar(3:9-3:10),Underscore(3:10-3:11),OpBar(3:11-3:12),OpenCurly(3:13-3:14),Newline(1:1-1:1),
Newline(4:6-4:44),
LowerIdent(5:5-5:15),OpAssign(5:16-5:17),Int(5:18-5:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:6-7:45),
LowerIdent(8:5-8:13),OpAssign(8:14-8:15),Int(8:16-8:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(10:6-10:44),
LowerIdent(11:5-11:19),OpAssign(11:20-11:21),StringStart(11:22-11:23),StringPart(11:23-11:28),StringEnd(11:28-11:29),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(13:6-13:58),
NamedUnderscore(14:5-14:13),OpAssign(14:14-14:15),Int(14:16-14:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(16:6-16:28),
LowerIdent(17:5-17:11),OpAssign(17:12-17:13),LowerIdent(17:14-17:22),OpPlus(17:23-17:24),Int(17:25-17:27),Newline(1:1-1:1),
LowerIdent(18:5-18:11),Newline(1:1-1:1),
CloseCurly(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(file @1-1-19-2
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
		(s-decl @3-1-19-2
			(p-ident @3-1-3-6 (raw "main!"))
			(e-lambda @3-9-19-2
				(args
					(p-underscore))
				(e-block @3-13-19-2
					(statements
						(s-decl @5-5-5-20
							(p-ident @5-5-5-15 (raw "unused_var"))
							(e-int @5-18-5-20 (raw "42")))
						(s-decl @8-5-8-19
							(p-ident @8-5-8-13 (raw "used_var"))
							(e-int @8-16-8-19 (raw "100")))
						(s-decl @11-5-11-29
							(p-ident @11-5-11-19 (raw "another_unused"))
							(e-string @11-22-11-29
								(e-string-part @11-23-11-28 (raw "hello"))))
						(s-decl @14-5-14-19
							(p-ident @14-5-14-13 (raw "_ignored"))
							(e-int @14-16-14-19 (raw "999")))
						(s-decl @17-5-18-11
							(p-ident @17-5-17-11 (raw "result"))
							(e-binop @17-14-18-11 (op "+")
								(e-ident @17-14-17-22 (qaul "") (raw "used_var"))
								(e-int @17-25-17-27 (raw "10"))))
						(e-ident @18-5-18-11 (qaul "") (raw "result"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
	# Regular unused variable - should warn
	unused_var = 42

	# Regular used variable - should be fine
	used_var = 100

	# Another unused variable - should warn
	another_unused = "hello"

	# Underscore variable that is unused - should be fine
	_ignored = 999

	# Use only the used_var
	result = used_var + 10
	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 105)
		(p-assign @3-1-3-6 (ident "main!") (id 72))
		(e-lambda @3-9-19-2 (id 104)
			(args
				(p-underscore @3-10-3-11 (id 73)))
			(e-block @3-13-19-2
				(s-let @5-5-5-20
					(p-assign @5-5-5-15 (ident "unused_var") (id 74))
					(e-int @5-18-5-20 (num-var 77) (sign-needed "false") (bits-needed "7") (value "42") (id 77)))
				(s-let @8-5-8-19
					(p-assign @8-5-8-13 (ident "used_var") (id 79))
					(e-int @8-16-8-19 (num-var 82) (sign-needed "false") (bits-needed "7") (value "100") (id 82)))
				(s-let @11-5-11-29
					(p-assign @11-5-11-19 (ident "another_unused") (id 84))
					(e-string @11-22-11-29 (id 86)
						(e-literal @11-23-11-28 (string "hello"))))
				(s-let @14-5-14-19
					(p-assign @14-5-14-13 (ident "_ignored") (id 88))
					(e-int @14-16-14-19 (num-var 91) (sign-needed "false") (bits-needed "9_to_15") (value "999") (id 91)))
				(s-let @17-5-18-11
					(p-assign @17-5-17-11 (ident "result") (id 93))
					(e-binop @17-14-18-11 (op "add") (id 98)
						(e-lookup-local @17-14-17-22
							(pattern (id 79)))
						(e-int @17-25-17-27 (num-var 97) (sign-needed "false") (bits-needed "7") (value "10"))))
				(e-lookup-local @18-5-18-11
					(pattern (id 93)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "main!") (type "*")))
	(expressions
		(expr @3-9-19-2 (type "*"))))
~~~