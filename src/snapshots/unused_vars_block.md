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
    _ignored # Comment 1
     = # Comment 2
      999 # Comment 3

    # Use only the used_var
    result = used_var + 10
    result
}
~~~
# EXPECTED
UNUSED VARIABLE - unused_vars_block.md:5:5:5:15
UNUSED VARIABLE - unused_vars_block.md:11:5:11:19
# PROBLEMS
**UNUSED VARIABLE**
Variable ``unused_var`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused_var` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:5:5:5:15:**
```roc
    unused_var = 42
```
    ^^^^^^^^^^


**UNUSED VARIABLE**
Variable ``another_unused`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_another_unused` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:11:5:11:19:**
```roc
    another_unused = "hello"
```
    ^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:6),OpAssign(3:7-3:8),OpBar(3:9-3:10),Underscore(3:10-3:11),OpBar(3:11-3:12),OpenCurly(3:13-3:14),
LowerIdent(5:5-5:15),OpAssign(5:16-5:17),Int(5:18-5:20),
LowerIdent(8:5-8:13),OpAssign(8:14-8:15),Int(8:16-8:19),
LowerIdent(11:5-11:19),OpAssign(11:20-11:21),StringStart(11:22-11:23),StringPart(11:23-11:28),StringEnd(11:28-11:29),
NamedUnderscore(14:5-14:13),
OpAssign(15:6-15:7),
Int(16:7-16:10),
LowerIdent(19:5-19:11),OpAssign(19:12-19:13),LowerIdent(19:14-19:22),OpPlus(19:23-19:24),Int(19:25-19:27),
LowerIdent(20:5-20:11),
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
		(s-decl @3.1-21.2
			(p-ident @3.1-3.6 (raw "main!"))
			(e-lambda @3.9-21.2
				(args
					(p-underscore))
				(e-block @3.13-21.2
					(statements
						(s-decl @5.5-5.20
							(p-ident @5.5-5.15 (raw "unused_var"))
							(e-int @5.18-5.20 (raw "42")))
						(s-decl @8.5-8.19
							(p-ident @8.5-8.13 (raw "used_var"))
							(e-int @8.16-8.19 (raw "100")))
						(s-decl @11.5-11.29
							(p-ident @11.5-11.19 (raw "another_unused"))
							(e-string @11.22-11.29
								(e-string-part @11.23-11.28 (raw "hello"))))
						(s-decl @14.5-16.10
							(p-ident @14.5-14.13 (raw "_ignored"))
							(e-int @16.7-16.10 (raw "999")))
						(s-decl @19.5-19.27
							(p-ident @19.5-19.11 (raw "result"))
							(e-binop @19.14-19.27 (op "+")
								(e-ident @19.14-19.22 (raw "used_var"))
								(e-int @19.25-19.27 (raw "10"))))
						(e-ident @20.5-20.11 (raw "result"))))))))
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
	_ignored # Comment 1
		= # Comment 2
			999 # Comment 3

	# Use only the used_var
	result = used_var + 10
	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.6 (ident "main!"))
		(e-lambda @3.9-21.2
			(args
				(p-underscore @3.10-3.11))
			(e-block @3.13-21.2
				(s-let @5.5-5.20
					(p-assign @5.5-5.15 (ident "unused_var"))
					(e-int @5.18-5.20 (value "42")))
				(s-let @8.5-8.19
					(p-assign @8.5-8.13 (ident "used_var"))
					(e-int @8.16-8.19 (value "100")))
				(s-let @11.5-11.29
					(p-assign @11.5-11.19 (ident "another_unused"))
					(e-string @11.22-11.29
						(e-literal @11.23-11.28 (string "hello"))))
				(s-let @14.5-16.10
					(p-assign @14.5-14.13 (ident "_ignored"))
					(e-int @16.7-16.10 (value "999")))
				(s-let @19.5-19.27
					(p-assign @19.5-19.11 (ident "result"))
					(e-binop @19.14-19.27 (op "add")
						(e-lookup-local @19.14-19.22
							(p-assign @8.5-8.13 (ident "used_var")))
						(e-int @19.25-19.27 (value "10"))))
				(e-lookup-local @20.5-20.11
					(p-assign @19.5-19.11 (ident "result")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.6 (type "_arg -> _ret")))
	(expressions
		(expr @3.9-21.2 (type "_arg -> _ret"))))
~~~
