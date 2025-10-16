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
Variable `unused_var` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused_var` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:5:5:5:15:**
```roc
    unused_var = 42
```
    ^^^^^^^^^^


**UNUSED VARIABLE**
Variable `another_unused` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_another_unused` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:11:5:11:19:**
```roc
    another_unused = "hello"
```
    ^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
NamedUnderscore,
OpAssign,
Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "unused_var"))
							(e-int (raw "42")))
						(s-decl
							(p-ident (raw "used_var"))
							(e-int (raw "100")))
						(s-decl
							(p-ident (raw "another_unused"))
							(e-string
								(e-string-part (raw "hello"))))
						(s-decl
							(p-ident (raw "_ignored"))
							(e-int (raw "999")))
						(s-decl
							(p-ident (raw "result"))
							(e-binop (op "+")
								(e-ident (raw "used_var"))
								(e-int (raw "10"))))
						(e-ident (raw "result"))))))))
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
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "unused_var"))
					(e-num (value "42")))
				(s-let
					(p-assign (ident "used_var"))
					(e-num (value "100")))
				(s-let
					(p-assign (ident "another_unused"))
					(e-string
						(e-literal (string "hello"))))
				(s-let
					(p-assign (ident "_ignored"))
					(e-num (value "999")))
				(s-let
					(p-assign (ident "result"))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "used_var")))
						(e-num (value "10"))))
				(e-lookup-local
					(p-assign (ident "result")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Num(_size)")))
	(expressions
		(expr (type "_arg -> Num(_size)"))))
~~~
