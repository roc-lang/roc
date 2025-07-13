# META
~~~ini
description=Test showing type error that would occur if rigid variables were not instantiated
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
    (x, y) = pair
    (y, x)
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = |_| {
    # First use: swap (Int, Str)
    result1 = swap((42, "hello"))
    
    # Second use: swap (Bool, List Int)
    # This would fail if 'a' and 'b' from the first call were reused
    result2 = swap((Bool.true, [1, 2, 3]))
    
    # Third use: swap (Str, Str) 
    # This shows even when both types are the same, we still need fresh vars
    result3 = swap(("foo", "bar"))
    
    {}
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - rigid_var_no_instantiation_error.md:6:12:6:13
UNDEFINED VARIABLE - rigid_var_no_instantiation_error.md:6:6:6:7
UNDEFINED VARIABLE - rigid_var_no_instantiation_error.md:6:9:6:10
UNDEFINED VARIABLE - rigid_var_no_instantiation_error.md:7:6:7:7
UNDEFINED VARIABLE - rigid_var_no_instantiation_error.md:7:9:7:10
UNDEFINED VARIABLE - rigid_var_no_instantiation_error.md:17:21:17:30
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:13:5:13:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:17:5:17:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:21:5:21:12
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **=** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**rigid_var_no_instantiation_error.md:6:12:6:13:**
```roc
    (x, y) = pair
```
           ^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**rigid_var_no_instantiation_error.md:6:6:6:7:**
```roc
    (x, y) = pair
```
     ^


**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**rigid_var_no_instantiation_error.md:6:9:6:10:**
```roc
    (x, y) = pair
```
        ^


**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**rigid_var_no_instantiation_error.md:7:6:7:7:**
```roc
    (y, x)
```
     ^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**rigid_var_no_instantiation_error.md:7:9:7:10:**
```roc
    (y, x)
```
        ^


**UNDEFINED VARIABLE**
Nothing is named `true` in this scope.
Is there an `import` or `exposing` missing up-top?

**rigid_var_no_instantiation_error.md:17:21:17:30:**
```roc
    result2 = swap((Bool.true, [1, 2, 3]))
```
                    ^^^^^^^^^


**UNUSED VARIABLE**
Variable ``result1`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result1` to suppress this warning.
The unused variable is declared here:
**rigid_var_no_instantiation_error.md:13:5:13:12:**
```roc
    result1 = swap((42, "hello"))
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable ``result2`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result2` to suppress this warning.
The unused variable is declared here:
**rigid_var_no_instantiation_error.md:17:5:17:12:**
```roc
    result2 = swap((Bool.true, [1, 2, 3]))
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable ``result3`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:
**rigid_var_no_instantiation_error.md:21:5:21:12:**
```roc
    result3 = swap(("foo", "bar"))
```
    ^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),
LowerIdent(4:1-4:5),OpColon(4:6-4:7),OpenRound(4:8-4:9),LowerIdent(4:9-4:10),Comma(4:10-4:11),LowerIdent(4:12-4:13),CloseRound(4:13-4:14),OpArrow(4:15-4:17),OpenRound(4:18-4:19),LowerIdent(4:19-4:20),Comma(4:20-4:21),LowerIdent(4:22-4:23),CloseRound(4:23-4:24),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpBar(5:8-5:9),LowerIdent(5:9-5:13),OpBar(5:13-5:14),OpenCurly(5:15-5:16),
OpenRound(6:5-6:6),LowerIdent(6:6-6:7),Comma(6:7-6:8),LowerIdent(6:9-6:10),CloseRound(6:10-6:11),OpAssign(6:12-6:13),LowerIdent(6:14-6:18),
OpenRound(7:5-7:6),LowerIdent(7:6-7:7),Comma(7:7-7:8),LowerIdent(7:9-7:10),CloseRound(7:10-7:11),
CloseCurly(8:1-8:2),
LowerIdent(11:1-11:6),OpAssign(11:7-11:8),OpBar(11:9-11:10),Underscore(11:10-11:11),OpBar(11:11-11:12),OpenCurly(11:13-11:14),
LowerIdent(13:5-13:12),OpAssign(13:13-13:14),LowerIdent(13:15-13:19),NoSpaceOpenRound(13:19-13:20),NoSpaceOpenRound(13:20-13:21),Int(13:21-13:23),Comma(13:23-13:24),StringStart(13:25-13:26),StringPart(13:26-13:31),StringEnd(13:31-13:32),CloseRound(13:32-13:33),CloseRound(13:33-13:34),
LowerIdent(17:5-17:12),OpAssign(17:13-17:14),LowerIdent(17:15-17:19),NoSpaceOpenRound(17:19-17:20),NoSpaceOpenRound(17:20-17:21),UpperIdent(17:21-17:25),NoSpaceDotLowerIdent(17:25-17:30),Comma(17:30-17:31),OpenSquare(17:32-17:33),Int(17:33-17:34),Comma(17:34-17:35),Int(17:36-17:37),Comma(17:37-17:38),Int(17:39-17:40),CloseSquare(17:40-17:41),CloseRound(17:41-17:42),CloseRound(17:42-17:43),
LowerIdent(21:5-21:12),OpAssign(21:13-21:14),LowerIdent(21:15-21:19),NoSpaceOpenRound(21:19-21:20),NoSpaceOpenRound(21:20-21:21),StringStart(21:21-21:22),StringPart(21:22-21:25),StringEnd(21:25-21:26),Comma(21:26-21:27),StringStart(21:28-21:29),StringPart(21:29-21:32),StringEnd(21:32-21:33),CloseRound(21:33-21:34),CloseRound(21:34-21:35),
OpenCurly(23:5-23:6),CloseCurly(23:6-23:7),
CloseCurly(24:1-24:2),EndOfFile(24:2-24:2),
~~~
# PARSE
~~~clojure
(file @1.1-24.2
	(app @1.1-1.57
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11 (text "main!")))
		(record-field @1.15-1.55 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.55 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4.1-4.24 (name "swap")
			(ty-fn @4.8-4.24
				(ty-tuple @4.8-4.14
					(ty-var @4.9-4.9 (raw "a"))
					(ty-var @1.1-1.1 (raw "b")))
				(ty-tuple @4.18-4.24
					(ty-var @4.19-4.19 (raw "b"))
					(ty-var @1.1-1.1 (raw "a")))))
		(s-decl @5.1-8.2
			(p-ident @5.1-5.5 (raw "swap"))
			(e-lambda @5.8-8.2
				(args
					(p-ident @5.9-5.13 (raw "pair")))
				(e-block @5.15-8.2
					(statements
						(e-tuple @6.5-6.11
							(e-ident @6.6-6.7 (raw "x"))
							(e-ident @6.9-6.10 (raw "y")))
						(e-malformed @6.12-6.13 (reason "expr_unexpected_token"))
						(e-ident @6.14-6.18 (raw "pair"))
						(e-tuple @7.5-7.11
							(e-ident @7.6-7.7 (raw "y"))
							(e-ident @7.9-7.10 (raw "x")))))))
		(s-decl @11.1-24.2
			(p-ident @11.1-11.6 (raw "main!"))
			(e-lambda @11.9-24.2
				(args
					(p-underscore))
				(e-block @11.13-24.2
					(statements
						(s-decl @13.5-13.34
							(p-ident @13.5-13.12 (raw "result1"))
							(e-apply @13.15-13.34
								(e-ident @13.15-13.19 (raw "swap"))
								(e-tuple @13.20-13.33
									(e-int @13.21-13.23 (raw "42"))
									(e-string @13.25-13.32
										(e-string-part @13.26-13.31 (raw "hello"))))))
						(s-decl @17.5-17.43
							(p-ident @17.5-17.12 (raw "result2"))
							(e-apply @17.15-17.43
								(e-ident @17.15-17.19 (raw "swap"))
								(e-tuple @17.20-17.42
									(e-ident @17.21-17.30 (raw "Bool.true"))
									(e-list @17.32-17.41
										(e-int @17.33-17.34 (raw "1"))
										(e-int @17.36-17.37 (raw "2"))
										(e-int @17.39-17.40 (raw "3"))))))
						(s-decl @21.5-21.35
							(p-ident @21.5-21.12 (raw "result3"))
							(e-apply @21.15-21.35
								(e-ident @21.15-21.19 (raw "swap"))
								(e-tuple @21.20-21.34
									(e-string @21.21-21.26
										(e-string-part @21.22-21.25 (raw "foo")))
									(e-string @21.28-21.33
										(e-string-part @21.29-21.32 (raw "bar"))))))
						(e-record @23.5-23.7)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
	(x, y)
	
	pair
	(y, x)

	# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = |_| {
	# First use: swap (Int, Str)
	result1 = swap((42, "hello"))

	# Second use: swap (Bool, List Int)
	# This would fail if 'a' and 'b' from the first call were reused
	result2 = swap((Bool.true, [1, 2, 3]))

	# Third use: swap (Str, Str) 
	# This shows even when both types are the same, we still need fresh vars
	result3 = swap(("foo", "bar"))

	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "swap"))
		(e-lambda @5.8-8.2
			(args
				(p-assign @5.9-5.13 (ident "pair")))
			(e-block @5.15-8.2
				(s-expr @6.5-6.11
					(e-tuple @6.5-6.11
						(elems
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-runtime-error (tag "ident_not_in_scope")))))
				(s-expr @6.14-6.18
					(e-lookup-local @6.14-6.18
						(p-assign @5.9-5.13 (ident "pair"))))
				(e-tuple @7.5-7.11
					(elems
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope"))))))
		(annotation @5.1-5.5
			(declared-type
				(ty-fn @4.8-4.24 (effectful false)
					(ty-tuple @4.8-4.14
						(ty-var @4.9-4.9 (name "a"))
						(ty-var @1.1-1.1 (name "b")))
					(ty-tuple @4.18-4.24
						(ty-var @4.19-4.19 (name "b"))
						(ty-var @1.1-1.1 (name "a")))))))
	(d-let
		(p-assign @11.1-11.6 (ident "main!"))
		(e-lambda @11.9-24.2
			(args
				(p-underscore @11.10-11.11))
			(e-block @11.13-24.2
				(s-let @13.5-13.34
					(p-assign @13.5-13.12 (ident "result1"))
					(e-call @13.15-13.34
						(e-lookup-local @13.15-13.19
							(p-assign @5.1-5.5 (ident "swap")))
						(e-tuple @13.20-13.33
							(elems
								(e-int @13.21-13.23 (value "42"))
								(e-string @13.25-13.32
									(e-literal @13.26-13.31 (string "hello")))))))
				(s-let @17.5-17.43
					(p-assign @17.5-17.12 (ident "result2"))
					(e-call @17.15-17.43
						(e-lookup-local @17.15-17.19
							(p-assign @5.1-5.5 (ident "swap")))
						(e-tuple @17.20-17.42
							(elems
								(e-runtime-error (tag "ident_not_in_scope"))
								(e-list @17.32-17.41
									(elems
										(e-int @17.33-17.34 (value "1"))
										(e-int @17.36-17.37 (value "2"))
										(e-int @17.39-17.40 (value "3"))))))))
				(s-let @21.5-21.35
					(p-assign @21.5-21.12 (ident "result3"))
					(e-call @21.15-21.35
						(e-lookup-local @21.15-21.19
							(p-assign @5.1-5.5 (ident "swap")))
						(e-tuple @21.20-21.34
							(elems
								(e-string @21.21-21.26
									(e-literal @21.22-21.25 (string "foo")))
								(e-string @21.28-21.33
									(e-literal @21.29-21.32 (string "bar")))))))
				(e-empty_record @23.5-23.7)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "(a, b) -> (Error, Error)"))
		(patt @11.1-11.6 (type "arg -> {}")))
	(expressions
		(expr @5.8-8.2 (type "(a, b) -> (Error, Error)"))
		(expr @11.9-24.2 (type "arg -> {}"))))
~~~
