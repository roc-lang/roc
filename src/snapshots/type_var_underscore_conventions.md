# META
~~~ini
description=Comprehensive test of type variable underscore conventions
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Test 1: UNUSED TYPE VARIABLE NAME - single-use variable should start with underscore
single_use : List(elem) -> Str
single_use = |x| "hello"

# Test 2: TYPE VAR ENDING IN UNDERSCORE - variables should never end with underscore
ending_underscore : List(elem_) -> elem_
ending_underscore = |list| "default"

# Test 3: COMBINATION - single-use ending in underscore (both errors)
combo_single : List(bad_) -> Str
combo_single = |x| "combo"

# Test 4: VALID CASES - these should not generate warnings
valid_single : List(_elem) -> Str
valid_single = |x| "valid"

valid_multi : elem -> List(elem)
valid_multi = |x| [x]

main = |x| "done"
~~~
# EXPECTED
UNUSED VARIABLE - type_var_underscore_conventions.md:5:15:5:16
UNUSED VARIABLE - type_var_underscore_conventions.md:9:22:9:26
UNUSED VARIABLE - type_var_underscore_conventions.md:13:17:13:18
UNUSED VARIABLE - type_var_underscore_conventions.md:17:17:17:18
UNUSED VARIABLE - type_var_underscore_conventions.md:22:9:22:10
# PROBLEMS
**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:5:15:5:16:**
```roc
single_use = |x| "hello"
```
              ^


**UNUSED VARIABLE**
Variable ``list`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:9:22:9:26:**
```roc
ending_underscore = |list| "default"
```
                     ^^^^


**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:13:17:13:18:**
```roc
combo_single = |x| "combo"
```
                ^


**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:17:17:17:18:**
```roc
valid_single = |x| "valid"
```
                ^


**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_underscore_conventions.md:22:9:22:10:**
```roc
main = |x| "done"
```
        ^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
LowerIdent(4:1-4:11),OpColon(4:12-4:13),UpperIdent(4:14-4:18),NoSpaceOpenRound(4:18-4:19),LowerIdent(4:19-4:23),CloseRound(4:23-4:24),OpArrow(4:25-4:27),UpperIdent(4:28-4:31),
LowerIdent(5:1-5:11),OpAssign(5:12-5:13),OpBar(5:14-5:15),LowerIdent(5:15-5:16),OpBar(5:16-5:17),StringStart(5:18-5:19),StringPart(5:19-5:24),StringEnd(5:24-5:25),
LowerIdent(8:1-8:18),OpColon(8:19-8:20),UpperIdent(8:21-8:25),NoSpaceOpenRound(8:25-8:26),LowerIdent(8:26-8:31),CloseRound(8:31-8:32),OpArrow(8:33-8:35),LowerIdent(8:36-8:41),
LowerIdent(9:1-9:18),OpAssign(9:19-9:20),OpBar(9:21-9:22),LowerIdent(9:22-9:26),OpBar(9:26-9:27),StringStart(9:28-9:29),StringPart(9:29-9:36),StringEnd(9:36-9:37),
LowerIdent(12:1-12:13),OpColon(12:14-12:15),UpperIdent(12:16-12:20),NoSpaceOpenRound(12:20-12:21),LowerIdent(12:21-12:25),CloseRound(12:25-12:26),OpArrow(12:27-12:29),UpperIdent(12:30-12:33),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),OpBar(13:16-13:17),LowerIdent(13:17-13:18),OpBar(13:18-13:19),StringStart(13:20-13:21),StringPart(13:21-13:26),StringEnd(13:26-13:27),
LowerIdent(16:1-16:13),OpColon(16:14-16:15),UpperIdent(16:16-16:20),NoSpaceOpenRound(16:20-16:21),NamedUnderscore(16:21-16:26),CloseRound(16:26-16:27),OpArrow(16:28-16:30),UpperIdent(16:31-16:34),
LowerIdent(17:1-17:13),OpAssign(17:14-17:15),OpBar(17:16-17:17),LowerIdent(17:17-17:18),OpBar(17:18-17:19),StringStart(17:20-17:21),StringPart(17:21-17:26),StringEnd(17:26-17:27),
LowerIdent(19:1-19:12),OpColon(19:13-19:14),LowerIdent(19:15-19:19),OpArrow(19:20-19:22),UpperIdent(19:23-19:27),NoSpaceOpenRound(19:27-19:28),LowerIdent(19:28-19:32),CloseRound(19:32-19:33),
LowerIdent(20:1-20:12),OpAssign(20:13-20:14),OpBar(20:15-20:16),LowerIdent(20:16-20:17),OpBar(20:17-20:18),OpenSquare(20:19-20:20),LowerIdent(20:20-20:21),CloseSquare(20:21-20:22),
LowerIdent(22:1-22:5),OpAssign(22:6-22:7),OpBar(22:8-22:9),LowerIdent(22:9-22:10),OpBar(22:10-22:11),StringStart(22:12-22:13),StringPart(22:13-22:17),StringEnd(22:17-22:18),EndOfFile(22:18-22:18),
~~~
# PARSE
~~~clojure
(file @1.1-22.18
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4.1-4.31 (name "single_use")
			(ty-fn @4.14-4.31
				(ty-apply @4.14-4.24
					(ty @4.14-4.18 (name "List"))
					(ty-var @4.19-4.23 (raw "elem")))
				(ty @4.28-4.31 (name "Str"))))
		(s-decl @5.1-5.25
			(p-ident @5.1-5.11 (raw "single_use"))
			(e-lambda @5.14-5.25
				(args
					(p-ident @5.15-5.16 (raw "x")))
				(e-string @5.18-5.25
					(e-string-part @5.19-5.24 (raw "hello")))))
		(s-type-anno @8.1-8.41 (name "ending_underscore")
			(ty-fn @8.21-8.41
				(ty-apply @8.21-8.32
					(ty @8.21-8.25 (name "List"))
					(ty-var @8.26-8.31 (raw "elem_")))
				(ty-var @8.36-8.41 (raw "elem_"))))
		(s-decl @9.1-9.37
			(p-ident @9.1-9.18 (raw "ending_underscore"))
			(e-lambda @9.21-9.37
				(args
					(p-ident @9.22-9.26 (raw "list")))
				(e-string @9.28-9.37
					(e-string-part @9.29-9.36 (raw "default")))))
		(s-type-anno @12.1-12.33 (name "combo_single")
			(ty-fn @12.16-12.33
				(ty-apply @12.16-12.26
					(ty @12.16-12.20 (name "List"))
					(ty-var @12.21-12.25 (raw "bad_")))
				(ty @12.30-12.33 (name "Str"))))
		(s-decl @13.1-13.27
			(p-ident @13.1-13.13 (raw "combo_single"))
			(e-lambda @13.16-13.27
				(args
					(p-ident @13.17-13.18 (raw "x")))
				(e-string @13.20-13.27
					(e-string-part @13.21-13.26 (raw "combo")))))
		(s-type-anno @16.1-16.34 (name "valid_single")
			(ty-fn @16.16-16.34
				(ty-apply @16.16-16.27
					(ty @16.16-16.20 (name "List"))
					(underscore-ty-var @16.21-16.26 (raw "_elem")))
				(ty @16.31-16.34 (name "Str"))))
		(s-decl @17.1-17.27
			(p-ident @17.1-17.13 (raw "valid_single"))
			(e-lambda @17.16-17.27
				(args
					(p-ident @17.17-17.18 (raw "x")))
				(e-string @17.20-17.27
					(e-string-part @17.21-17.26 (raw "valid")))))
		(s-type-anno @19.1-19.33 (name "valid_multi")
			(ty-fn @19.15-19.33
				(ty-var @19.15-19.19 (raw "elem"))
				(ty-apply @19.23-19.33
					(ty @19.23-19.27 (name "List"))
					(ty-var @19.28-19.32 (raw "elem")))))
		(s-decl @20.1-20.22
			(p-ident @20.1-20.12 (raw "valid_multi"))
			(e-lambda @20.15-20.22
				(args
					(p-ident @20.16-20.17 (raw "x")))
				(e-list @20.19-20.22
					(e-ident @20.20-20.21 (raw "x")))))
		(s-decl @22.1-22.18
			(p-ident @22.1-22.5 (raw "main"))
			(e-lambda @22.8-22.18
				(args
					(p-ident @22.9-22.10 (raw "x")))
				(e-string @22.12-22.18
					(e-string-part @22.13-22.17 (raw "done")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.11 (ident "single_use"))
		(e-lambda @5.14-5.25
			(args
				(p-assign @5.15-5.16 (ident "x")))
			(e-string @5.18-5.25
				(e-literal @5.19-5.24 (string "hello"))))
		(annotation @5.1-5.11
			(declared-type
				(ty-fn @4.14-4.31 (effectful false)
					(ty-apply @4.14-4.24 (symbol "List")
						(ty-var @4.19-4.23 (name "elem")))
					(ty @4.28-4.31 (name "Str"))))))
	(d-let
		(p-assign @9.1-9.18 (ident "ending_underscore"))
		(e-lambda @9.21-9.37
			(args
				(p-assign @9.22-9.26 (ident "list")))
			(e-string @9.28-9.37
				(e-literal @9.29-9.36 (string "default"))))
		(annotation @9.1-9.18
			(declared-type
				(ty-fn @8.21-8.41 (effectful false)
					(ty-apply @8.21-8.32 (symbol "List")
						(ty-var @8.26-8.31 (name "elem_")))
					(ty-var @8.36-8.41 (name "elem_"))))))
	(d-let
		(p-assign @13.1-13.13 (ident "combo_single"))
		(e-lambda @13.16-13.27
			(args
				(p-assign @13.17-13.18 (ident "x")))
			(e-string @13.20-13.27
				(e-literal @13.21-13.26 (string "combo"))))
		(annotation @13.1-13.13
			(declared-type
				(ty-fn @12.16-12.33 (effectful false)
					(ty-apply @12.16-12.26 (symbol "List")
						(ty-var @12.21-12.25 (name "bad_")))
					(ty @12.30-12.33 (name "Str"))))))
	(d-let
		(p-assign @17.1-17.13 (ident "valid_single"))
		(e-lambda @17.16-17.27
			(args
				(p-assign @17.17-17.18 (ident "x")))
			(e-string @17.20-17.27
				(e-literal @17.21-17.26 (string "valid"))))
		(annotation @17.1-17.13
			(declared-type
				(ty-fn @16.16-16.34 (effectful false)
					(ty-apply @16.16-16.27 (symbol "List")
						(ty-var @16.21-16.26 (name "_elem")))
					(ty @16.31-16.34 (name "Str"))))))
	(d-let
		(p-assign @20.1-20.12 (ident "valid_multi"))
		(e-lambda @20.15-20.22
			(args
				(p-assign @20.16-20.17 (ident "x")))
			(e-list @20.19-20.22
				(elems
					(e-lookup-local @20.20-20.21
						(p-assign @20.16-20.17 (ident "x"))))))
		(annotation @20.1-20.12
			(declared-type
				(ty-fn @19.15-19.33 (effectful false)
					(ty-var @19.15-19.19 (name "elem"))
					(ty-apply @19.23-19.33 (symbol "List")
						(ty-var @19.28-19.32 (name "elem")))))))
	(d-let
		(p-assign @22.1-22.5 (ident "main"))
		(e-lambda @22.8-22.18
			(args
				(p-assign @22.9-22.10 (ident "x")))
			(e-string @22.12-22.18
				(e-literal @22.13-22.17 (string "done"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.11 (type "Error -> Str"))
		(patt @9.1-9.18 (type "Error -> Str"))
		(patt @13.1-13.13 (type "Error -> Str"))
		(patt @17.1-17.13 (type "Error -> Str"))
		(patt @20.1-20.12 (type "elem -> Error"))
		(patt @22.1-22.5 (type "_arg -> Str")))
	(expressions
		(expr @5.14-5.25 (type "Error -> Str"))
		(expr @9.21-9.37 (type "Error -> Str"))
		(expr @13.16-13.27 (type "Error -> Str"))
		(expr @17.16-17.27 (type "Error -> Str"))
		(expr @20.15-20.22 (type "elem -> Error"))
		(expr @22.8-22.18 (type "_arg -> Str"))))
~~~
