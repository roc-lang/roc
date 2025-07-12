# META
~~~ini
description=Complex nested type applications in function annotation - focused test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test complex nested type applications in function signatures
processComplex : Result(List(Maybe(a)), Dict(Str, Error(b))) -> List(a)
processComplex = |result|
    match result {
        Ok(maybeList) => []
        Err(_) => []
    }

# Test multiple levels of nesting
deepNested : Maybe(Result(List(Dict(Str, a)), b)) -> a
deepNested = |_| crash "not implemented"

# Test type alias with complex nesting
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))

main! = |_| processComplex(Ok([Some(42), None]))
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_app_complex_nested.md:13:18:13:23
UNDECLARED TYPE - type_app_complex_nested.md:16:33:16:38
UNDECLARED TYPE - type_app_complex_nested.md:16:54:16:59
UNDECLARED TYPE - type_app_complex_nested.md:4:30:4:35
UNDECLARED TYPE - type_app_complex_nested.md:4:51:4:56
UNUSED VARIABLE - type_app_complex_nested.md:7:12:7:21
UNDECLARED TYPE - type_app_complex_nested.md:12:14:12:19
INVALID STATEMENT - type_app_complex_nested.md:13:24:13:41
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **crash** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_app_complex_nested.md:13:18:13:23:**
```roc
deepNested = |_| crash "not implemented"
```
                 ^^^^^


**UNDECLARED TYPE**
The type ``Maybe`` is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:16:33:16:38:**
```roc
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))
```
                                ^^^^^


**UNDECLARED TYPE**
The type ``Error`` is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:16:54:16:59:**
```roc
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))
```
                                                     ^^^^^


**UNDECLARED TYPE**
The type ``Maybe`` is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:4:30:4:35:**
```roc
processComplex : Result(List(Maybe(a)), Dict(Str, Error(b))) -> List(a)
```
                             ^^^^^


**UNDECLARED TYPE**
The type ``Error`` is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:4:51:4:56:**
```roc
processComplex : Result(List(Maybe(a)), Dict(Str, Error(b))) -> List(a)
```
                                                  ^^^^^


**UNUSED VARIABLE**
Variable ``maybeList`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_maybeList` to suppress this warning.
The unused variable is declared here:
**type_app_complex_nested.md:7:12:7:21:**
```roc
        Ok(maybeList) => []
```
           ^^^^^^^^^


**UNDECLARED TYPE**
The type ``Maybe`` is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:12:14:12:19:**
```roc
deepNested : Maybe(Result(List(Dict(Str, a)), b)) -> a
```
             ^^^^^


**INVALID LAMBDA**
The body of this lambda expression is not valid.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_app_complex_nested.md:13:24:13:41:**
```roc
deepNested = |_| crash "not implemented"
```
                       ^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(4:1-4:15),OpColon(4:16-4:17),UpperIdent(4:18-4:24),NoSpaceOpenRound(4:24-4:25),UpperIdent(4:25-4:29),NoSpaceOpenRound(4:29-4:30),UpperIdent(4:30-4:35),NoSpaceOpenRound(4:35-4:36),LowerIdent(4:36-4:37),CloseRound(4:37-4:38),CloseRound(4:38-4:39),Comma(4:39-4:40),UpperIdent(4:41-4:45),NoSpaceOpenRound(4:45-4:46),UpperIdent(4:46-4:49),Comma(4:49-4:50),UpperIdent(4:51-4:56),NoSpaceOpenRound(4:56-4:57),LowerIdent(4:57-4:58),CloseRound(4:58-4:59),CloseRound(4:59-4:60),CloseRound(4:60-4:61),OpArrow(4:62-4:64),UpperIdent(4:65-4:69),NoSpaceOpenRound(4:69-4:70),LowerIdent(4:70-4:71),CloseRound(4:71-4:72),
LowerIdent(5:1-5:15),OpAssign(5:16-5:17),OpBar(5:18-5:19),LowerIdent(5:19-5:25),OpBar(5:25-5:26),
KwMatch(6:5-6:10),LowerIdent(6:11-6:17),OpenCurly(6:18-6:19),
UpperIdent(7:9-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:21),CloseRound(7:21-7:22),OpFatArrow(7:23-7:25),OpenSquare(7:26-7:27),CloseSquare(7:27-7:28),
UpperIdent(8:9-8:12),NoSpaceOpenRound(8:12-8:13),Underscore(8:13-8:14),CloseRound(8:14-8:15),OpFatArrow(8:16-8:18),OpenSquare(8:19-8:20),CloseSquare(8:20-8:21),
CloseCurly(9:5-9:6),
LowerIdent(12:1-12:11),OpColon(12:12-12:13),UpperIdent(12:14-12:19),NoSpaceOpenRound(12:19-12:20),UpperIdent(12:20-12:26),NoSpaceOpenRound(12:26-12:27),UpperIdent(12:27-12:31),NoSpaceOpenRound(12:31-12:32),UpperIdent(12:32-12:36),NoSpaceOpenRound(12:36-12:37),UpperIdent(12:37-12:40),Comma(12:40-12:41),LowerIdent(12:42-12:43),CloseRound(12:43-12:44),CloseRound(12:44-12:45),Comma(12:45-12:46),LowerIdent(12:47-12:48),CloseRound(12:48-12:49),CloseRound(12:49-12:50),OpArrow(12:51-12:53),LowerIdent(12:54-12:55),
LowerIdent(13:1-13:11),OpAssign(13:12-13:13),OpBar(13:14-13:15),Underscore(13:15-13:16),OpBar(13:16-13:17),KwCrash(13:18-13:23),StringStart(13:24-13:25),StringPart(13:25-13:40),StringEnd(13:40-13:41),
UpperIdent(16:1-16:12),NoSpaceOpenRound(16:12-16:13),LowerIdent(16:13-16:14),Comma(16:14-16:15),LowerIdent(16:16-16:17),CloseRound(16:17-16:18),OpColon(16:19-16:20),UpperIdent(16:21-16:27),NoSpaceOpenRound(16:27-16:28),UpperIdent(16:28-16:32),NoSpaceOpenRound(16:32-16:33),UpperIdent(16:33-16:38),NoSpaceOpenRound(16:38-16:39),LowerIdent(16:39-16:40),CloseRound(16:40-16:41),CloseRound(16:41-16:42),Comma(16:42-16:43),UpperIdent(16:44-16:48),NoSpaceOpenRound(16:48-16:49),UpperIdent(16:49-16:52),Comma(16:52-16:53),UpperIdent(16:54-16:59),NoSpaceOpenRound(16:59-16:60),LowerIdent(16:60-16:61),CloseRound(16:61-16:62),CloseRound(16:62-16:63),CloseRound(16:63-16:64),
LowerIdent(18:1-18:6),OpAssign(18:7-18:8),OpBar(18:9-18:10),Underscore(18:10-18:11),OpBar(18:11-18:12),LowerIdent(18:13-18:27),NoSpaceOpenRound(18:27-18:28),UpperIdent(18:28-18:30),NoSpaceOpenRound(18:30-18:31),OpenSquare(18:31-18:32),UpperIdent(18:32-18:36),NoSpaceOpenRound(18:36-18:37),Int(18:37-18:39),CloseRound(18:39-18:40),Comma(18:40-18:41),UpperIdent(18:42-18:46),CloseSquare(18:46-18:47),CloseRound(18:47-18:48),CloseRound(18:48-18:49),EndOfFile(18:49-18:49),
~~~
# PARSE
~~~clojure
(file @1.1-18.49
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11 (text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @4.1-4.72 (name "processComplex")
			(ty-fn @4.18-4.72
				(ty-apply @4.18-4.61
					(ty @4.18-4.24 (name "Result"))
					(ty-apply @4.25-4.39
						(ty @4.25-4.29 (name "List"))
						(ty-apply @4.30-4.38
							(ty @4.30-4.35 (name "Maybe"))
							(ty-var @4.36-4.36 (raw "a"))))
					(ty-apply @4.41-4.60
						(ty @4.41-4.45 (name "Dict"))
						(ty @4.46-4.49 (name "Str"))
						(ty-apply @4.51-4.59
							(ty @4.51-4.56 (name "Error"))
							(ty-var @4.57-4.57 (raw "b")))))
				(ty-apply @4.65-4.72
					(ty @4.65-4.69 (name "List"))
					(ty-var @4.70-4.70 (raw "a")))))
		(s-decl @5.1-9.6
			(p-ident @5.1-5.15 (raw "processComplex"))
			(e-lambda @5.18-9.6
				(args
					(p-ident @5.19-5.25 (raw "result")))
				(e-match
					(e-ident @6.11-6.17 (raw "result"))
					(branches
						(branch @7.9-7.28
							(p-tag @7.9-7.22 (raw "Ok")
								(p-ident @7.12-7.21 (raw "maybeList")))
							(e-list @7.26-7.28))
						(branch @8.9-8.21
							(p-tag @8.9-8.15 (raw "Err")
								(p-underscore))
							(e-list @8.19-8.21))))))
		(s-type-anno @12.1-12.55 (name "deepNested")
			(ty-fn @12.14-12.55
				(ty-apply @12.14-12.50
					(ty @12.14-12.19 (name "Maybe"))
					(ty-apply @12.20-12.49
						(ty @12.20-12.26 (name "Result"))
						(ty-apply @12.27-12.45
							(ty @12.27-12.31 (name "List"))
							(ty-apply @12.32-12.44
								(ty @12.32-12.36 (name "Dict"))
								(ty @12.37-12.40 (name "Str"))
								(ty-var @1.1-1.1 (raw "a"))))
						(ty-var @1.1-1.1 (raw "b"))))
				(ty-var @1.1-1.1 (raw "a"))))
		(s-decl @13.1-13.23
			(p-ident @13.1-13.11 (raw "deepNested"))
			(e-lambda @13.14-13.23
				(args
					(p-underscore))
				(e-malformed @13.18-13.23 (reason "expr_unexpected_token"))))
		(e-string @13.24-13.41
			(e-string-part @13.25-13.40 (raw "not implemented")))
		(s-type-decl @16.1-16.64
			(header @16.1-16.18 (name "ComplexType")
				(args
					(ty-var @16.13-16.14 (raw "a"))
					(ty-var @16.16-16.17 (raw "b"))))
			(ty-apply @16.21-16.64
				(ty @16.21-16.27 (name "Result"))
				(ty-apply @16.28-16.42
					(ty @16.28-16.32 (name "List"))
					(ty-apply @16.33-16.41
						(ty @16.33-16.38 (name "Maybe"))
						(ty-var @16.39-16.39 (raw "a"))))
				(ty-apply @16.44-16.63
					(ty @16.44-16.48 (name "Dict"))
					(ty @16.49-16.52 (name "Str"))
					(ty-apply @16.54-16.62
						(ty @16.54-16.59 (name "Error"))
						(ty-var @16.60-16.60 (raw "b"))))))
		(s-decl @18.1-18.49
			(p-ident @18.1-18.6 (raw "main!"))
			(e-lambda @18.9-18.49
				(args
					(p-underscore))
				(e-apply @18.13-18.49
					(e-ident @18.13-18.27 (raw "processComplex"))
					(e-apply @18.28-18.48
						(e-tag @18.28-18.30 (raw "Ok"))
						(e-list @18.31-18.47
							(e-apply @18.32-18.40
								(e-tag @18.32-18.36 (raw "Some"))
								(e-int @18.37-18.39 (raw "42")))
							(e-tag @18.42-18.46 (raw "None")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test complex nested type applications in function signatures
processComplex : Result(List(Maybe(a)), Dict(Str, Error(b))) -> List(a)
processComplex = |result|
	match result {
		Ok(maybeList) => []
		Err(_) => []
	}

# Test multiple levels of nesting
deepNested : Maybe(Result(List(Dict(Str, a)), b)) -> a
deepNested = |_| 
"not implemented"

# Test type alias with complex nesting
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))

main! = |_| processComplex(Ok([Some(42), None]))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.15 (ident "processComplex"))
		(e-lambda @5.18-9.6
			(args
				(p-assign @5.19-5.25 (ident "result")))
			(e-match @6.5-9.6
				(match @6.5-9.6
					(cond
						(e-lookup-local @6.11-6.17
							(p-assign @5.19-5.25 (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @7.9-7.22)))
							(value
								(e-empty_list @7.26-7.28)))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @8.9-8.15)))
							(value
								(e-empty_list @8.19-8.21)))))))
		(annotation @5.1-5.15
			(declared-type
				(ty-fn @4.18-4.72 (effectful false)
					(ty-apply @4.18-4.61 (symbol "Result")
						(ty-apply @4.25-4.39 (symbol "List")
							(ty-apply @4.30-4.38 (symbol "Maybe")
								(ty-var @4.36-4.36 (name "a"))))
						(ty-apply @4.41-4.60 (symbol "Dict")
							(ty @4.46-4.49 (name "Str"))
							(ty-apply @4.51-4.59 (symbol "Error")
								(ty-var @4.57-4.57 (name "b")))))
					(ty-apply @4.65-4.72 (symbol "List")
						(ty-var @4.70-4.70 (name "a")))))))
	(d-let
		(p-assign @13.1-13.11 (ident "deepNested"))
		(e-lambda @13.14-13.23
			(args
				(p-underscore @13.15-13.16))
			(e-runtime-error (tag "lambda_body_not_canonicalized")))
		(annotation @13.1-13.11
			(declared-type
				(ty-fn @12.14-12.55 (effectful false)
					(ty-apply @12.14-12.50 (symbol "Maybe")
						(ty-apply @12.20-12.49 (symbol "Result")
							(ty-apply @12.27-12.45 (symbol "List")
								(ty-apply @12.32-12.44 (symbol "Dict")
									(ty @12.37-12.40 (name "Str"))
									(ty-var @1.1-1.1 (name "a"))))
							(ty-var @1.1-1.1 (name "b"))))
					(ty-var @1.1-1.1 (name "a"))))))
	(d-let
		(p-assign @18.1-18.6 (ident "main!"))
		(e-lambda @18.9-18.49
			(args
				(p-underscore @18.10-18.11))
			(e-call @18.13-18.49
				(e-lookup-local @18.13-18.27
					(p-assign @5.1-5.15 (ident "processComplex")))
				(e-tag @18.28-18.30 (name "Ok")
					(args
						(e-list @18.31-18.47
							(elems
								(e-tag @18.32-18.36 (name "Some")
									(args
										(e-int @18.37-18.39 (value "42"))))
								(e-tag @18.42-18.46 (name "None")))))))))
	(s-alias-decl @16.1-16.64
		(ty-header @16.1-16.18 (name "ComplexType")
			(ty-args
				(ty-var @16.13-16.14 (name "a"))
				(ty-var @16.16-16.17 (name "b"))))
		(ty-apply @16.21-16.64 (symbol "Result")
			(ty-apply @16.28-16.42 (symbol "List")
				(ty-apply @16.33-16.41 (symbol "Maybe")
					(ty-var @16.39-16.39 (name "a"))))
			(ty-apply @16.44-16.63 (symbol "Dict")
				(ty @16.49-16.52 (name "Str"))
				(ty-apply @16.54-16.62 (symbol "Error")
					(ty-var @16.60-16.60 (name "b")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.15 (type "Error -> Error"))
		(patt @13.1-13.11 (type "Error -> Error"))
		(patt @18.1-18.6 (type "* -> Error")))
	(type_decls
		(alias @16.1-16.64 (type "ComplexType(a, b)")
			(ty-header @16.1-16.18 (name "ComplexType")
				(ty-args
					(ty-var @16.13-16.14 (name "a"))
					(ty-var @16.16-16.17 (name "b"))))))
	(expressions
		(expr @5.18-9.6 (type "Error -> Error"))
		(expr @13.14-13.23 (type "Error -> Error"))
		(expr @18.9-18.49 (type "* -> Error"))))
~~~
