# META
~~~ini
description=Complex nested type applications in function annotation - focused test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test complex nested type applications in function signatures
processComplex : Result(List(Maybe(a)), Dict(Str, Error(_b))) -> List(a)
processComplex = |result|
    match result {
        Ok(maybeList) => []
        Err(_) => []
    }

# Test multiple levels of nesting
deepNested : Maybe(Result(List(Dict(Str, a)), _b)) -> a
deepNested = |_| {
	crash "not implemented"
}

# Test type alias with complex nesting
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))

main! = |_| processComplex(Ok([Some(42), None]))
~~~
# EXPECTED
UNDECLARED TYPE - type_app_complex_nested.md:18:33:18:38
UNDECLARED TYPE - type_app_complex_nested.md:18:44:18:48
UNDECLARED TYPE - type_app_complex_nested.md:18:54:18:59
UNDECLARED TYPE - type_app_complex_nested.md:4:30:4:35
UNDECLARED TYPE - type_app_complex_nested.md:4:41:4:45
UNDECLARED TYPE - type_app_complex_nested.md:4:51:4:56
UNUSED VARIABLE - type_app_complex_nested.md:7:12:7:21
UNDECLARED TYPE - type_app_complex_nested.md:12:14:12:19
UNDECLARED TYPE - type_app_complex_nested.md:12:32:12:36
# PROBLEMS
**UNDECLARED TYPE**
The type _Maybe_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:18:33:18:38:**
```roc
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))
```
                                ^^^^^


**UNDECLARED TYPE**
The type _Dict_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:18:44:18:48:**
```roc
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))
```
                                           ^^^^


**UNDECLARED TYPE**
The type _Error_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:18:54:18:59:**
```roc
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))
```
                                                     ^^^^^


**UNDECLARED TYPE**
The type _Maybe_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:4:30:4:35:**
```roc
processComplex : Result(List(Maybe(a)), Dict(Str, Error(_b))) -> List(a)
```
                             ^^^^^


**UNDECLARED TYPE**
The type _Dict_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:4:41:4:45:**
```roc
processComplex : Result(List(Maybe(a)), Dict(Str, Error(_b))) -> List(a)
```
                                        ^^^^


**UNDECLARED TYPE**
The type _Error_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:4:51:4:56:**
```roc
processComplex : Result(List(Maybe(a)), Dict(Str, Error(_b))) -> List(a)
```
                                                  ^^^^^


**UNUSED VARIABLE**
Variable `maybeList` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_maybeList` to suppress this warning.
The unused variable is declared here:
**type_app_complex_nested.md:7:12:7:21:**
```roc
        Ok(maybeList) => []
```
           ^^^^^^^^^


**UNDECLARED TYPE**
The type _Maybe_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:12:14:12:19:**
```roc
deepNested : Maybe(Result(List(Dict(Str, a)), _b)) -> a
```
             ^^^^^


**UNDECLARED TYPE**
The type _Dict_ is not declared in this scope.

This type is referenced here:
**type_app_complex_nested.md:12:32:12:36:**
```roc
deepNested : Maybe(Result(List(Dict(Str, a)), _b)) -> a
```
                               ^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(4:1-4:15),OpColon(4:16-4:17),UpperIdent(4:18-4:24),NoSpaceOpenRound(4:24-4:25),UpperIdent(4:25-4:29),NoSpaceOpenRound(4:29-4:30),UpperIdent(4:30-4:35),NoSpaceOpenRound(4:35-4:36),LowerIdent(4:36-4:37),CloseRound(4:37-4:38),CloseRound(4:38-4:39),Comma(4:39-4:40),UpperIdent(4:41-4:45),NoSpaceOpenRound(4:45-4:46),UpperIdent(4:46-4:49),Comma(4:49-4:50),UpperIdent(4:51-4:56),NoSpaceOpenRound(4:56-4:57),NamedUnderscore(4:57-4:59),CloseRound(4:59-4:60),CloseRound(4:60-4:61),CloseRound(4:61-4:62),OpArrow(4:63-4:65),UpperIdent(4:66-4:70),NoSpaceOpenRound(4:70-4:71),LowerIdent(4:71-4:72),CloseRound(4:72-4:73),
LowerIdent(5:1-5:15),OpAssign(5:16-5:17),OpBar(5:18-5:19),LowerIdent(5:19-5:25),OpBar(5:25-5:26),
KwMatch(6:5-6:10),LowerIdent(6:11-6:17),OpenCurly(6:18-6:19),
UpperIdent(7:9-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:21),CloseRound(7:21-7:22),OpFatArrow(7:23-7:25),OpenSquare(7:26-7:27),CloseSquare(7:27-7:28),
UpperIdent(8:9-8:12),NoSpaceOpenRound(8:12-8:13),Underscore(8:13-8:14),CloseRound(8:14-8:15),OpFatArrow(8:16-8:18),OpenSquare(8:19-8:20),CloseSquare(8:20-8:21),
CloseCurly(9:5-9:6),
LowerIdent(12:1-12:11),OpColon(12:12-12:13),UpperIdent(12:14-12:19),NoSpaceOpenRound(12:19-12:20),UpperIdent(12:20-12:26),NoSpaceOpenRound(12:26-12:27),UpperIdent(12:27-12:31),NoSpaceOpenRound(12:31-12:32),UpperIdent(12:32-12:36),NoSpaceOpenRound(12:36-12:37),UpperIdent(12:37-12:40),Comma(12:40-12:41),LowerIdent(12:42-12:43),CloseRound(12:43-12:44),CloseRound(12:44-12:45),Comma(12:45-12:46),NamedUnderscore(12:47-12:49),CloseRound(12:49-12:50),CloseRound(12:50-12:51),OpArrow(12:52-12:54),LowerIdent(12:55-12:56),
LowerIdent(13:1-13:11),OpAssign(13:12-13:13),OpBar(13:14-13:15),Underscore(13:15-13:16),OpBar(13:16-13:17),OpenCurly(13:18-13:19),
KwCrash(14:2-14:7),StringStart(14:8-14:9),StringPart(14:9-14:24),StringEnd(14:24-14:25),
CloseCurly(15:1-15:2),
UpperIdent(18:1-18:12),NoSpaceOpenRound(18:12-18:13),LowerIdent(18:13-18:14),Comma(18:14-18:15),LowerIdent(18:16-18:17),CloseRound(18:17-18:18),OpColon(18:19-18:20),UpperIdent(18:21-18:27),NoSpaceOpenRound(18:27-18:28),UpperIdent(18:28-18:32),NoSpaceOpenRound(18:32-18:33),UpperIdent(18:33-18:38),NoSpaceOpenRound(18:38-18:39),LowerIdent(18:39-18:40),CloseRound(18:40-18:41),CloseRound(18:41-18:42),Comma(18:42-18:43),UpperIdent(18:44-18:48),NoSpaceOpenRound(18:48-18:49),UpperIdent(18:49-18:52),Comma(18:52-18:53),UpperIdent(18:54-18:59),NoSpaceOpenRound(18:59-18:60),LowerIdent(18:60-18:61),CloseRound(18:61-18:62),CloseRound(18:62-18:63),CloseRound(18:63-18:64),
LowerIdent(20:1-20:6),OpAssign(20:7-20:8),OpBar(20:9-20:10),Underscore(20:10-20:11),OpBar(20:11-20:12),LowerIdent(20:13-20:27),NoSpaceOpenRound(20:27-20:28),UpperIdent(20:28-20:30),NoSpaceOpenRound(20:30-20:31),OpenSquare(20:31-20:32),UpperIdent(20:32-20:36),NoSpaceOpenRound(20:36-20:37),Int(20:37-20:39),CloseRound(20:39-20:40),Comma(20:40-20:41),UpperIdent(20:42-20:46),CloseSquare(20:46-20:47),CloseRound(20:47-20:48),CloseRound(20:48-20:49),
EndOfFile(21:1-21:1),
~~~
# PARSE
~~~clojure
(file @1.1-20.49
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
		(s-type-anno @4.1-4.73 (name "processComplex")
			(ty-fn @4.18-4.73
				(ty-apply @4.18-4.62
					(ty @4.18-4.24 (name "Result"))
					(ty-apply @4.25-4.39
						(ty @4.25-4.29 (name "List"))
						(ty-apply @4.30-4.38
							(ty @4.30-4.35 (name "Maybe"))
							(ty-var @4.36-4.37 (raw "a"))))
					(ty-apply @4.41-4.61
						(ty @4.41-4.45 (name "Dict"))
						(ty @4.46-4.49 (name "Str"))
						(ty-apply @4.51-4.60
							(ty @4.51-4.56 (name "Error"))
							(underscore-ty-var @4.57-4.59 (raw "_b")))))
				(ty-apply @4.66-4.73
					(ty @4.66-4.70 (name "List"))
					(ty-var @4.71-4.72 (raw "a")))))
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
		(s-type-anno @12.1-12.56 (name "deepNested")
			(ty-fn @12.14-12.56
				(ty-apply @12.14-12.51
					(ty @12.14-12.19 (name "Maybe"))
					(ty-apply @12.20-12.50
						(ty @12.20-12.26 (name "Result"))
						(ty-apply @12.27-12.45
							(ty @12.27-12.31 (name "List"))
							(ty-apply @12.32-12.44
								(ty @12.32-12.36 (name "Dict"))
								(ty @12.37-12.40 (name "Str"))
								(ty-var @12.42-12.43 (raw "a"))))
						(underscore-ty-var @12.47-12.49 (raw "_b"))))
				(ty-var @12.55-12.56 (raw "a"))))
		(s-decl @13.1-15.2
			(p-ident @13.1-13.11 (raw "deepNested"))
			(e-lambda @13.14-15.2
				(args
					(p-underscore))
				(e-block @13.18-15.2
					(statements
						(s-crash @14.2-14.25
							(e-string @14.8-14.25
								(e-string-part @14.9-14.24 (raw "not implemented"))))))))
		(s-type-decl @18.1-18.64
			(header @18.1-18.18 (name "ComplexType")
				(args
					(ty-var @18.13-18.14 (raw "a"))
					(ty-var @18.16-18.17 (raw "b"))))
			(ty-apply @18.21-18.64
				(ty @18.21-18.27 (name "Result"))
				(ty-apply @18.28-18.42
					(ty @18.28-18.32 (name "List"))
					(ty-apply @18.33-18.41
						(ty @18.33-18.38 (name "Maybe"))
						(ty-var @18.39-18.40 (raw "a"))))
				(ty-apply @18.44-18.63
					(ty @18.44-18.48 (name "Dict"))
					(ty @18.49-18.52 (name "Str"))
					(ty-apply @18.54-18.62
						(ty @18.54-18.59 (name "Error"))
						(ty-var @18.60-18.61 (raw "b"))))))
		(s-decl @20.1-20.49
			(p-ident @20.1-20.6 (raw "main!"))
			(e-lambda @20.9-20.49
				(args
					(p-underscore))
				(e-apply @20.13-20.49
					(e-ident @20.13-20.27 (raw "processComplex"))
					(e-apply @20.28-20.48
						(e-tag @20.28-20.30 (raw "Ok"))
						(e-list @20.31-20.47
							(e-apply @20.32-20.40
								(e-tag @20.32-20.36 (raw "Some"))
								(e-int @20.37-20.39 (raw "42")))
							(e-tag @20.42-20.46 (raw "None")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test complex nested type applications in function signatures
processComplex : Result(List(Maybe(a)), Dict(Str, Error(_b))) -> List(a)
processComplex = |result|
	match result {
		Ok(maybeList) => []
		Err(_) => []
	}

# Test multiple levels of nesting
deepNested : Maybe(Result(List(Dict(Str, a)), _b)) -> a
deepNested = |_| {
	crash "not implemented"
}

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
									(p-nominal @7.9-7.22
										(p-applied-tag @7.9-7.22))))
							(value
								(e-empty_list @7.26-7.28)))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @8.9-8.15
										(p-applied-tag @8.9-8.15))))
							(value
								(e-empty_list @8.19-8.21)))))))
		(annotation @5.1-5.15
			(declared-type
				(ty-fn @4.18-4.73 (effectful false)
					(ty-apply @4.18-4.62 (name "Result") (local)
						(ty-apply @4.18-4.62 (name "List") (builtin)
							(ty-malformed @4.30-4.35))
						(ty-malformed @4.18-4.62))
					(ty-apply @4.66-4.73 (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var @4.36-4.37 (name "a"))))))))
	(d-let
		(p-assign @13.1-13.11 (ident "deepNested"))
		(e-lambda @13.14-15.2
			(args
				(p-underscore @13.15-13.16))
			(e-block @13.18-15.2
				(e-crash @14.2-14.25 (msg "not implemented"))))
		(annotation @13.1-13.11
			(declared-type
				(ty-fn @12.14-12.56 (effectful false)
					(ty-malformed @12.14-12.19)
					(ty-rigid-var-lookup (ty-rigid-var @12.42-12.43 (name "a")))))))
	(d-let
		(p-assign @20.1-20.6 (ident "main!"))
		(e-closure @20.9-20.49
			(captures
				(capture @5.1-5.15 (ident "processComplex")))
			(e-lambda @20.9-20.49
				(args
					(p-underscore @20.10-20.11))
				(e-call @20.13-20.49
					(e-lookup-local @20.13-20.27
						(p-assign @5.1-5.15 (ident "processComplex")))
					(e-nominal @20.28-20.48 (nominal "Result")
						(e-tag @20.28-20.48 (name "Ok")
							(args
								(e-list @20.31-20.47
									(elems
										(e-tag @20.32-20.40 (name "Some")
											(args
												(e-num @20.37-20.39 (value "42"))))
										(e-tag @20.42-20.46 (name "None")))))))))))
	(s-alias-decl @18.1-18.64
		(ty-header @18.1-18.18 (name "ComplexType")
			(ty-args
				(ty-rigid-var @18.13-18.14 (name "a"))
				(ty-rigid-var @18.16-18.17 (name "b"))))
		(ty-apply @18.21-18.64 (name "Result") (local)
			(ty-apply @18.21-18.64 (name "List") (builtin)
				(ty-malformed @18.33-18.38))
			(ty-malformed @18.21-18.64))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.15 (type "Result(List(Error), Error) -> List(a)"))
		(patt @13.1-13.11 (type "Error -> a"))
		(patt @20.1-20.6 (type "_arg -> List(a)")))
	(type_decls
		(alias @18.1-18.64 (type "ComplexType(a, b)")
			(ty-header @18.1-18.18 (name "ComplexType")
				(ty-args
					(ty-rigid-var @18.13-18.14 (name "a"))
					(ty-rigid-var @18.16-18.17 (name "b"))))))
	(expressions
		(expr @5.18-9.6 (type "Result(List(Error), Error) -> List(a)"))
		(expr @13.14-15.2 (type "Error -> a"))
		(expr @20.9-20.49 (type "_arg -> List(a)"))))
~~~
