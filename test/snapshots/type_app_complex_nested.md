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
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,OpenSquare,CloseSquare,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,OpenSquare,CloseSquare,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,LowerIdent,CloseRound,CloseRound,Comma,NamedUnderscore,CloseRound,CloseRound,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwCrash,StringStart,StringPart,StringEnd,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,UpperIdent,CloseSquare,CloseRound,CloseRound,
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
		(s-type-anno (name "processComplex")
			(ty-fn
				(ty-apply
					(ty (name "Result"))
					(ty-apply
						(ty (name "List"))
						(ty-apply
							(ty (name "Maybe"))
							(ty-var (raw "a"))))
					(ty-apply
						(ty (name "Dict"))
						(ty (name "Str"))
						(ty-apply
							(ty (name "Error"))
							(underscore-ty-var (raw "_b")))))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "processComplex"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-ident (raw "maybeList")))
							(e-list))
						(branch
							(p-tag (raw "Err")
								(p-underscore))
							(e-list))))))
		(s-type-anno (name "deepNested")
			(ty-fn
				(ty-apply
					(ty (name "Maybe"))
					(ty-apply
						(ty (name "Result"))
						(ty-apply
							(ty (name "List"))
							(ty-apply
								(ty (name "Dict"))
								(ty (name "Str"))
								(ty-var (raw "a"))))
						(underscore-ty-var (raw "_b"))))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "deepNested"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-crash
							(e-string
								(e-string-part (raw "not implemented"))))))))
		(s-type-decl
			(header (name "ComplexType")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-apply
				(ty (name "Result"))
				(ty-apply
					(ty (name "List"))
					(ty-apply
						(ty (name "Maybe"))
						(ty-var (raw "a"))))
				(ty-apply
					(ty (name "Dict"))
					(ty (name "Str"))
					(ty-apply
						(ty (name "Error"))
						(ty-var (raw "b"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "processComplex"))
					(e-apply
						(e-tag (raw "Ok"))
						(e-list
							(e-apply
								(e-tag (raw "Some"))
								(e-int (raw "42")))
							(e-tag (raw "None")))))))))
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
		(p-assign (ident "processComplex"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-empty_list)))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-empty_list)))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "0"))
						(ty-apply (name "List") (builtin)
							(ty-malformed))
						(ty-malformed))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
	(d-let
		(p-assign (ident "deepNested"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(e-crash (msg "not implemented"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "processComplex")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "processComplex")))
					(e-tag (name "Ok")
						(args
							(e-list
								(elems
									(e-tag (name "Some")
										(args
											(e-num (value "42"))))
									(e-tag (name "None"))))))))))
	(s-alias-decl
		(ty-header (name "ComplexType")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "0"))
			(ty-apply (name "List") (builtin)
				(ty-malformed))
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> List(a)"))
		(patt (type "Error -> a"))
		(patt (type "_arg -> List(a)")))
	(type_decls
		(alias (type "ComplexType(a, b)")
			(ty-header (name "ComplexType")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(expressions
		(expr (type "Error -> List(a)"))
		(expr (type "Error -> a"))
		(expr (type "_arg -> List(a)"))))
~~~
