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
DOES NOT EXIST - rigid_var_no_instantiation_error.md:17:21:17:30
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:13:5:13:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:17:5:17:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:21:5:21:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 17 21) (end 17 30))
		(headline
			(annotated code "Bool.true")
			(reflow " does not exist."))
		(document
			(annotated code "Bool")
			(reflow " is in scope, but it has no associated ")
			(annotated code "true")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "rigid_var_no_instantiation_error.md") (start 17 21) (end 17 30) (annotation error) (line-text "    result2 = swap((Bool.true, [1, 2, 3]))"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 13 5) (end 13 12))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "result1")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_result1")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "rigid_var_no_instantiation_error.md") (start 13 5) (end 13 12) (annotation error) (line-text "    result1 = swap((42, \"hello\"))"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 17 5) (end 17 12))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "result2")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_result2")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "rigid_var_no_instantiation_error.md") (start 17 5) (end 17 12) (annotation error) (line-text "    result2 = swap((Bool.true, [1, 2, 3]))"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 21 5) (end 21 12))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "result3")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_result3")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "rigid_var_no_instantiation_error.md") (start 21 5) (end 21 12) (annotation error) (line-text "    result3 = swap((\"foo\", \"bar\"))")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,LowerIdent,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
OpenCurly,CloseCurly,
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
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "swap")
			(ty-fn
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-tuple
					(ty-var (raw "b"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "swap"))
			(e-lambda
				(args
					(p-ident (raw "pair")))
				(e-block
					(statements
						(s-decl
							(p-tuple
								(p-ident (raw "x"))
								(p-ident (raw "y")))
							(e-ident (raw "pair")))
						(e-tuple
							(e-ident (raw "y"))
							(e-ident (raw "x")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result1"))
							(e-apply
								(e-ident (raw "swap"))
								(e-tuple
									(e-int (raw "42"))
									(e-string
										(e-string-part (raw "hello"))))))
						(s-decl
							(p-ident (raw "result2"))
							(e-apply
								(e-ident (raw "swap"))
								(e-tuple
									(e-ident (raw "Bool.true"))
									(e-list
										(e-int (raw "1"))
										(e-int (raw "2"))
										(e-int (raw "3"))))))
						(s-decl
							(p-ident (raw "result3"))
							(e-apply
								(e-ident (raw "swap"))
								(e-tuple
									(e-string
										(e-string-part (raw "foo")))
									(e-string
										(e-string-part (raw "bar"))))))
						(e-record)))))))
~~~
# FORMATTED
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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "swap"))
		(e-lambda
			(args
				(p-assign (ident "pair")))
			(e-block
				(s-let
					(p-tuple
						(patterns
							(p-assign (ident "x"))
							(p-assign (ident "y"))))
					(e-lookup-local
						(p-assign (ident "pair"))))
				(e-tuple
					(elems
						(e-lookup-local
							(p-assign (ident "y")))
						(e-lookup-local
							(p-assign (ident "x")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tuple
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))
				(ty-tuple
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "result1"))
					(e-call (constraint-fn-var 289)
						(e-lookup-local
							(p-assign (ident "swap")))
						(e-tuple
							(elems
								(e-num (value "42"))
								(e-string
									(e-literal (string "hello")))))))
				(s-let
					(p-assign (ident "result2"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(s-let
					(p-assign (ident "result3"))
					(e-call (constraint-fn-var 337)
						(e-lookup-local
							(p-assign (ident "swap")))
						(e-tuple
							(elems
								(e-string
									(e-literal (string "foo")))
								(e-string
									(e-literal (string "bar")))))))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a, b) -> (b, a)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "(a, b) -> (b, a)"))
		(expr (type "_arg -> {}"))))
~~~
