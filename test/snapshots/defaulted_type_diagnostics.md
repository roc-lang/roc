# META
~~~ini
description=Requirements that fail on a defaulted type are reported as undetermined, describing the type as written and never naming the default
type=snippet
~~~
# SOURCE
~~~roc
g : (a -> a) -> I64 where [a.d : I64 -> a, a.encode : a -> I64]
g = |f| {
    A : a
    (f(A.d(41))).encode()
}

materialized = g(|n| n + 1)

literal_method = 35.foo()

literal_signature = 35.plus("s")

shared_literals = if True 1 else "s"

string_compare = "apple" > "banana"

string_method = "s".foo()

written : Dec
written = 35

written_method = written.foo()
~~~
# EXPECTED
MISSING METHOD - defaulted_type_diagnostics.md:22:26:22:29
TYPE NOT DETERMINED - defaulted_type_diagnostics.md:15:18:15:25
TYPE NOT DETERMINED - defaulted_type_diagnostics.md:17:17:17:20
TYPE NOT DETERMINED - defaulted_type_diagnostics.md:9:18:9:20
TYPE NOT DETERMINED - defaulted_type_diagnostics.md:11:21:11:23
TYPE MISMATCH - defaulted_type_diagnostics.md:13:34:13:37
TYPE NOT DETERMINED - defaulted_type_diagnostics.md:7:16:7:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 22 26) (end 22 29))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "foo")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "defaulted_type_diagnostics.md") (start 22 26) (end 22 29) (annotation error) (line-text "written_method = written.foo()"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "foo")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "For this to work, the type would need to have a method named")
			(reflow " ")
			(annotated code "foo")
			(reflow " ")
			(reflow "associated with it in the type's declaration.")))
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 15 18) (end 15 25))
		(headline
			(reflow "Nothing in this program determines the type of this string:"))
		(document
			(source-region (file "defaulted_type_diagnostics.md") (start 15 18) (end 15 25) (annotation error) (line-text "string_compare = \"apple\" > \"banana\""))
			(line-break)
			(reflow "Its type needs all of these:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "b where [b.is_gt : b, b -> Bool]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated operator ">")
			(reflow " ")
			(reflow "to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "None of the built-in string types")
			(reflow " ")
			(reflow "support")
			(reflow " ")
			(annotated operator ">")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 17 17) (end 17 20))
		(headline
			(reflow "Nothing in this program determines the type of this string:"))
		(document
			(source-region (file "defaulted_type_diagnostics.md") (start 17 17) (end 17 20) (annotation error) (line-text "string_method = \"s\".foo()"))
			(line-break)
			(reflow "Its type needs all of these:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "b where [b.foo : b -> _ret]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated code "foo")
			(reflow " ")
			(reflow "method to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "None of the built-in string types")
			(reflow " ")
			(reflow "have a method named")
			(reflow " ")
			(annotated code "foo")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 9 18) (end 9 20))
		(headline
			(reflow "Nothing in this program determines the type of this number:"))
		(document
			(source-region (file "defaulted_type_diagnostics.md") (start 9 18) (end 9 20) (annotation error) (line-text "literal_method = 35.foo()"))
			(line-break)
			(reflow "Its type needs all of these:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "b where [b.foo : b -> _ret]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated code "foo")
			(reflow " ")
			(reflow "method to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "None of the built-in number types")
			(reflow " ")
			(reflow "have a method named")
			(reflow " ")
			(annotated code "foo")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 11 21) (end 11 23))
		(headline
			(reflow "Nothing in this program determines the type of this number:"))
		(document
			(source-region (file "defaulted_type_diagnostics.md") (start 11 21) (end 11 23) (annotation error) (line-text "literal_signature = 35.plus(\"s\")"))
			(line-break)
			(reflow "Its type needs all of these:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "b where [b.plus : b, c -> _ret]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated code "plus")
			(reflow " ")
			(reflow "method to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a suffix or a type annotation saying which type it should be.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 13 34) (end 13 37))
		(headline
			(reflow "This string literal must have the same type as a number literal, and nothing in this program determines a type that can be both:"))
		(document
			(source-region (file "defaulted_type_diagnostics.md") (start 13 34) (end 13 37) (annotation error) (line-text "shared_literals = if True 1 else \"s\""))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be.")))
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 7 16) (end 7 17))
		(headline
			(reflow "Nothing in this program determines a type this needs:"))
		(document
			(source-region (file "defaulted_type_diagnostics.md") (start 7 16) (end 7 17) (annotation error) (line-text "materialized = g(|n| n + 1)"))
			(line-break)
			(reflow "Its type needs all of these:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a where [a.d : I64 -> a, a.encode : a -> I64, a.plus : a, b -> a]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated code "d")
			(reflow " ")
			(reflow "method to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
OpenRound,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,
LowerIdent,OpAssign,Int,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,Int,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,KwIf,UpperIdent,Int,KwElse,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpGreaterThan,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "g")
			(ty-fn
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "a")))
				(ty (name "I64")))
			(where
				(method (mod-of "a") (name "d")
					(ty-fn
						(ty (name "I64"))
						(ty-var (raw "a"))))
				(method (mod-of "a") (name "encode")
					(ty-fn
						(ty-var (raw "a"))
						(ty (name "I64"))))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-block
					(statements
						(s-type-decl
							(header (name "A")
								(args))
							(ty-var (raw "a")))
						(e-method-call (method ".encode")
							(receiver
								(e-tuple
									(e-apply
										(e-ident (raw "f"))
										(e-apply
											(e-ident (raw "A.d"))
											(e-int (raw "41"))))))
							(args))))))
		(s-decl
			(p-ident (raw "materialized"))
			(e-apply
				(e-ident (raw "g"))
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-binop (op "+")
						(e-ident (raw "n"))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "literal_method"))
			(e-method-call (method ".foo")
				(receiver
					(e-int (raw "35")))
				(args)))
		(s-decl
			(p-ident (raw "literal_signature"))
			(e-method-call (method ".plus")
				(receiver
					(e-int (raw "35")))
				(args
					(e-string
						(e-string-part (raw "s"))))))
		(s-decl
			(p-ident (raw "shared_literals"))
			(e-if-then-else
				(e-tag (raw "True"))
				(e-int (raw "1"))
				(e-string
					(e-string-part (raw "s")))))
		(s-decl
			(p-ident (raw "string_compare"))
			(e-binop (op ">")
				(e-string
					(e-string-part (raw "apple")))
				(e-string
					(e-string-part (raw "banana")))))
		(s-decl
			(p-ident (raw "string_method"))
			(e-method-call (method ".foo")
				(receiver
					(e-string
						(e-string-part (raw "s"))))
				(args)))
		(s-type-anno (name "written")
			(ty (name "Dec")))
		(s-decl
			(p-ident (raw "written"))
			(e-int (raw "35")))
		(s-decl
			(p-ident (raw "written_method"))
			(e-method-call (method ".foo")
				(receiver
					(e-ident (raw "written")))
				(args)))))
~~~
# FORMATTED
~~~roc
g : (a -> a) -> I64 where [a.d : I64 -> a, a.encode : a -> I64]
g = |f| {
	A : a
	(f(A.d(41))).encode()
}

materialized = g(|n| n + 1)

literal_method = (35).foo()

literal_signature = (35).plus("s")

shared_literals = if True 1 else "s"

string_compare = "apple" > "banana"

string_method = "s".foo()

written : Dec
written = 35

written_method = written.foo()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-block
				(s-type-var-alias (alias "A") (type-var "a")
					(ty-rigid-var (name "a")))
				(e-dispatch-call (method "encode") (constraint-fn-var 311)
					(receiver
						(e-call (constraint-fn-var 310)
							(e-lookup-local
								(p-assign (ident "f")))
							(e-type-dispatch-call (method "d") (type-dispatch-stmt 20) (constraint-fn-var 306)
								(args
									(e-num (value "41"))))))
					(args))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "a"))
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
				(ty-lookup (name "I64") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "d")
					(ty-fn (effectful false)
						(ty-lookup (name "I64") (builtin))
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "encode")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "I64") (builtin)))))))
	(d-let
		(p-assign (ident "materialized"))
		(e-call (constraint-fn-var 331)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-dispatch-call (method "plus") (constraint-fn-var 329)
					(receiver
						(e-lookup-local
							(p-assign (ident "n"))))
					(args
						(e-num (value "1")))))))
	(d-let
		(p-assign (ident "literal_method"))
		(e-dispatch-call (method "foo") (constraint-fn-var 339)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args)))
	(d-let
		(p-assign (ident "literal_signature"))
		(e-dispatch-call (method "plus") (constraint-fn-var 356)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args
				(e-string
					(e-literal (string "s"))))))
	(d-let
		(p-assign (ident "shared_literals"))
		(e-if
			(if-branches
				(if-branch
					(e-tag (name "True"))
					(e-runtime-error (tag "erroneous_value_expr"))))
			(if-else
				(e-runtime-error (tag "erroneous_value_expr")))))
	(d-let
		(p-assign (ident "string_compare"))
		(e-dispatch-call (method "is_gt") (constraint-fn-var 395)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args
				(e-runtime-error (tag "erroneous_value_expr")))))
	(d-let
		(p-assign (ident "string_method"))
		(e-dispatch-call (method "foo") (constraint-fn-var 405)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args)))
	(d-let
		(p-assign (ident "written"))
		(e-num (value "35"))
		(annotation
			(ty-lookup (name "Dec") (builtin))))
	(d-let
		(p-assign (ident "written_method"))
		(e-runtime-error (tag "erroneous_value_expr")
			(e-lookup-local
				(p-assign (ident "written"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a -> a) -> I64 where [a.d : I64 -> a, a.encode : a -> I64]"))
		(patt (type "I64"))
		(patt (type "_b"))
		(patt (type "_b"))
		(patt (type "Dec"))
		(patt (type "Bool"))
		(patt (type "_b"))
		(patt (type "Dec"))
		(patt (type "_b")))
	(expressions
		(expr (type "(a -> a) -> I64 where [a.d : I64 -> a, a.encode : a -> I64]"))
		(expr (type "I64"))
		(expr (type "_b"))
		(expr (type "_b"))
		(expr (type "Dec"))
		(expr (type "Bool"))
		(expr (type "_b"))
		(expr (type "Dec"))
		(expr (type "_b"))))
~~~
