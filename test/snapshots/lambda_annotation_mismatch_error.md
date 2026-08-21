# META
~~~ini
description=Lambda annotation mismatch error message test - verifies error messages assume annotation is correct and implementation is wrong
type=snippet
~~~
# SOURCE
~~~roc
# Annotation says it takes and returns strings, but implementation uses number addition
string_function : Str -> Str
string_function = |x| x + 42

# Annotation says function returns I64, but implementation returns Frac(_prec)
wrong_type_function : I64 -> I64
wrong_type_function = |x| x * 3.14
~~~
# EXPECTED
MISSING METHOD - lambda_annotation_mismatch_error.md:3:23:3:29
INVALID NUMBER - lambda_annotation_mismatch_error.md:7:31:7:35
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 3 23) (end 3 29))
		(headline
			(reflow "The value before this")
			(reflow " ")
			(annotated operator "+")
			(reflow " ")
			(reflow "operator has a type that doesn't have a")
			(reflow " ")
			(annotated code "plus")
			(reflow " ")
			(reflow "method."))
		(document
			(source-region (file "lambda_annotation_mismatch_error.md") (start 3 23) (end 3 29) (annotation error) (line-text "string_function = |x| x + 42"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "plus")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The")
			(reflow " ")
			(annotated operator "+")
			(reflow " ")
			(reflow "operator calls a method named")
			(reflow " ")
			(annotated code "plus")
			(reflow " ")
			(reflow "on the value preceding it, passing the value after the operator as the one argument.")))
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 7 31) (end 7 35))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "lambda_annotation_mismatch_error.md") (start 7 31) (end 7 35) (annotation error) (line-text "wrong_type_function = |x| x * 3.14"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "I64")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "string_function")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "string_function"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "42")))))
		(s-type-anno (name "wrong_type_function")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "wrong_type_function"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "*")
					(e-ident (raw "x"))
					(e-frac (raw "3.14")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "string_function"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "wrong_type_function"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str"))
		(patt (type "I64 -> I64")))
	(expressions
		(expr (type "Str -> Str"))
		(expr (type "I64 -> I64"))))
~~~
