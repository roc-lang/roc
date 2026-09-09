# META
~~~ini
description=For loop iterating over List Str
type=snippet
~~~
# SOURCE
~~~roc
count : U64
count = {
	var counter_ = 0
	for _ in ["hello", "world", "test"] {
		counter_ = counter_ + 1
	}
	counter_
}

expect count == 3
~~~
# EXPECTED
VAR NAME MISSING `$` - for_loop_list_str.md:3:6:3:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 3 6) (end 3 14))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "counter_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$counter_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_list_str.md") (start 3 6) (end 3 14) (annotation warning) (line-text "\tvar counter_ = 0")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,Underscore,KwIn,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "count")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "count"))
			(e-block
				(statements
					(s-var (name "counter_")
						(e-int (raw "0")))
					(s-for
						(p-underscore)
						(e-list
							(e-string
								(e-string-part (raw "hello")))
							(e-string
								(e-string-part (raw "world")))
							(e-string
								(e-string-part (raw "test"))))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "counter_"))
									(e-binop (op "+")
										(e-ident (raw "counter_"))
										(e-int (raw "1")))))))
					(e-ident (raw "counter_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "count"))
				(e-int (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "count"))
		(e-block
			(s-var
				(p-var-assign (ident "counter_"))
				(e-num (value "0")))
			(s-for
				(p-underscore)
				(e-list
					(elems
						(e-string
							(e-literal (string "hello")))
						(e-string
							(e-literal (string "world")))
						(e-string
							(e-literal (string "test")))))
				(e-block
					(s-reassign
						(p-var-assign (ident "counter_"))
						(e-dispatch-call (method "plus") (constraint-fn-var 318)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "counter_"))))
							(args
								(e-num (value "1")))))
					(e-empty_record)))
			(e-lookup-local
				(p-var-assign (ident "counter_"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "count"))))
			(rhs
				(e-num (value "3"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
