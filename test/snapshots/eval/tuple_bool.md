# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
~~~
# EXPECTED
MISSING METHOD - tuple_bool.md:1:38:1:43
MISSING METHOD - tuple_bool.md:1:45:1:51
MISSING METHOD - tuple_bool.md:1:69:1:74
MISSING METHOD - tuple_bool.md:1:78:1:83
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 38) (end 1 43))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "tuple_bool.md") (start 1 38) (end 1 43) (annotation error) (line-text "(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "not")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[True, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 45) (end 1 51))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "tuple_bool.md") (start 1 45) (end 1 51) (annotation error) (line-text "(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "not")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[False, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 69) (end 1 74))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "tuple_bool.md") (start 1 69) (end 1 74) (annotation error) (line-text "(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "not")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[True, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 78) (end 1 83))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "tuple_bool.md") (start 1 78) (end 1 83) (annotation error) (line-text "(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "not")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[True, ..]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
OpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,OpBang,UpperIdent,Comma,OpBang,UpperIdent,Comma,UpperIdent,OpAnd,UpperIdent,Comma,OpBang,UpperIdent,OpOr,OpBang,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-tag (raw "True"))
	(e-tag (raw "False"))
	(e-tag (raw "Bool.True"))
	(e-tag (raw "Bool.False"))
	(unary "!"
		(e-tag (raw "True")))
	(unary "!"
		(e-tag (raw "False")))
	(e-binop (op "and")
		(e-tag (raw "True"))
		(e-tag (raw "False")))
	(e-binop (op "or")
		(unary "!"
			(e-tag (raw "True")))
		(unary "!"
			(e-tag (raw "True")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-tag (name "True"))
		(e-tag (name "False"))
		(e-nominal-external
			(builtin)
			(e-tag (name "True")))
		(e-nominal-external
			(builtin)
			(e-tag (name "False")))
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-if
			(if-branches
				(if-branch
					(e-tag (name "True"))
					(e-tag (name "False"))))
			(if-else
				(e-nominal-external
					(builtin)
					(e-tag (name "False")))))
		(e-if
			(if-branches
				(if-branch
					(e-runtime-error (tag "erroneous_value_expr"))
					(e-nominal-external
						(builtin)
						(e-tag (name "True")))))
			(if-else
				(e-runtime-error (tag "erroneous_value_expr"))))))
~~~
# TYPES
~~~clojure
(expr (type "([True, ..], [False, ..], Bool, Bool, [True, ..], [False, ..], Bool, Bool)"))
~~~
