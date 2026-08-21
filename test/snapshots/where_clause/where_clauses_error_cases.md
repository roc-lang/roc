# META
~~~ini
description=Error cases for where clauses
type=snippet
~~~
# SOURCE
~~~roc
# Missing colon in constraint
broken_fn1 : a -> b
  where [a.method -> b]

# Empty where clause
broken_fn2 : a -> b
  where []

# Referencing undefined type variable
broken_fn3 : a -> b
  where [c.method : c -> d]
~~~
# EXPECTED
EXPECTED CONSTRAINT TYPE - where_clauses_error_cases.md:3:10:3:11
EXPECTED WHERE CLAUSE END - where_clauses_error_cases.md:3:3:3:21
UNEXPECTED STATEMENT - where_clauses_error_cases.md:3:22:3:23
UNEXPECTED STATEMENT - where_clauses_error_cases.md:3:23:3:24
EXPECTED WHERE CONSTRAINT - where_clauses_error_cases.md:7:3:7:10
UNEXPECTED STATEMENT - where_clauses_error_cases.md:7:10:7:11
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:3:10:3:21
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:7:3:7:10
DECLARATION HAS NO VALUE - where_clauses_error_cases.md:2:1:3:21
DECLARATION HAS NO VALUE - where_clauses_error_cases.md:6:1:7:10
UNBOUND WHERE RECEIVER - where_clauses_error_cases.md:11:10:11:27
DECLARATION HAS NO VALUE - where_clauses_error_cases.md:10:1:11:28
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Constraint Type")
		(region (start 3 10) (end 3 11))
		(headline
			(reflow "I was parsing a `where` method constraint, and I expected `:` before the method type."))
		(document
			(reflow "Method constraints use a colon between the method name and its type.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "where [a.hash : a -> U64]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "a")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "where_clauses_error_cases.md") (start 3 10) (end 3 11) (annotation error) (line-text "  where [a.method -> b]"))))
	(report
		(severity runtime_error)
		(title "Expected Where Clause End")
		(region (start 3 3) (end 3 21))
		(headline
			(reflow "I was parsing a `where` clause, and I expected `]`."))
		(document
			(reflow "Close the where constraint list after the final constraint.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "where [a.hash : a -> U64]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "where [a.method ->")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "where_clauses_error_cases.md") (start 3 3) (end 3 21) (annotation error) (line-text "  where [a.method -> b]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 22) (end 3 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "b")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "where_clauses_error_cases.md") (start 3 22) (end 3 23) (annotation error) (line-text "  where [a.method -> b]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 23) (end 3 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "where_clauses_error_cases.md") (start 3 23) (end 3 24) (annotation error) (line-text "  where [a.method -> b]"))))
	(report
		(severity runtime_error)
		(title "Expected Where Constraint")
		(region (start 7 3) (end 7 10))
		(headline
			(reflow "I was parsing a `where` clause, and I expected at least one constraint."))
		(document
			(reflow "Remove the empty ")
			(annotated code "where")
			(reflow " clause or add a constraint inside the brackets.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "where [a.hash : a -> U64]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "where [")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "where_clauses_error_cases.md") (start 7 3) (end 7 10) (annotation error) (line-text "  where []"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 7 10) (end 7 11))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "where_clauses_error_cases.md") (start 7 10) (end 7 11) (annotation error) (line-text "  where []"))))
	(report
		(severity runtime_error)
		(title "Malformed Where Clause")
		(region (start 3 10) (end 3 21))
		(headline
			(reflow "This where clause could not be parsed correctly."))
		(document
			(source-region (file "where_clauses_error_cases.md") (start 3 10) (end 3 21) (annotation error) (line-text "  where [a.method -> b]"))
			(line-break)
			(reflow "Check the syntax of your where clause.")))
	(report
		(severity runtime_error)
		(title "Malformed Where Clause")
		(region (start 7 3) (end 7 10))
		(headline
			(reflow "This where clause could not be parsed correctly."))
		(document
			(source-region (file "where_clauses_error_cases.md") (start 7 3) (end 7 10) (annotation error) (line-text "  where []"))
			(line-break)
			(reflow "Check the syntax of your where clause.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 1) (end 3 21))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "where_clauses_error_cases.md") (start 2 1) (end 3 21) (annotation error) (line-text "broken_fn1 : a -> b\n  where [a.method -> b]"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 6 1) (end 7 10))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "where_clauses_error_cases.md") (start 6 1) (end 7 10) (annotation error) (line-text "broken_fn2 : a -> b\n  where []"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Unbound Where Receiver")
		(region (start 11 10) (end 11 27))
		(headline
			(reflow "The type variable")
			(reflow " ")
			(annotated code "c")
			(reflow " ")
			(reflow "is not introduced by this annotation's type or a connected method constraint, so this where clause cannot add the")
			(reflow " ")
			(annotated symbol "method")
			(reflow " ")
			(reflow "method to it."))
		(document
			(source-region (file "where_clauses_error_cases.md") (start 11 10) (end 11 27) (annotation error) (line-text "  where [c.method : c -> d]"))
			(line-break)
			(reflow "A where clause receiver must be introduced by the annotation's type, or by the method type of a receiver that is already connected to the annotation. Connect")
			(reflow " ")
			(annotated code "c")
			(reflow " ")
			(reflow "to the annotation, or remove this constraint.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 10 1) (end 11 28))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "where_clauses_error_cases.md") (start 10 1) (end 11 28) (annotation error) (line-text "broken_fn3 : a -> b\n  where [c.method : c -> d]"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "broken_fn1")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(malformed (reason "where_expected_colon"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "broken_fn2")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(malformed (reason "where_expected_constraints"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "broken_fn3")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(method (mod-of "c") (name "method")
					(args
						(ty-var (raw "c")))
					(ty-var (raw "d")))))))
~~~
# FORMATTED
~~~roc
# Missing colon in constraint
broken_fn1 : a -> b
	where []


# Empty where clause
broken_fn2 : a -> b
	where []


# Referencing undefined type variable
broken_fn3 : a -> b
	where [c.method : c -> d]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "broken_fn1"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(malformed))))
	(d-let
		(p-assign (ident "broken_fn2"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(malformed))))
	(d-let
		(p-assign (ident "broken_fn3"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(method (ty-rigid-var (name "c")) (name "method")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
					(ty-rigid-var (name "d")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b"))
		(patt (type "a -> b"))
		(patt (type "Error")))
	(expressions
		(expr (type "a -> b"))
		(expr (type "a -> b"))
		(expr (type "Error"))))
~~~
