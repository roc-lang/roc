# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
a : U8
a = 255

b : U16
b = 65535

c : U32
c = 429496729 U64
d = 18446744073709551615

e : U128
e = 3402823669209384634633746074317682114553.14: I8
f =8

g : I16
g = -32768

h : I32
h = -483648

i : I64
i = -92233725808

j : I128
j = -17011687303715884105728
~~~
# EXPECTED
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_025.md:9:1:9:2
UNEXPECTED STATEMENT - fuzz_crash_025.md:9:3:9:4
UNEXPECTED STATEMENT - fuzz_crash_025.md:9:5:9:25
UNEXPECTED STATEMENT - fuzz_crash_025.md:12:48:12:49
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_025.md:13:1:13:2
UNEXPECTED STATEMENT - fuzz_crash_025.md:13:3:13:4
UNEXPECTED STATEMENT - fuzz_crash_025.md:13:4:13:5
INVALID NUMBER - fuzz_crash_025.md:12:5:12:48
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 9 1) (end 9 2))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "d")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_025.md") (start 9 1) (end 9 2) (annotation error) (line-text "d = 18446744073709551615"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 9 3) (end 9 4))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_025.md") (start 9 3) (end 9 4) (annotation error) (line-text "d = 18446744073709551615"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 9 5) (end 9 25))
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
			(annotated code "18446744073709551615")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_025.md") (start 9 5) (end 9 25) (annotation error) (line-text "d = 18446744073709551615"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 12 48) (end 12 49))
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
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_025.md") (start 12 48) (end 12 49) (annotation error) (line-text "e = 3402823669209384634633746074317682114553.14: I8"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 13 1) (end 13 2))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "f")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_025.md") (start 13 1) (end 13 2) (annotation error) (line-text "f =8"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 3) (end 13 4))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_025.md") (start 13 3) (end 13 4) (annotation error) (line-text "f =8"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 4) (end 13 5))
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
			(annotated code "8")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_025.md") (start 13 4) (end 13 5) (annotation error) (line-text "f =8"))))
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 12 5) (end 12 48))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "fuzz_crash_025.md") (start 12 5) (end 12 48) (annotation error) (line-text "e = 3402823669209384634633746074317682114553.14: I8"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U128")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "a")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "255")))
		(s-type-anno (name "b")
			(ty (name "U16")))
		(s-decl
			(p-ident (raw "b"))
			(e-int (raw "65535")))
		(s-type-anno (name "c")
			(ty (name "U32")))
		(s-decl
			(p-ident (raw "c"))
			(e-int (raw "429496729")))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "e")
			(ty (name "U128")))
		(s-decl
			(p-ident (raw "e"))
			(e-frac (raw "3402823669209384634633746074317682114553.14")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "g")
			(ty (name "I16")))
		(s-decl
			(p-ident (raw "g"))
			(e-int (raw "-32768")))
		(s-type-anno (name "h")
			(ty (name "I32")))
		(s-decl
			(p-ident (raw "h"))
			(e-int (raw "-483648")))
		(s-type-anno (name "i")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "i"))
			(e-int (raw "-92233725808")))
		(s-type-anno (name "j")
			(ty (name "I128")))
		(s-decl
			(p-ident (raw "j"))
			(e-int (raw "-17011687303715884105728")))))
~~~
# FORMATTED
~~~roc
a : U8
a = 255

b : U16
b = 65535

c : U32
c = 429496729


e : U128
e = 3402823669209384634633746074317682114553.14



g : I16
g = -32768

h : I32
h = -483648

i : I64
i = -92233725808

j : I128
j = -17011687303715884105728
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "255"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "65535"))
		(annotation
			(ty-lookup (name "U16") (builtin))))
	(d-let
		(p-assign (ident "c"))
		(e-num (value "429496729"))
		(annotation
			(ty-lookup (name "U32") (builtin))))
	(d-let
		(p-assign (ident "e"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "U128") (builtin))))
	(d-let
		(p-assign (ident "g"))
		(e-num (value "-32768"))
		(annotation
			(ty-lookup (name "I16") (builtin))))
	(d-let
		(p-assign (ident "h"))
		(e-num (value "-483648"))
		(annotation
			(ty-lookup (name "I32") (builtin))))
	(d-let
		(p-assign (ident "i"))
		(e-num (value "-92233725808"))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(d-let
		(p-assign (ident "j"))
		(e-num (value "-17011687303715884105728"))
		(annotation
			(ty-lookup (name "I128") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8"))
		(patt (type "U16"))
		(patt (type "U32"))
		(patt (type "Error"))
		(patt (type "I16"))
		(patt (type "I32"))
		(patt (type "I64"))
		(patt (type "I128")))
	(expressions
		(expr (type "U8"))
		(expr (type "U16"))
		(expr (type "U32"))
		(expr (type "Error"))
		(expr (type "I16"))
		(expr (type "I32"))
		(expr (type "I64"))
		(expr (type "I128"))))
~~~
