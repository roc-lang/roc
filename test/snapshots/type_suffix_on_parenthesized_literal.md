# META
~~~ini
description=A type suffix after a parenthesized literal reports that the suffix goes directly on the literal, while one after another expression reports an unexpected accessor
type=snippet
~~~
# SOURCE
~~~roc
int = (1).U64
nested_frac = ((-1.5)).Dec
string = ("text").Str
value = 1
not_a_literal = (value).U64
~~~
# EXPECTED
PARENTHESIZED TYPE SUFFIX - type_suffix_on_parenthesized_literal.md:1:10:1:14
PARENTHESIZED TYPE SUFFIX - type_suffix_on_parenthesized_literal.md:2:23:2:27
PARENTHESIZED TYPE SUFFIX - type_suffix_on_parenthesized_literal.md:3:18:3:22
EXPECTED RECORD ACCESSOR - type_suffix_on_parenthesized_literal.md:5:24:5:28
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Parenthesized Type Suffix")
		(region (start 1 10) (end 1 14))
		(headline
			(reflow "I was parsing access after `.`, and I found a type suffix after parentheses."))
		(document
			(reflow "A type suffix like ")
			(annotated code ".U64")
			(reflow " is part of the literal it gives a type to, so it goes directly after the number or string, inside any parentheses around it.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "1.U64")
			(line-break)
			(indent 1)
			(text "-12.34.Dec")
			(line-break)
			(indent 1)
			(text "(1.U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "type_suffix_on_parenthesized_literal.md") (start 1 10) (end 1 14) (annotation error) (line-text "int = (1).U64"))))
	(report
		(severity runtime_error)
		(title "Parenthesized Type Suffix")
		(region (start 2 23) (end 2 27))
		(headline
			(reflow "I was parsing access after `.`, and I found a type suffix after parentheses."))
		(document
			(reflow "A type suffix like ")
			(annotated code ".U64")
			(reflow " is part of the literal it gives a type to, so it goes directly after the number or string, inside any parentheses around it.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "1.U64")
			(line-break)
			(indent 1)
			(text "-12.34.Dec")
			(line-break)
			(indent 1)
			(text "(1.U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "type_suffix_on_parenthesized_literal.md") (start 2 23) (end 2 27) (annotation error) (line-text "nested_frac = ((-1.5)).Dec"))))
	(report
		(severity runtime_error)
		(title "Parenthesized Type Suffix")
		(region (start 3 18) (end 3 22))
		(headline
			(reflow "I was parsing access after `.`, and I found a type suffix after parentheses."))
		(document
			(reflow "A type suffix like ")
			(annotated code ".U64")
			(reflow " is part of the literal it gives a type to, so it goes directly after the number or string, inside any parentheses around it.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "1.U64")
			(line-break)
			(indent 1)
			(text "-12.34.Dec")
			(line-break)
			(indent 1)
			(text "(1.U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "type_suffix_on_parenthesized_literal.md") (start 3 18) (end 3 22) (annotation error) (line-text "string = (\"text\").Str"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 5 24) (end 5 28))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".U64")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "type_suffix_on_parenthesized_literal.md") (start 5 24) (end 5 28) (annotation error) (line-text "not_a_literal = (value).U64")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenRound,Int,CloseRound,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpenRound,NoSpaceOpenRound,Float,CloseRound,CloseRound,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpenRound,LowerIdent,CloseRound,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "int"))
			(e-malformed (reason "expr_type_suffix_on_parenthesized_literal")))
		(s-decl
			(p-ident (raw "nested_frac"))
			(e-malformed (reason "expr_type_suffix_on_parenthesized_literal")))
		(s-decl
			(p-ident (raw "string"))
			(e-malformed (reason "expr_type_suffix_on_parenthesized_literal")))
		(s-decl
			(p-ident (raw "value"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "not_a_literal"))
			(e-malformed (reason "expr_dot_suffix_not_allowed")))))
~~~
# FORMATTED
~~~roc
int =

nested_frac =

string =

value = 1

not_a_literal =
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "int"))
		(e-runtime-error (tag "expr_syntax_error")))
	(d-let
		(p-assign (ident "nested_frac"))
		(e-runtime-error (tag "expr_syntax_error")))
	(d-let
		(p-assign (ident "string"))
		(e-runtime-error (tag "expr_syntax_error")))
	(d-let
		(p-assign (ident "value"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "not_a_literal"))
		(e-runtime-error (tag "expr_syntax_error"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Error"))))
~~~
