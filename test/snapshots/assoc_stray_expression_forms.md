# META
~~~ini
description=Every expression statement in an associated block is reported, whatever its form and wherever the block is nested (issue 12802)
type=file:Forms.roc
~~~
# SOURCE
~~~roc
Forms := [A].{
    1 + 2
    x = 1
    (x, x)
    "s"
    [1, 2]
    if x == 1 2 else 3
    match x { _ => 1 }
    |a| a
    y = 2

    Inner := [B].{
        junk
        z = 3
    }

    local = {
        Local := [C].{
            stray
            w = 4
        }
        Local.w
    }
}
~~~
# EXPECTED
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:2:5:2:10
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:4:5:4:11
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:5:5:5:8
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:6:5:6:11
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:7:5:7:23
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:8:5:8:23
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:9:5:9:10
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:13:9:13:13
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_forms.md:19:13:19:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 2 5) (end 2 10))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "1 + 2")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 2 5) (end 2 10) (annotation error) (line-text "    1 + 2"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 4 5) (end 4 11))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "(x, x)")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 4 5) (end 4 11) (annotation error) (line-text "    (x, x)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 5 5) (end 5 8))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "\"s\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 5 5) (end 5 8) (annotation error) (line-text "    \"s\""))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 6 5) (end 6 11))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[1, 2]")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 6 5) (end 6 11) (annotation error) (line-text "    [1, 2]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 7 5) (end 7 23))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "if x == 1 2 else 3")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 7 5) (end 7 23) (annotation error) (line-text "    if x == 1 2 else 3"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 8 5) (end 8 23))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "match x { _ => 1 }")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 8 5) (end 8 23) (annotation error) (line-text "    match x { _ => 1 }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 9 5) (end 9 10))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "|a| a")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 9 5) (end 9 10) (annotation error) (line-text "    |a| a"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 13 9) (end 13 13))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "junk")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 13 9) (end 13 13) (annotation error) (line-text "        junk"))))
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 19 13) (end 19 18))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "stray")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_forms.md") (start 19 13) (end 19 18) (annotation error) (line-text "            stray")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
Int,OpPlus,Int,
LowerIdent,OpAssign,Int,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
StringStart,StringPart,StringEnd,
OpenSquare,Int,Comma,Int,CloseSquare,
KwIf,LowerIdent,OpEquals,Int,Int,KwElse,Int,
KwMatch,LowerIdent,OpenCurly,Underscore,OpFatArrow,Int,CloseCurly,
OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Forms")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(e-binop (op "+")
					(e-int (raw "1"))
					(e-int (raw "2")))
				(s-decl
					(p-ident (raw "x"))
					(e-int (raw "1")))
				(e-tuple
					(e-ident (raw "x"))
					(e-ident (raw "x")))
				(e-string
					(e-string-part (raw "s")))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "x"))
						(e-int (raw "1")))
					(e-int (raw "2"))
					(e-int (raw "3")))
				(e-match
					(e-ident (raw "x"))
					(branches
						(branch
							(p-underscore)
							(e-int (raw "1")))))
				(e-lambda
					(args
						(p-ident (raw "a")))
					(e-ident (raw "a")))
				(s-decl
					(p-ident (raw "y"))
					(e-int (raw "2")))
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "B"))))
					(associated
						(e-ident (raw "junk"))
						(s-decl
							(p-ident (raw "z"))
							(e-int (raw "3")))))
				(s-decl
					(p-ident (raw "local"))
					(e-block
						(statements
							(s-type-decl
								(header (name "Local")
									(args))
								(ty-tag-union
									(tags
										(ty (name "C"))))
								(associated
									(e-ident (raw "stray"))
									(s-decl
										(p-ident (raw "w"))
										(e-int (raw "4")))))
							(e-ident (raw "Local.w")))))))))
~~~
# FORMATTED
~~~roc
Forms := [A].{
	1 + 2
	x = 1
	(x, x)
	"s"
	[1, 2]
	if (x == 1) 2 else 3
	match x {
		_ => 1
	}
	|a| a
	y = 2

	Inner := [B].{
		junk
		z = 3
	}

	local = {
		Local := [C].{
			stray
			w = 4
		}
		Local.w
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Forms.x"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "Forms.y"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "Forms.Inner.z"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "Forms.local"))
		(e-block
			(s-nominal-decl
				(ty-header (name "Local"))
				(ty-tag-union
					(ty-tag-name (name "C"))))
			(s-let
				(p-assign (ident "Local.w"))
				(e-num (value "4")))
			(e-lookup-local
				(p-assign (ident "Local.w")))))
	(s-nominal-decl
		(ty-header (name "Forms"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "Forms.Inner"))
		(ty-tag-union
			(ty-tag-name (name "B"))))
	(s-nominal-decl
		(ty-header (name "Local"))
		(ty-tag-union
			(ty-tag-name (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec")))
	(type_decls
		(nominal (type "Forms")
			(ty-header (name "Forms")))
		(nominal (type "Forms.Inner")
			(ty-header (name "Forms.Inner")))
		(nominal (type "Local")
			(ty-header (name "Local"))))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~
