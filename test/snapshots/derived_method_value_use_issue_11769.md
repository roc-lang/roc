# META
~~~ini
description=A compiler-derived associated method can be called by name, but using it as a value without calling it is reported (issue 11769)
type=snippet
~~~
# SOURCE
~~~roc
Flag := [On, Off].{
	encoder_for : _
}

encode_flag = Flag.encoder_for
encode_str = Str.encoder_for
~~~
# EXPECTED
DERIVED METHOD USED AS VALUE - derived_method_value_use_issue_11769.md:5:15:5:31
DERIVED METHOD USED AS VALUE - derived_method_value_use_issue_11769.md:6:14:6:29
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Derived Method Used As Value")
		(region (start 5 15) (end 5 31))
		(headline
			(reflow "The compiler derives")
			(reflow " ")
			(annotated code "encoder_for")
			(reflow " ")
			(reflow "for this type, so it can only be called directly, not used as a value."))
		(document
			(source-region (file "derived_method_value_use_issue_11769.md") (start 5 15) (end 5 31) (annotation error) (line-text "encode_flag = Flag.encoder_for"))
			(line-break)
			(line-break)
			(reflow "Call it here with its arguments instead.")))
	(report
		(severity runtime_error)
		(title "Derived Method Used As Value")
		(region (start 6 14) (end 6 29))
		(headline
			(reflow "The compiler derives")
			(reflow " ")
			(annotated code "encoder_for")
			(reflow " ")
			(reflow "for this type, so it can only be called directly, not used as a value."))
		(document
			(source-region (file "derived_method_value_use_issue_11769.md") (start 6 14) (end 6 29) (annotation error) (line-text "encode_str = Str.encoder_for"))
			(line-break)
			(line-break)
			(reflow "Call it here with its arguments instead."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,Underscore,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Flag")
				(args))
			(ty-tag-union
				(tags
					(ty (name "On"))
					(ty (name "Off"))))
			(associated
				(s-type-anno (name "encoder_for")
					(_))))
		(s-decl
			(p-ident (raw "encode_flag"))
			(e-ident (raw "Flag.encoder_for")))
		(s-decl
			(p-ident (raw "encode_str"))
			(e-ident (raw "Str.encoder_for")))))
~~~
# FORMATTED
~~~roc
Flag := [On, Off].{
	encoder_for : _
}

encode_flag = Flag.encoder_for

encode_str = Str.encoder_for
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "derived_method_value_use_issue_11769.Flag.encoder_for"))
		(e-derived-method (kind "encoder"))
		(annotation
			(ty-underscore)))
	(d-let
		(p-assign (ident "encode_flag"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "encode_str"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-nominal-decl
		(ty-header (name "Flag"))
		(ty-tag-union
			(ty-tag-name (name "On"))
			(ty-tag-name (name "Off")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Flag")
			(ty-header (name "Flag"))))
	(expressions
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
