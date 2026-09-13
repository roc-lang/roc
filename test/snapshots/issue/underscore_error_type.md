# META
~~~ini
description=Type declarations with underscores should become error types that fail unification
type=snippet
~~~
# SOURCE
~~~roc
BadType := _

foo : BadType
foo = 42

BadList := List(_)

bar : BadList
bar = [1, 2, 3]

BadRecord := { field: _, other: U32 }

baz : BadRecord
baz = { field: "hi", other: 5 }

BadFunction := _ -> _

qux : BadFunction
qux = |x| x

BadTuple := (_, U32)

quux : BadTuple
quux = ("hello", 42)
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:6:17:6:17
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:6:12:6:16
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:21:14:21:14
TYPE MISMATCH - underscore_error_type.md:4:7:4:9
TYPE MISMATCH - underscore_error_type.md:9:7:9:16
TYPE MISMATCH - underscore_error_type.md:19:7:19:12
TYPE MISMATCH - underscore_error_type.md:24:8:24:21
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_type.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 6 17) (end 6 17))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_type.md") (start 6 17) (end 6 17) (annotation error) (line-text "BadList := List(_)"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 6 12) (end 6 16))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_type.md") (start 6 12) (end 6 16) (annotation error) (line-text "BadList := List(_)"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_type.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_type.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_type.md") (start 1 1) (end 1 1) (annotation error) (line-text "BadType := _"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 21 14) (end 21 14))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "underscore_error_type.md") (start 21 14) (end 21 14) (annotation error) (line-text "BadTuple := (_, U32)"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 7) (end 4 9))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "underscore_error_type.md") (start 4 7) (end 4 9) (annotation error) (line-text "foo = 42"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "BadType")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 9 7) (end 9 16))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "underscore_error_type.md") (start 9 7) (end 9 16) (annotation error) (line-text "bar = [1, 2, 3]"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "BadList")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 19 7) (end 19 12))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "underscore_error_type.md") (start 19 7) (end 19 12) (annotation error) (line-text "qux = |x| x"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> a")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "BadFunction")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 24 8) (end 24 21))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "underscore_error_type.md") (start 24 8) (end 24 21) (annotation error) (line-text "quux = (\"hello\", 42)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b)")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "BadTuple")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,Underscore,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
UpperIdent,OpColonEqual,Underscore,OpArrow,Underscore,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
UpperIdent,OpColonEqual,OpenRound,Underscore,Comma,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenRound,StringStart,StringPart,StringEnd,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "BadType")
				(args))
			(_))
		(s-type-anno (name "foo")
			(ty (name "BadType")))
		(s-decl
			(p-ident (raw "foo"))
			(e-int (raw "42")))
		(s-type-decl
			(header (name "BadList")
				(args))
			(ty-apply
				(ty (name "List"))
				(_)))
		(s-type-anno (name "bar")
			(ty (name "BadList")))
		(s-decl
			(p-ident (raw "bar"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-type-decl
			(header (name "BadRecord")
				(args))
			(ty-record
				(anno-record-field (name "field")
					(_))
				(anno-record-field (name "other")
					(ty (name "U32")))))
		(s-type-anno (name "baz")
			(ty (name "BadRecord")))
		(s-decl
			(p-ident (raw "baz"))
			(e-record
				(field (field "field")
					(e-string
						(e-string-part (raw "hi"))))
				(field (field "other")
					(e-int (raw "5")))))
		(s-type-decl
			(header (name "BadFunction")
				(args))
			(ty-fn
				(_)
				(_)))
		(s-type-anno (name "qux")
			(ty (name "BadFunction")))
		(s-decl
			(p-ident (raw "qux"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-type-decl
			(header (name "BadTuple")
				(args))
			(ty-tuple
				(_)
				(ty (name "U32"))))
		(s-type-anno (name "quux")
			(ty (name "BadTuple")))
		(s-decl
			(p-ident (raw "quux"))
			(e-tuple
				(e-string
					(e-string-part (raw "hello")))
				(e-int (raw "42"))))))
~~~
# FORMATTED
~~~roc
BadType := _

foo : BadType
foo = 42

BadList := List(_)

bar : BadList
bar = [1, 2, 3]

BadRecord := { field : _, other : U32 }

baz : BadRecord
baz = { field: "hi", other: 5 }

BadFunction := _ -> _

qux : BadFunction
qux = |x| x

BadTuple := (_, U32)

quux : BadTuple
quux = ("hello", 42)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "BadType") (local))))
	(d-let
		(p-assign (ident "bar"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "BadList") (local))))
	(d-let
		(p-assign (ident "baz"))
		(e-record
			(fields
				(field (name "field")
					(e-string
						(e-literal (string "hi"))))
				(field (name "other")
					(e-num (value "5")))))
		(annotation
			(ty-lookup (name "BadRecord") (local))))
	(d-let
		(p-assign (ident "qux"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "BadFunction") (local))))
	(d-let
		(p-assign (ident "quux"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "BadTuple") (local))))
	(s-nominal-decl
		(ty-header (name "BadType"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "BadList"))
		(ty-apply (name "List") (builtin)
			(ty-underscore)))
	(s-nominal-decl
		(ty-header (name "BadRecord"))
		(ty-record
			(field (field "field")
				(ty-underscore))
			(field (field "other")
				(ty-lookup (name "U32") (builtin)))))
	(s-nominal-decl
		(ty-header (name "BadFunction"))
		(ty-fn (effectful false)
			(ty-underscore)
			(ty-underscore)))
	(s-nominal-decl
		(ty-header (name "BadTuple"))
		(ty-tuple
			(ty-underscore)
			(ty-lookup (name "U32") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "BadType"))
		(patt (type "BadList"))
		(patt (type "BadRecord"))
		(patt (type "BadFunction"))
		(patt (type "BadTuple")))
	(type_decls
		(nominal (type "BadType")
			(ty-header (name "BadType")))
		(nominal (type "BadList")
			(ty-header (name "BadList")))
		(nominal (type "BadRecord")
			(ty-header (name "BadRecord")))
		(nominal (type "BadFunction")
			(ty-header (name "BadFunction")))
		(nominal (type "BadTuple")
			(ty-header (name "BadTuple"))))
	(expressions
		(expr (type "BadType"))
		(expr (type "BadList"))
		(expr (type "BadRecord"))
		(expr (type "BadFunction"))
		(expr (type "BadTuple"))))
~~~
