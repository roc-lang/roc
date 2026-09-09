# META
~~~ini
description=Dollar-prefixed record labels are allowed, while a punned pattern field is checked as an immutable binding
type=snippet
~~~
# SOURCE
~~~roc
my_record = { $field: "value", ok: 1 }

f = |{ $a }| "y"

g : { $b : Str } -> Str
g = |_| "x"
~~~
# EXPECTED
DOLLAR PREFIX WITHOUT `VAR` - dollar_prefix_record_fields.md:3:8:3:10
UNUSED VARIABLE - dollar_prefix_record_fields.md:3:8:3:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Dollar Prefix Without `var`")
		(region (start 3 8) (end 3 10))
		(headline
			(reflow "The immutable binding ")
			(annotated symbol-unqualified "$a")
			(reflow " starts with ")
			(annotated code "$")
			(reflow " but is not declared with ")
			(annotated keyword "var")
			(reflow "."))
		(document
			(reflow "Either rename this binding and all of its uses to ")
			(annotated symbol-unqualified "a")
			(reflow ", or declare it with ")
			(annotated keyword "var")
			(reflow " if it should be mutable.")
			(line-break)
			(line-break)
			(source-region (file "dollar_prefix_record_fields.md") (start 3 8) (end 3 10) (annotation warning) (line-text "f = |{ $a }| \"y\""))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 8) (end 3 10))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "$a")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_$a")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "dollar_prefix_record_fields.md") (start 3 8) (end 3 10) (annotation error) (line-text "f = |{ $a }| \"y\"")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,CloseCurly,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "my_record"))
			(e-record
				(field (field "$field")
					(e-string
						(e-string-part (raw "value"))))
				(field (field "ok")
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-record
						(field (name "$a") (rest false))))
				(e-string
					(e-string-part (raw "y")))))
		(s-type-anno (name "g")
			(ty-fn
				(ty-record
					(anno-record-field (name "$b")
						(ty (name "Str"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-underscore))
				(e-string
					(e-string-part (raw "x")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "my_record"))
		(e-record
			(fields
				(field (name "$field")
					(e-string
						(e-literal (string "value"))))
				(field (name "ok")
					(e-num (value "1"))))))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "$a") (ident "$a")
							(required
								(p-assign (ident "$a")))))))
			(e-string
				(e-literal (string "y")))))
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-underscore))
			(e-string
				(e-literal (string "x"))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "$b")
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ $field: Str, ok: Dec }"))
		(patt (type "{ $a: _field } -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(patt (type "{ $b: Str } -> Str")))
	(expressions
		(expr (type "{ $field: Str, ok: Dec }"))
		(expr (type "{ $a: _field } -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(expr (type "{ $b: Str } -> Str"))))
~~~
