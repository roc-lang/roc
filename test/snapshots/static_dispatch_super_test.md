# META
~~~ini
description=Dot access super test
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
~~~
# EXPECTED
NOT IMPLEMENTED - :0:0:0:0
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

# TOKENS
~~~zig
LowerIdent(1:1-1:8),NoSpaceOpenRound(1:8-1:9),LowerIdent(1:9-1:13),CloseRound(1:13-1:14),NoSpaceOpQuestion(1:14-1:15),NoSpaceDotLowerIdent(1:15-1:38),NoSpaceOpenRound(1:38-1:39),CloseRound(1:39-1:40),NoSpaceOpQuestion(1:40-1:41),NoSpaceDotLowerIdent(1:41-1:69),NoSpaceOpenRound(1:69-1:70),CloseRound(1:70-1:71),NoSpaceOpQuestion(1:71-1:72),NoSpaceDotLowerIdent(1:72-1:85),NoSpaceOpQuestion(1:85-1:86),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.86
	(e-field-access @1.1-1.72
		(e-field-access @1.1-1.41
			(e-question-suffix @1.1-1.15
				(e-apply @1.1-1.14
					(e-ident @1.1-1.8 (raw "some_fn"))
					(e-ident @1.9-1.13 (raw "arg1"))))
			(e-question-suffix @1.15-1.41
				(e-apply @1.15-1.40
					(e-ident @1.15-1.38 (raw "static_dispatch_method")))))
		(e-question-suffix @1.41-1.72
			(e-apply @1.41-1.71
				(e-ident @1.41-1.69 (raw "next_static_dispatch_method")))))
	(e-question-suffix @1.72-1.86
		(e-ident @1.72-1.85 (raw "record_field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.86 (field "unknown")
	(receiver
		(e-dot-access @1.1-1.72 (field "unknown")
			(receiver
				(e-dot-access @1.1-1.41 (field "unknown")
					(receiver
						(e-runtime-error (tag "not_implemented"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.86 (type "Error"))
~~~
