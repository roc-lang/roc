# META
~~~ini
description=record_access_multiline_formatting (4)
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
	.next_static_dispatch_method()? # Comment 3
	.record_field?
~~~
# EXPECTED
NOT IMPLEMENTED - :0:0:0:0
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
DotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,
DotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,
DotLowerIdent,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(e-field-access
		(e-field-access
			(e-question-suffix
				(e-apply
					(e-ident (raw "some_fn"))
					(e-ident (raw "arg1"))))
			(e-question-suffix
				(e-apply
					(e-ident (raw ".static_dispatch_method")))))
		(e-question-suffix
			(e-apply
				(e-ident (raw ".next_static_dispatch_method")))))
	(e-question-suffix
		(e-ident (raw ".record_field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access (field "unknown")
	(receiver
		(e-dot-access (field "unknown")
			(receiver
				(e-dot-access (field "unknown")
					(receiver
						(e-runtime-error (tag "not_implemented"))))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
