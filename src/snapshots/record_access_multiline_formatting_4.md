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
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize suffix_single_question expression
Let us know if you want to help!

# TOKENS
~~~zig
LowerIdent(1:1-1:8),NoSpaceOpenRound(1:8-1:9),LowerIdent(1:9-1:13),CloseRound(1:13-1:14),NoSpaceOpQuestion(1:14-1:15),Newline(1:17-1:27),
DotLowerIdent(2:2-2:25),NoSpaceOpenRound(2:25-2:26),CloseRound(2:26-2:27),NoSpaceOpQuestion(2:27-2:28),Newline(2:30-2:40),
DotLowerIdent(3:2-3:30),NoSpaceOpenRound(3:30-3:31),CloseRound(3:31-3:32),NoSpaceOpQuestion(3:32-3:33),Newline(3:35-3:45),
DotLowerIdent(4:2-4:15),NoSpaceOpQuestion(4:15-4:16),EndOfFile(4:16-4:16),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-4.16
	(e-field-access @1.1-4.15
		(e-field-access @1.1-3.30
			(e-question-suffix @1.1-1.15
				(e-apply @1.1-1.14
					(e-ident @1.1-1.8 (raw "some_fn"))
					(e-ident @1.9-1.13 (raw "arg1"))))
			(e-question-suffix @2.2-2.28
				(e-apply @2.2-2.27
					(e-ident @2.2-2.25 (raw ".static_dispatch_method")))))
		(e-question-suffix @3.2-3.33
			(e-apply @3.2-3.32
				(e-ident @3.2-3.30 (raw ".next_static_dispatch_method")))))
	(e-question-suffix @4.2-4.16
		(e-ident @4.2-4.15 (raw ".record_field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-4.16 (field "unknown")
	(receiver
		(e-dot-access @1.1-4.15 (field "unknown")
			(receiver
				(e-dot-access @1.1-3.30 (field "unknown")
					(receiver
						(e-runtime-error (tag "not_implemented"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.16 (type "*"))
~~~
