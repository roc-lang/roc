# META
~~~ini
description=local_dispatch_multiline_formatting (1)
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)?
	->static_dispatch_method()?
	->next_static_dispatch_method()?
	->record_field?
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize local_dispatch expression

# TOKENS
~~~zig
LowerIdent(1:1-1:8),NoSpaceOpenRound(1:8-1:9),LowerIdent(1:9-1:13),CloseRound(1:13-1:14),NoSpaceOpQuestion(1:14-1:15),Newline(1:1-1:1),
OpArrow(2:2-2:4),LowerIdent(2:4-2:26),NoSpaceOpenRound(2:26-2:27),CloseRound(2:27-2:28),NoSpaceOpQuestion(2:28-2:29),Newline(1:1-1:1),
OpArrow(3:2-3:4),LowerIdent(3:4-3:31),NoSpaceOpenRound(3:31-3:32),CloseRound(3:32-3:33),NoSpaceOpQuestion(3:33-3:34),Newline(1:1-1:1),
OpArrow(4:2-4:4),LowerIdent(4:4-4:16),NoSpaceOpQuestion(4:16-4:17),EndOfFile(4:17-4:17),
~~~
# PARSE
~~~clojure
(local_dispatch (1:1-4:17)
	(local_dispatch (1:1-4:4)
		(local_dispatch (1:1-3:4)
			(suffix_single_question (1:1-1:15)
				(apply (1:1-1:14)
					(ident (1:1-1:8) "" "some_fn")
					(ident (1:9-1:13) "" "arg1")))
			(suffix_single_question (2:2-2:29)
				(apply (2:2-2:28)
					(ident (2:4-2:26) "" "static_dispatch_method"))))
		(suffix_single_question (3:2-3:34)
			(apply (3:2-3:33)
				(ident (3:4-3:31) "" "next_static_dispatch_method"))))
	(suffix_single_question (4:2-4:17)
		(ident (4:4-4:16) "" "record_field")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_runtime_error (1:1-1:1) "not_implemented")
~~~
# TYPES
~~~clojure
(expr 13 (type "Error"))
~~~