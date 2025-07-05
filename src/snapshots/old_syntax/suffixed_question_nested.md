# META
~~~ini
description=suffixed_question_nested
type=expr
~~~
# SOURCE
~~~roc
foo?  (  bar? baz)  ( blah stuff)
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize suffix_single_question expression
Let us know if you want to help!

# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceOpQuestion(1:4-1:5),OpenRound(1:7-1:8),LowerIdent(1:10-1:13),NoSpaceOpQuestion(1:13-1:14),LowerIdent(1:15-1:18),CloseRound(1:18-1:19),OpenRound(1:21-1:22),LowerIdent(1:23-1:27),LowerIdent(1:28-1:33),CloseRound(1:33-1:34),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-question-suffix @1.1-1.5
	(e-ident @1.1-1.4 (raw "foo")))
~~~
# FORMATTED
~~~roc
foo?
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
