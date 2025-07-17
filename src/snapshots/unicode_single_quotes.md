# META
~~~ini
description=Unicode single quotes
type=expr
~~~
# SOURCE
~~~roc
(
    'a',
    'é',
    '🚀',
    '\u(1F680)',
    '\\',
    '\'',
)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),
SingleQuote(2:5-2:8),Comma(2:8-2:9),
SingleQuote(3:5-3:9),Comma(3:9-3:10),
SingleQuote(4:5-4:11),Comma(4:11-4:12),
SingleQuote(5:5-5:16),Comma(5:16-5:17),
SingleQuote(6:5-6:9),Comma(6:9-6:10),
SingleQuote(7:5-7:9),Comma(7:9-7:10),
CloseRound(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-8.2
	(e-single-quote @2.5-2.8 (raw "'a'"))
	(e-single-quote @3.5-3.9 (raw "'é'"))
	(e-single-quote @4.5-4.11 (raw "'🚀'"))
	(e-single-quote @5.5-5.16 (raw "'\u(1F680)'"))
	(e-single-quote @6.5-6.9 (raw "'\\'"))
	(e-single-quote @7.5-7.9 (raw "'\''")))
~~~
# FORMATTED
~~~roc
(
	'a',
	'é',
	'🚀',
	'\u(1F680)',
	'\\',
	'\'',
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-8.2
	(elems
		(e-int @2.5-2.8 (value "97"))
		(e-int @3.5-3.9 (value "233"))
		(e-int @4.5-4.11 (value "128640"))
		(e-int @5.5-5.16 (value "128640"))
		(e-int @6.5-6.9 (value "92"))
		(e-int @7.5-7.9 (value "39"))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "(Num(_size), Num(_size2), Num(_size3), Num(_size4), Num(_size5), Num(_size6))"))
~~~
