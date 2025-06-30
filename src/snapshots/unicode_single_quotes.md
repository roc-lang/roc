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
    'ñ',
    '🚀',
    '\u(1F680)',
    '\u(00E9)',
)
~~~
# PROBLEMS
**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it appears to be invalid.

**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it appears to be invalid.

# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
SingleQuote(2:5-2:8),Comma(2:8-2:9),Newline(1:1-1:1),
SingleQuote(3:5-3:9),Comma(3:9-3:10),Newline(1:1-1:1),
SingleQuote(4:5-4:9),Comma(4:9-4:10),Newline(1:1-1:1),
SingleQuote(5:5-5:11),Comma(5:11-5:12),Newline(1:1-1:1),
SingleQuote(6:5-6:16),Comma(6:16-6:17),Newline(1:1-1:1),
SingleQuote(7:5-7:15),Comma(7:15-7:16),Newline(1:1-1:1),
CloseRound(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-8.2
	(e-single-quote @2.5-2.8 (raw "'a'"))
	(e-single-quote @3.5-3.9 (raw "'é'"))
	(e-single-quote @4.5-4.9 (raw "'ñ'"))
	(e-single-quote @5.5-5.11 (raw "'🚀'"))
	(e-single-quote @6.5-6.16 (raw "'\u(1F680)'"))
	(e-single-quote @7.5-7.15 (raw "'\u(00E9)'")))
~~~
# FORMATTED
~~~roc
(
	'a',
	'é',
	'ñ',
	'🚀',
	'\u(1F680)',
	'\u(00E9)',
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-8.2 (id 81)
	(elems
		(e-int @2.5-2.8 (value "97"))
		(e-int @3.5-3.9 (value "233"))
		(e-int @4.5-4.9 (value "241"))
		(e-int @5.5-5.11 (value "128640"))
		(e-runtime-error (tag "invalid_single_quote"))
		(e-runtime-error (tag "invalid_single_quote"))))
~~~
# TYPES
~~~clojure
(expr (id 81) (type "(Num(*), Num(*), Num(*), Num(*), Error, Error)"))
~~~
