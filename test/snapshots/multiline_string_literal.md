# META
~~~ini
description=multiline_string_literal
type=expr
~~~
# SOURCE
~~~roc
"""This is a string
"""With multiple lines
"""and ${someVar} in it
"""${something} at beginning
"""and at the ${end}
""" and with a linebreak ${
	some_func(a, b)
} in here, and there are whitespaces at the end ->		  
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:20),
MultilineStringStart(2:1-2:4),StringPart(2:4-2:23),
MultilineStringStart(3:1-3:4),StringPart(3:4-3:8),OpenStringInterpolation(3:8-3:10),LowerIdent(3:10-3:17),CloseStringInterpolation(3:17-3:18),StringPart(3:18-3:24),
MultilineStringStart(4:1-4:4),StringPart(4:4-4:4),OpenStringInterpolation(4:4-4:6),LowerIdent(4:6-4:15),CloseStringInterpolation(4:15-4:16),StringPart(4:16-4:29),
MultilineStringStart(5:1-5:4),StringPart(5:4-5:15),OpenStringInterpolation(5:15-5:17),LowerIdent(5:17-5:20),CloseStringInterpolation(5:20-5:21),StringPart(5:21-5:21),
MultilineStringStart(6:1-6:4),StringPart(6:4-6:26),OpenStringInterpolation(6:26-6:28),
LowerIdent(7:2-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:13),Comma(7:13-7:14),LowerIdent(7:15-7:16),CloseRound(7:16-7:17),
CloseStringInterpolation(8:1-8:2),StringPart(8:2-8:55),EndOfFile(8:55-8:55),
~~~
# PARSE
~~~clojure
(e-multiline-string @1.1-8.55
	(e-string-part @1.4-1.20 (raw "This is a string"))
	(e-string-part @2.4-2.23 (raw "With multiple lines"))
	(e-string-part @3.4-3.8 (raw "and "))
	(e-ident @3.10-3.17 (raw "someVar"))
	(e-string-part @3.18-3.24 (raw " in it"))
	(e-string-part @4.4-4.4 (raw ""))
	(e-ident @4.6-4.15 (raw "something"))
	(e-string-part @4.16-4.29 (raw " at beginning"))
	(e-string-part @5.4-5.15 (raw "and at the "))
	(e-ident @5.17-5.20 (raw "end"))
	(e-string-part @5.21-5.21 (raw ""))
	(e-string-part @6.4-6.26 (raw " and with a linebreak "))
	(e-apply @7.2-7.17
		(e-ident @7.2-7.11 (raw "some_func"))
		(e-ident @7.12-7.13 (raw "a"))
		(e-ident @7.15-7.16 (raw "b")))
	(e-string-part @8.2-8.55 (raw " in here, and there are whitespaces at the end ->		  ")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
