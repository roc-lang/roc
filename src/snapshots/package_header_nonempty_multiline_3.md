# META
~~~ini
description=package_header_nonempty_multiline (3)
type=file
~~~
# SOURCE
~~~roc
package
	[something, SomeType,]
	{ somePkg: "../main.roc", }
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage(1:1-1:8),Newline(1:1-1:1),
OpenSquare(2:2-2:3),LowerIdent(2:3-2:12),Comma(2:12-2:13),UpperIdent(2:14-2:22),Comma(2:22-2:23),CloseSquare(2:23-2:24),Newline(1:1-1:1),
OpenCurly(3:2-3:3),LowerIdent(3:4-3:11),OpColon(3:11-3:12),StringStart(3:13-3:14),StringPart(3:14-3:25),StringEnd(3:25-3:26),Comma(3:26-3:27),CloseCurly(3:28-3:29),EndOfFile(3:29-3:29),
~~~
# PARSE
~~~clojure
(file (1:1-3:29)
	(package (1:1-3:29)
		(exposes (2:2-2:24)
			(exposed_item (lower_ident "something"))
			(exposed_item (upper_ident "SomeType")))
		(packages (3:2-3:29)
			(record_field (3:4-3:27)
				"somePkg"
				(string (3:13-3:26) (string_part (3:14-3:25) "../main.roc")))))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[
		something,
		SomeType,
	]
	{
		somePkg: "../main.roc",
	}
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~