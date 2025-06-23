# META
~~~ini
description=platform_header_empty (1)
type=file
~~~
# SOURCE
~~~roc
platform "foo"
	requires {} {}
	exposes []
	packages {}
	provides []
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:14),StringEnd(1:14-1:15),Newline(1:1-1:1),
KwRequires(2:2-2:10),OpenCurly(2:11-2:12),CloseCurly(2:12-2:13),OpenCurly(2:14-2:15),CloseCurly(2:15-2:16),Newline(1:1-1:1),
KwExposes(3:2-3:9),OpenSquare(3:10-3:11),CloseSquare(3:11-3:12),Newline(1:1-1:1),
KwPackages(4:2-4:10),OpenCurly(4:11-4:12),CloseCurly(4:12-4:13),Newline(1:1-1:1),
KwProvides(5:2-5:10),OpenSquare(5:11-5:12),CloseSquare(5:12-5:13),EndOfFile(5:13-5:13),
~~~
# PARSE
~~~clojure
(file (1:1-5:13)
	(platform (1:1-5:13)
		"foo"
		(rigids (2:11-2:13))
		(record (2:14-2:16))
		(exposes (3:10-3:12))
		(packages (4:11-4:13))
		(provides (5:11-5:13)))
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~