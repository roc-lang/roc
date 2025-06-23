# META
~~~ini
description=Various type annotations
type=file
~~~
# SOURCE
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
baz : (a, b, c)
add_one : (U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# PROBLEMS
**UNDECLARED TYPE**
The type ``String`` is not declared in this scope.

This type is referenced here:
**type_annotations.md:7:14:7:20:**
```roc
main! : List(String) -> Result({}, _)
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),Newline(1:1-1:1),
LowerIdent(4:1-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:12),NoSpaceOpenRound(4:12-4:13),LowerIdent(4:13-4:14),Comma(4:14-4:15),LowerIdent(4:16-4:17),Comma(4:17-4:18),Underscore(4:19-4:20),CloseRound(4:20-4:21),Newline(1:1-1:1),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),OpenRound(5:7-5:8),LowerIdent(5:8-5:9),Comma(5:9-5:10),LowerIdent(5:11-5:12),Comma(5:12-5:13),LowerIdent(5:14-5:15),CloseRound(5:15-5:16),Newline(1:1-1:1),
LowerIdent(6:1-6:8),OpColon(6:9-6:10),OpenRound(6:11-6:12),UpperIdent(6:12-6:14),Comma(6:14-6:15),UpperIdent(6:16-6:19),OpArrow(6:20-6:22),UpperIdent(6:23-6:26),CloseRound(6:26-6:27),Newline(1:1-1:1),
LowerIdent(7:1-7:6),OpColon(7:7-7:8),UpperIdent(7:9-7:13),NoSpaceOpenRound(7:13-7:14),UpperIdent(7:14-7:20),CloseRound(7:20-7:21),OpArrow(7:22-7:24),UpperIdent(7:25-7:31),NoSpaceOpenRound(7:31-7:32),OpenCurly(7:32-7:33),CloseCurly(7:33-7:34),Comma(7:34-7:35),Underscore(7:36-7:37),CloseRound(7:37-7:38),Newline(1:1-1:1),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:18),NoSpaceOpenRound(8:18-8:19),NoSpaceOpenRound(8:19-8:20),LowerIdent(8:20-8:21),Comma(8:21-8:22),LowerIdent(8:23-8:24),Comma(8:24-8:25),LowerIdent(8:26-8:27),CloseRound(8:27-8:28),CloseRound(8:28-8:29),EndOfFile(8:29-8:29),
~~~
# PARSE
~~~clojure
(file (1:1-8:29)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(type_anno (3:1-4:4) "foo" (ty "U64"))
		(type_anno (4:1-5:4)
			"bar"
			(apply (4:7-4:21)
				(ty "Thing")
				(ty_var (4:13-4:14) "a")
				(ty_var (4:16-4:17) "b")
				(_)))
		(type_anno (5:1-6:8)
			"baz"
			(tuple (5:7-5:16)
				(ty_var (5:8-5:9) "a")
				(ty_var (5:11-5:12) "b")
				(ty_var (5:14-5:15) "c")))
		(type_anno (6:1-7:6)
			"add_one"
			(fn (6:12-6:26)
				(ty "U8")
				(ty "U16")
				(ty "U32")))
		(type_anno (7:1-8:10)
			"main!"
			(fn (7:9-7:38)
				(apply (7:9-7:21)
					(ty "List")
					(ty "String"))
				(apply (7:25-7:38)
					(ty "Result")
					(record (7:32-7:34))
					(_))))
		(type_anno (8:1-8:29)
			"tag_tuple"
			(apply (8:13-8:29)
				(ty "Value")
				(tuple (8:19-8:28)
					(ty_var (8:20-8:21) "a")
					(ty_var (8:23-8:24) "b")
					(ty_var (8:26-8:27) "c"))))))
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