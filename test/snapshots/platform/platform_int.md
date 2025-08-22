# META
~~~ini
description=the int test platform
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {} { multiplyInts : I64, I64 -> I64 }
    exposes []
    packages {}
    provides { multiplyInts: "multiplyInts" }

multiplyInts : I64, I64 -> I64
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:11),StringEnd(1:11-1:12),
KwRequires(2:5-2:13),OpenCurly(2:14-2:15),CloseCurly(2:15-2:16),OpenCurly(2:17-2:18),LowerIdent(2:19-2:31),OpColon(2:32-2:33),UpperIdent(2:34-2:37),Comma(2:37-2:38),UpperIdent(2:39-2:42),OpArrow(2:43-2:45),UpperIdent(2:46-2:49),CloseCurly(2:50-2:51),
KwExposes(3:5-3:12),OpenSquare(3:13-3:14),CloseSquare(3:14-3:15),
KwPackages(4:5-4:13),OpenCurly(4:14-4:15),CloseCurly(4:15-4:16),
KwProvides(5:5-5:13),OpenCurly(5:14-5:15),LowerIdent(5:16-5:28),OpColon(5:28-5:29),StringStart(5:30-5:31),StringPart(5:31-5:43),StringEnd(5:43-5:44),CloseCurly(5:45-5:46),
LowerIdent(7:1-7:13),OpColon(7:14-7:15),UpperIdent(7:16-7:19),Comma(7:19-7:20),UpperIdent(7:21-7:24),OpArrow(7:25-7:27),UpperIdent(7:28-7:31),EndOfFile(7:31-7:31),
~~~
# PARSE
~~~clojure
(file @1.1-7.31
	(platform @1.1-5.46 (name "")
		(rigids @2.14-2.16)
		(ty-record @2.17-2.51
			(anno-record-field @2.19-2.49 (name "multiplyInts")
				(ty-fn @2.34-2.49
					(ty @2.34-2.37 (name "I64"))
					(ty @2.39-2.42 (name "I64"))
					(ty @2.46-2.49 (name "I64")))))
		(exposes @3.13-3.15)
		(packages @4.14-4.16)
		(provides @5.14-5.46
			(record-field @5.16-5.44 (name "multiplyInts")
				(e-string @5.30-5.44
					(e-string-part @5.31-5.43 (raw "multiplyInts"))))))
	(statements
		(s-type-anno @7.1-7.31 (name "multiplyInts")
			(ty-fn @7.16-7.31
				(ty @7.16-7.19 (name "I64"))
				(ty @7.21-7.24 (name "I64"))
				(ty @7.28-7.31 (name "I64"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {} { multiplyInts : I64, I64 -> I64 }
	exposes []
	packages {}
	provides { multiplyInts: "multiplyInts" }

multiplyInts : I64, I64 -> I64
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
