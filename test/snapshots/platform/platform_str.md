# META
~~~ini
description=the str test platform
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {} { processString : Str -> Str }
    exposes []
    packages {}
    provides { processString: "processString" }

processString : Str -> Str
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:11),StringEnd(1:11-1:12),
KwRequires(2:5-2:13),OpenCurly(2:14-2:15),CloseCurly(2:15-2:16),OpenCurly(2:17-2:18),LowerIdent(2:19-2:32),OpColon(2:33-2:34),UpperIdent(2:35-2:38),OpArrow(2:39-2:41),UpperIdent(2:42-2:45),CloseCurly(2:46-2:47),
KwExposes(3:5-3:12),OpenSquare(3:13-3:14),CloseSquare(3:14-3:15),
KwPackages(4:5-4:13),OpenCurly(4:14-4:15),CloseCurly(4:15-4:16),
KwProvides(5:5-5:13),OpenCurly(5:14-5:15),LowerIdent(5:16-5:29),OpColon(5:29-5:30),StringStart(5:31-5:32),StringPart(5:32-5:45),StringEnd(5:45-5:46),CloseCurly(5:47-5:48),
LowerIdent(7:1-7:14),OpColon(7:15-7:16),UpperIdent(7:17-7:20),OpArrow(7:21-7:23),UpperIdent(7:24-7:27),EndOfFile(7:27-7:27),
~~~
# PARSE
~~~clojure
(file @1.1-7.27
	(platform @1.1-5.48 (name "")
		(rigids @2.14-2.16)
		(ty-record @2.17-2.47
			(anno-record-field @2.19-2.45 (name "processString")
				(ty-fn @2.35-2.45
					(ty @2.35-2.38 (name "Str"))
					(ty @2.42-2.45 (name "Str")))))
		(exposes @3.13-3.15)
		(packages @4.14-4.16)
		(provides @5.14-5.48
			(record-field @5.16-5.46 (name "processString")
				(e-string @5.31-5.46
					(e-string-part @5.32-5.45 (raw "processString"))))))
	(statements
		(s-type-anno @7.1-7.27 (name "processString")
			(ty-fn @7.17-7.27
				(ty @7.17-7.20 (name "Str"))
				(ty @7.24-7.27 (name "Str"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {} { processString : Str -> Str }
	exposes []
	packages {}
	provides { processString: "processString" }

processString : Str -> Str
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
