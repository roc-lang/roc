# META
~~~ini
description=platform_header_nonempty (1)
type=file
~~~
# SOURCE
~~~roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
			} # Comment after signatures close
	exposes # Comment after exposes keyword
		[ # Comment after exposes open
			foo, # Comment after exposed item
		] # Comment after exposes close
	packages # Comment after packages keyword
		{ # Comment after packages open
			some_pkg: "../some_pkg.roc", # Comment after package
		} # Comment after packages close
	provides # Comment after provides keyword
		[ # Comment after provides open
			bar, # Comment after exposed item
		]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_header_nonempty_1.md:12:4:12:7
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'foo' is exposed in the module header but is not defined:
**platform_header_nonempty_1.md:12:4:12:7:**
```roc
			foo, # Comment after exposed item
```
   ^^^


# TOKENS
~~~zig
KwPlatform(1:1-1:9),
StringStart(2:2-2:3),StringPart(2:3-2:6),StringEnd(2:6-2:7),
KwRequires(3:2-3:10),
OpenCurly(4:3-4:4),
UpperIdent(5:4-5:8),Comma(5:8-5:9),
CloseCurly(6:3-6:4),
OpenCurly(7:4-7:5),
LowerIdent(8:5-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:17),NoSpaceOpenRound(8:17-8:18),UpperIdent(8:18-8:21),CloseRound(8:21-8:22),OpFatArrow(8:23-8:25),OpenCurly(8:26-8:27),CloseCurly(8:27-8:28),Comma(8:28-8:29),
CloseCurly(9:4-9:5),
KwExposes(10:2-10:9),
OpenSquare(11:3-11:4),
LowerIdent(12:4-12:7),Comma(12:7-12:8),
CloseSquare(13:3-13:4),
KwPackages(14:2-14:10),
OpenCurly(15:3-15:4),
LowerIdent(16:4-16:12),OpColon(16:12-16:13),StringStart(16:14-16:15),StringPart(16:15-16:30),StringEnd(16:30-16:31),Comma(16:31-16:32),
CloseCurly(17:3-17:4),
KwProvides(18:2-18:10),
OpenSquare(19:3-19:4),
LowerIdent(20:4-20:7),Comma(20:7-20:8),
CloseSquare(21:3-21:4),EndOfFile(21:4-21:4),
~~~
# PARSE
~~~clojure
(file @1.1-21.4
	(platform @1.1-21.4 (name "foo")
		(rigids @4.3-6.4
			(exposed-upper-ident @5.4-5.8 (text "Main")))
		(ty-record @7.4-9.5
			(anno-record-field @8.5-8.28 (name "main!")
				(ty-fn @8.13-8.28
					(ty-apply @8.13-8.22
						(ty @8.13-8.17 (name "List"))
						(ty @8.18-8.21 (name "Str")))
					(ty-record @8.26-8.28))))
		(exposes @11.3-13.4
			(exposed-lower-ident @12.4-12.7
				(text "foo")))
		(packages @15.3-17.4
			(record-field @16.4-16.31 (name "some_pkg")
				(e-string @16.14-16.31
					(e-string-part @16.15-16.30 (raw "../some_pkg.roc")))))
		(provides @19.3-21.4
			(exposed-lower-ident @20.4-20.7
				(text "bar"))))
	(statements))
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
