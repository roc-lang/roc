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
		{ # Comment after provides open
			bar: "roc__bar", # Comment after provides entry
		}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_header_nonempty_1.md:12:4:12:7
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `foo` is exposed, but it is not defined anywhere in this module.

**platform_header_nonempty_1.md:12:4:12:7:**
```roc
			foo, # Comment after exposed item
```
			^^^
You can fix this by either defining `foo` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPlatform,
StringStart,StringPart,StringEnd,
KwRequires,
OpenCurly,
UpperIdent,Comma,
CloseCurly,
OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,Comma,
CloseCurly,
KwExposes,
OpenSquare,
LowerIdent,Comma,
CloseSquare,
KwPackages,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
KwProvides,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "foo")
		(rigids
			(exposed-upper-ident (text "Main")))
		(ty-record
			(anno-record-field (name "main!")
				(ty-fn
					(ty-apply
						(ty (name "List"))
						(ty (name "Str")))
					(ty-record))))
		(exposes
			(exposed-lower-ident
				(text "foo")))
		(packages
			(record-field (name "some_pkg")
				(e-string
					(e-string-part (raw "../some_pkg.roc")))))
		(provides
			(record-field (name "bar")
				(e-string
					(e-string-part (raw "roc__bar"))))))
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
