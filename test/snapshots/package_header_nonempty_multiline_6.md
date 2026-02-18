# META
~~~ini
description=package_header_nonempty_multiline (6)
type=file
~~~
# SOURCE
~~~roc
package # Comment after keyword
	[ # Comment after exposes open
		something, # Comment after exposed item
		SomeType, # Comment after last exposed item
	]
	{ # Comment after packages open
		somePkg: "../main.roc", # Comment after package
		other: "../../other/main.roc", # Comment after last package
	}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_6.md:3:3:3:12
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_6.md:4:3:4:11
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `something` is exposed, but it is not defined anywhere in this module.

**package_header_nonempty_multiline_6.md:3:3:3:12:**
```roc
		something, # Comment after exposed item
```
		^^^^^^^^^
You can fix this by either defining `something` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `SomeType` is exposed, but it is not defined anywhere in this module.

**package_header_nonempty_multiline_6.md:4:3:4:11:**
```roc
		SomeType, # Comment after last exposed item
```
		^^^^^^^^
You can fix this by either defining `SomeType` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage,
OpenSquare,
LowerIdent,Comma,
UpperIdent,Comma,
CloseSquare,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "something"))
			(exposed-upper-ident (text "SomeType")))
		(packages
			(record-field (name "somePkg")
				(e-string
					(e-string-part (raw "../main.roc"))))
			(record-field (name "other")
				(e-string
					(e-string-part (raw "../../other/main.roc"))))))
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
