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
KwPackage(1:1-1:8),
OpenSquare(2:2-2:3),
LowerIdent(3:3-3:12),Comma(3:12-3:13),
UpperIdent(4:3-4:11),Comma(4:11-4:12),
CloseSquare(5:2-5:3),
OpenCurly(6:2-6:3),
LowerIdent(7:3-7:10),OpColon(7:10-7:11),StringStart(7:12-7:13),StringPart(7:13-7:24),StringEnd(7:24-7:25),Comma(7:25-7:26),
LowerIdent(8:3-8:8),OpColon(8:8-8:9),StringStart(8:10-8:11),StringPart(8:11-8:31),StringEnd(8:31-8:32),Comma(8:32-8:33),
CloseCurly(9:2-9:3),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.3
	(package @1.1-9.3
		(exposes @2.2-5.3
			(exposed-lower-ident @3.3-3.12
				(text "something"))
			(exposed-upper-ident @4.3-4.11 (text "SomeType")))
		(packages @6.2-9.3
			(record-field @7.3-7.25 (name "somePkg")
				(e-string @7.12-7.25
					(e-string-part @7.13-7.24 (raw "../main.roc"))))
			(record-field @8.3-8.32 (name "other")
				(e-string @8.10-8.32
					(e-string-part @8.11-8.31 (raw "../../other/main.roc"))))))
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
