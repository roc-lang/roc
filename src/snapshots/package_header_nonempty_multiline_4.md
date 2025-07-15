# META
~~~ini
description=package_header_nonempty_multiline (4)
type=file
~~~
# SOURCE
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
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_4.md:3:3:3:12
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_4.md:4:3:4:11
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that ``something`` is exposed, but it is not defined anywhere in this module.

**package_header_nonempty_multiline_4.md:3:3:3:12:**
```roc
		something,
```
  ^^^^^^^^^
You can fix this by either defining ``something`` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that ``SomeType`` is exposed, but it is not defined anywhere in this module.

**package_header_nonempty_multiline_4.md:4:3:4:11:**
```roc
		SomeType,
```
  ^^^^^^^^
You can fix this by either defining ``SomeType`` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage(1:1-1:8),
OpenSquare(2:2-2:3),
LowerIdent(3:3-3:12),Comma(3:12-3:13),
UpperIdent(4:3-4:11),Comma(4:11-4:12),
CloseSquare(5:2-5:3),
OpenCurly(6:2-6:3),
LowerIdent(7:3-7:10),OpColon(7:10-7:11),StringStart(7:12-7:13),StringPart(7:13-7:24),StringEnd(7:24-7:25),Comma(7:25-7:26),
CloseCurly(8:2-8:3),EndOfFile(8:3-8:3),
~~~
# PARSE
~~~clojure
(file @1.1-8.3
	(package @1.1-8.3
		(exposes @2.2-5.3
			(exposed-lower-ident @3.3-3.12
				(text "something"))
			(exposed-upper-ident @4.3-4.11 (text "SomeType")))
		(packages @6.2-8.3
			(record-field @7.3-7.25 (name "somePkg")
				(e-string @7.12-7.25
					(e-string-part @7.13-7.24 (raw "../main.roc"))))))
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
