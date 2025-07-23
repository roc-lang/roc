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
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:3:2:12
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:14:2:22
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'something' is exposed in the module header but is not defined:
**package_header_nonempty_multiline_3.md:2:3:2:12:**
```roc
	[something, SomeType,]
```
  ^^^^^^^^^


**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'SomeType' is exposed in the module header but is not defined:
**package_header_nonempty_multiline_3.md:2:14:2:22:**
```roc
	[something, SomeType,]
```
             ^^^^^^^^


# TOKENS
~~~zig
KwPackage(1:1-1:8),
OpenSquare(2:2-2:3),LowerIdent(2:3-2:12),Comma(2:12-2:13),UpperIdent(2:14-2:22),Comma(2:22-2:23),CloseSquare(2:23-2:24),
OpenCurly(3:2-3:3),LowerIdent(3:4-3:11),OpColon(3:11-3:12),StringStart(3:13-3:14),StringPart(3:14-3:25),StringEnd(3:25-3:26),Comma(3:26-3:27),CloseCurly(3:28-3:29),EndOfFile(3:29-3:29),
~~~
# PARSE
~~~clojure
(file @1.1-3.29
	(package @1.1-3.29
		(exposes @2.2-2.24
			(exposed-lower-ident @2.3-2.12
				(text "something"))
			(exposed-upper-ident @2.14-2.22 (text "SomeType")))
		(packages @3.2-3.29
			(record-field @3.4-3.26 (name "somePkg")
				(e-string @3.13-3.26
					(e-string-part @3.14-3.25 (raw "../main.roc"))))))
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
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
