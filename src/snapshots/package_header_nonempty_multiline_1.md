# META
~~~ini
description=package_header_nonempty_multiline (1)
type=file
~~~
# SOURCE
~~~roc
package # This comment is here
	[something, SomeType]
	{ somePkg: "../main.roc" }
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_1.md:2:3:2:12
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_1.md:2:14:2:22
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that ``something`` is exposed, but it is not defined anywhere in this module.

**package_header_nonempty_multiline_1.md:2:3:2:12:**
```roc
	[something, SomeType]
```
  ^^^^^^^^^
You can fix this by either defining ``something`` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that ``SomeType`` is exposed, but it is not defined anywhere in this module.

**package_header_nonempty_multiline_1.md:2:14:2:22:**
```roc
	[something, SomeType]
```
             ^^^^^^^^
You can fix this by either defining ``SomeType`` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage(1:1-1:8),
OpenSquare(2:2-2:3),LowerIdent(2:3-2:12),Comma(2:12-2:13),UpperIdent(2:14-2:22),CloseSquare(2:22-2:23),
OpenCurly(3:2-3:3),LowerIdent(3:4-3:11),OpColon(3:11-3:12),StringStart(3:13-3:14),StringPart(3:14-3:25),StringEnd(3:25-3:26),CloseCurly(3:27-3:28),EndOfFile(3:28-3:28),
~~~
# PARSE
~~~clojure
(file @1.1-3.28
	(package @1.1-3.28
		(exposes @2.2-2.23
			(exposed-lower-ident @2.3-2.12
				(text "something"))
			(exposed-upper-ident @2.14-2.22 (text "SomeType")))
		(packages @3.2-3.28
			(record-field @3.4-3.26 (name "somePkg")
				(e-string @3.13-3.26
					(e-string-part @3.14-3.25 (raw "../main.roc"))))))
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
