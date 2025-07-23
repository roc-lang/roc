# META
~~~ini
description=main module from package
type=package
~~~
# SOURCE
~~~roc
package [
    Color,
] {}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - main.md:2:5:2:10
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'Color' is exposed in the module header but is not defined:
**main.md:2:5:2:10:**
```roc
    Color,
```
    ^^^^^


# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:9-1:10),
UpperIdent(2:5-2:10),Comma(2:10-2:11),
CloseSquare(3:1-3:2),OpenCurly(3:3-3:4),CloseCurly(3:4-3:5),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.5
	(package @1.1-3.5
		(exposes @1.9-3.2
			(exposed-upper-ident @2.5-2.10 (text "Color")))
		(packages @3.3-3.5))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[
		Color,
	]
	{}
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
