# META
~~~ini
description=An empty module with singleline exposes
type=file
~~~
# SOURCE
~~~roc
module [something, SomeType]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - module_nonempty_single.md:1:9:1:18
EXPOSED BUT NOT DEFINED - module_nonempty_single.md:1:20:1:28
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'something' is exposed in the module header but is not defined:
**module_nonempty_single.md:1:9:1:18:**
```roc
module [something, SomeType]
```
        ^^^^^^^^^


**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'SomeType' is exposed in the module header but is not defined:
**module_nonempty_single.md:1:20:1:28:**
```roc
module [something, SomeType]
```
                   ^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:18),Comma(1:18-1:19),UpperIdent(1:20-1:28),CloseSquare(1:28-1:29),EndOfFile(1:29-1:29),
~~~
# PARSE
~~~clojure
(file @1.1-1.29
	(module @1.1-1.29
		(exposes @1.8-1.29
			(exposed-lower-ident @1.9-1.18
				(text "something"))
			(exposed-upper-ident @1.20-1.28 (text "SomeType"))))
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
