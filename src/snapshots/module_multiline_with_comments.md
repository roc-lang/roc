# META
~~~ini
description=An empty module with multiline exposes and comments
type=file
~~~
# SOURCE
~~~roc
module # Comment after module keyword
	[ # Comment After exposes open
		something, # Comment after exposed item
		SomeType, # Comment after final exposed item
	]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - module_multiline_with_comments.md:3:3:3:12
EXPOSED BUT NOT DEFINED - module_multiline_with_comments.md:4:3:4:11
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that ``something`` is exposed, but it is not defined anywhere in this module.

**module_multiline_with_comments.md:3:3:3:12:**
```roc
		something, # Comment after exposed item
```
  ^^^^^^^^^
You can fix this by either defining ``something`` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that ``SomeType`` is exposed, but it is not defined anywhere in this module.

**module_multiline_with_comments.md:4:3:4:11:**
```roc
		SomeType, # Comment after final exposed item
```
  ^^^^^^^^
You can fix this by either defining ``SomeType`` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),
OpenSquare(2:2-2:3),
LowerIdent(3:3-3:12),Comma(3:12-3:13),
UpperIdent(4:3-4:11),Comma(4:11-4:12),
CloseSquare(5:2-5:3),EndOfFile(5:3-5:3),
~~~
# PARSE
~~~clojure
(file @1.1-5.3
	(module @1.1-5.3
		(exposes @2.2-5.3
			(exposed-lower-ident @3.3-3.12
				(text "something"))
			(exposed-upper-ident @4.3-4.11 (text "SomeType"))))
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
