# META
~~~ini
description=Package header missing opening curly for packages
type=file
~~~
# SOURCE
~~~roc
package [main]
~~~
# EXPECTED
PARSE ERROR - parse_error_package_missing_curly.md:1:1:1:8
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_package_platform_open_curly`
This is an unexpected parsing error. Please check your syntax.

**parse_error_package_missing_curly.md:1:1:1:8:**
```roc
package [main]
```
^^^^^^^


# TOKENS
~~~zig
KwPackage,OpenSquare,LowerIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_package_platform_open_curly"))
	(statements))
~~~
# FORMATTED
~~~roc
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
