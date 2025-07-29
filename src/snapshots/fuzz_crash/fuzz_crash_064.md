# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc

~~~
# EXPECTED
MISSING HEADER - :0:0:0:0
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

# TOKENS
~~~zig
EndOfFile(1:1-1:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.1
	(malformed-header @1.1-1.1 (tag "missing_header"))
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
