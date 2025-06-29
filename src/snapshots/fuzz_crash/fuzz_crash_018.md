# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_018.md:1:1:1:4:**
```roc
0 b:S
```


# TOKENS
~~~zig
Int(1:1-1:2),LowerIdent(1:3-1:4),OpColon(1:4-1:5),UpperIdent(1:5-1:6),Newline(1:1-1:1),
DotUpperIdent(2:1-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(file @1.1-2.3
	(malformed-header @1.1-1.4 (tag "missing_header"))
	(statements
		(s-type-anno @1.3-2.3 (name "b")
			(ty-mod (module "R") (name "S")))))
~~~
# FORMATTED
~~~roc
b : S..R
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
