# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform"",r:"
}
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_058.md:1:22:1:23
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**fuzz_crash_058.md:1:22:1:23:**
```roc
app[]{f:platform"",r:"
```
                     ^


# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))
			(record-field (name "r")
				(e-string
					(e-string-part (raw ""))))))
	(statements))
~~~
# FORMATTED
~~~roc
app [] {
	f: platform "",
	r: "",
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
