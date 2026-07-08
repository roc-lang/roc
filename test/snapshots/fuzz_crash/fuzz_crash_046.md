# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import fS
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
INCOMPLETE IMPORT - fuzz_crash_046.md:1:20:1:26
# PROBLEMS

ASCII CONTROL CHARACTER

ASCII control characters are not allowed in Roc source code.



┌───────────────────┐
│ INCOMPLETE IMPORT ├─ I was parsing an import, and the module path is ───────┐
└┬──────────────────┘  incomplete.                                            │
 │                                                                            │
 │  app[]{f:platform""}import fS                                             │
 │                     ‾‾‾‾‾‾                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_046.md:1:20 ┘

    Imports must name a module, optionally with a qualifier and exposing list.

    For example:
        import Json.Decode exposing [decode]

    I found `import` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.

# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,KwImport,LowerIdent,UpperIdent,
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
					(e-string-part (raw ""))))))
	(statements
		(s-malformed (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
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
