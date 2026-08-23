# META
~~~ini
description=App Header - roc used as a platform shorthand
type=header
~~~
# SOURCE
~~~roc
app [main!] { roc: platform "../main.roc" }
~~~
# EXPECTED
RESERVED DEPENDENCY NAME - app_header__roc_version_reserved.md:1:15:1:42
# PROBLEMS
── ✗ reserved dependency name ───────── app_header__roc_version_reserved.md:1:15

I was parsing a dependency record, and `roc` is used as the name of a platform
or package.

app [main!] { roc: platform "../main.roc" }
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^

The roc name is reserved for pinning the compiler version, so it cannot name a
dependency. Pick a different name for this one.

For example:
    pf: platform "../platform/main.roc"

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app
	(provides
		(exposed-lower-ident
			(text "main!")))
	(record-field (name "roc")
		(e-string
			(e-string-part (raw "../main.roc"))))
	(packages
		(record-field (name "roc")
			(e-string
				(e-string-part (raw "../main.roc"))))))
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
