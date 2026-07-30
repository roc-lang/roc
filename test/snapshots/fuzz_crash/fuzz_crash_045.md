# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform""requires{}{}exposes[]packages{}provides[
~~~
# EXPECTED
EXPECTED OPENING BRACE - fuzz_crash_045.md:1:50:1:51
# PROBLEMS

┌────────────────────────┐
│ EXPECTED OPENING BRACE ├─ I was parsing a `provides` section, and I ────────┐
└┬───────────────────────┘  expected an opening `{`.                          │
 │                                                                            │
 │  platform""requires{}{}exposes[]packages{}provides[                        │
 │                                                   ‾                        │
 └──────────────────────────────────────────────────── fuzz_crash_045.md:1:50 ┘

    Host symbol mappings are written as record-like entries inside braces.

    For example:
        provides { "roc_main": main }

    I found `[` here.

# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,OpenCurly,CloseCurly,OpenCurly,CloseCurly,KwExposes,OpenSquare,CloseSquare,KwPackages,OpenCurly,CloseCurly,KwProvides,OpenSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires)
		(exposes)
		(packages)
		(provides))
	(statements))
~~~
# FORMATTED
~~~roc
platform ""
	requires {}
	exposes []
	packages {}
	provides {}
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
