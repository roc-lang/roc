# META
~~~ini
description=parser formatter stability: qualified wildcard package exposure
type=file
~~~
# SOURCE
~~~roc
package[e,E.a.*]{}
~~~
# EXPECTED
MOD NOT FOUND - fuzz_crash_109.md:1:11:1:16
EXPOSED BUT NOT DEFINED - fuzz_crash_109.md:1:9:1:10
EXPOSED BUT NOT DEFINED - fuzz_crash_109.md:1:11:1:16
# PROBLEMS

┌──────────────────┐
│ MOD NOT FOUND ├─ The mod `a` was not found in this Roc project. ──────┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  package[e,E.a.*]{}                                                        │
 │            ‾‾‾‾‾                                                           │
 └──────────────────────────────────────────────────── fuzz_crash_109.md:1:11 ┘



┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The mod header says that `e` is exposed, ─────┐
└┬────────────────────────┘  but it is not defined anywhere in this mod.   │
 │                                                                            │
 │  package[e,E.a.*]{}                                                        │
 │          ‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_109.md:1:9 ┘

    You can fix this by either defining `e` in this mod, or by removing it
    from the list of exposed values.


┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The mod header says that `.a` is exposed, ────┐
└┬────────────────────────┘  but it is not defined anywhere in this mod.   │
 │                                                                            │
 │  package[e,E.a.*]{}                                                        │
 │            ‾‾‾‾‾                                                           │
 └──────────────────────────────────────────────────── fuzz_crash_109.md:1:11 ┘

    You can fix this by either defining `.a` in this mod, or by removing it
    from the list of exposed values.

# TOKENS
~~~zig
KwPackage,OpenSquare,LowerIdent,Comma,UpperIdent,NoSpaceDotLowerIdent,DotStar,CloseSquare,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "e"))
			(exposed-upper-ident-star (text "E.a")))
		(packages))
	(statements))
~~~
# FORMATTED
~~~roc
package [e, E.a.*] {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "a")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
