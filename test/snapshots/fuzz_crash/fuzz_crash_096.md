# META
~~~ini
description=Issue #10092: Panic node is not a type annotation tag
type=file
~~~
# SOURCE
~~~roc
T := [].{
	A : T.A
}
~~~
# EXPECTED
MISSING NESTED TYPE - fuzz_crash_096.md:2:6:2:9
# PROBLEMS

┌─────────────────────┐
│ MISSING NESTED TYPE ├─ `T` is in scope, but it doesn't have a nested type ──┐
└┬────────────────────┘  named `A`.                                           │
 │                                                                            │
 │  A : T.A                                                                   │
 │      ‾‾‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_096.md:2:6 ┘


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "T")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-decl
					(header (name "A")
						(args))
					(ty (name "T.A")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "T"))
		(ty-tag-union))
	(s-alias-decl
		(ty-header (name "fuzz_crash_096.T.A"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "T")
			(ty-header (name "T")))
		(alias (type "T.A")
			(ty-header (name "fuzz_crash_096.T.A"))))
	(expressions))
~~~
