# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ #
		] # Cse
	packages # Cd
		vides # Cd
		{ # pen
pkg: "..l", mmen		} # Cose
	provides # Cd
		[ Ok(world), (n # pen
ar,
		]
~~~
# EXPECTED
EXPECTED OPENING BRACE - fuzz_crash_029.md:11:3:11:8
PARSE ERROR - fuzz_crash_029.md:12:3:12:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_029.md:13:6:13:7
PARSE ERROR - fuzz_crash_029.md:13:7:13:10
PARSE ERROR - fuzz_crash_029.md:13:10:13:11
PARSE ERROR - fuzz_crash_029.md:13:11:13:12
PARSE ERROR - fuzz_crash_029.md:13:13:13:17
PARSE ERROR - fuzz_crash_029.md:13:19:13:20
PARSE ERROR - fuzz_crash_029.md:14:2:14:10
PARSE ERROR - fuzz_crash_029.md:15:3:15:4
PARSE ERROR - fuzz_crash_029.md:15:14:15:15
PARSE ERROR - fuzz_crash_029.md:15:16:15:17
PARSE ERROR - fuzz_crash_029.md:15:17:15:18
PARSE ERROR - fuzz_crash_029.md:16:1:16:3
PARSE ERROR - fuzz_crash_029.md:16:3:16:4
PARSE ERROR - fuzz_crash_029.md:17:3:17:4
MALFORMED TYPE - fuzz_crash_029.md:13:6:13:7
DECLARATION HAS NO VALUE - fuzz_crash_029.md:13:1:13:7
# PROBLEMS
                                                      ┌────────────────────────┐
┌─ Platform headers must have a packages section ─────┤ EXPECTED OPENING BRACE │
│  that lists package dependencies.                   └───────────────────────┬┘
│                                                                             │
│    vides # Cd                                                               │
│    ‾‾‾‾‾                                                                    │
└───────────────────────────────────────────────────── fuzz_crash_029.md:11:3 ┘

    For example:     packages { base: "../base/main.roc" }
                                                                 ┌─────────────┐
┌─ A parsing error occurred: expected_provides ──────────────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│    { # pen                                                                  │
│    ‾                                                                        │
└───────────────────────────────────────────────────── fuzz_crash_029.md:12:3 ┘

    This is an unexpected parsing error. Please check your syntax.
                                         ┌─────────────────────────────────────┐
┌─ The token " is not expected in a ─────┤ UNEXPECTED TOKEN IN TYPE ANNOTATION │
│  type annotation.                      └────────────────────────────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│       ‾                                                                     │
└───────────────────────────────────────────────────── fuzz_crash_029.md:13:6 ┘

    Type annotations should contain types like Str, Num a, or List U64.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│        ‾‾‾                                                                  │
└───────────────────────────────────────────────────── fuzz_crash_029.md:13:7 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│           ‾                                                                 │
└──────────────────────────────────────────────────── fuzz_crash_029.md:13:10 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│            ‾                                                                │
└──────────────────────────────────────────────────── fuzz_crash_029.md:13:11 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│              ‾‾‾‾                                                           │
└──────────────────────────────────────────────────── fuzz_crash_029.md:13:13 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│                    ‾                                                        │
└──────────────────────────────────────────────────── fuzz_crash_029.md:13:19 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│   provides # Cd                                                             │
│   ‾‾‾‾‾‾‾‾                                                                  │
└───────────────────────────────────────────────────── fuzz_crash_029.md:14:2 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│    [ Ok(world), (n # pen                                                    │
│    ‾                                                                        │
└───────────────────────────────────────────────────── fuzz_crash_029.md:15:3 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ Type applications require parentheses around their type ──────┤ PARSE ERROR │
│  arguments.                                                    └────────────┬┘
│                                                                             │
│    [ Ok(world), (n # pen                                                    │
│               ‾                                                             │
└──────────────────────────────────────────────────── fuzz_crash_029.md:15:14 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│    [ Ok(world), (n # pen                                                    │
│                 ‾                                                           │
└──────────────────────────────────────────────────── fuzz_crash_029.md:15:16 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│    [ Ok(world), (n # pen                                                    │
│                  ‾                                                          │
└──────────────────────────────────────────────────── fuzz_crash_029.md:15:17 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  ar,                                                                        │
│  ‾‾                                                                         │
└───────────────────────────────────────────────────── fuzz_crash_029.md:16:1 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│  ar,                                                                        │
│    ‾                                                                        │
└───────────────────────────────────────────────────── fuzz_crash_029.md:16:3 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                                 ┌─────────────┐
┌─ A parsing error occurred: statement_unexpected_token ─────────┤ PARSE ERROR │
│                                                                └────────────┬┘
│                                                                             │
│    ]                                                                        │
│    ‾                                                                        │
└───────────────────────────────────────────────────── fuzz_crash_029.md:17:3 ┘

    This is an unexpected parsing error. Please check your syntax.
                                                              ┌────────────────┐
┌─ This type annotation is malformed or contains invalid ─────┤ MALFORMED TYPE │
│  syntax.                                                    └───────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│       ‾                                                                     │
└───────────────────────────────────────────────────── fuzz_crash_029.md:13:6 ┘

                                                    ┌──────────────────────────┐
┌─ This declaration has a type annotation but no ───┤ DECLARATION HAS NO VALUE │
│  implementation.                                  └─────────────────────────┬┘
│                                                                             │
│  pkg: "..l", mmen  } # Cose                                                 │
│  ‾‾‾‾‾‾                                                                     │
└───────────────────────────────────────────────────── fuzz_crash_029.md:13:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.
# TOKENS
~~~zig
KwPlatform,
StringStart,StringPart,StringEnd,
KwRequires,
OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,Comma,
CloseCurly,
KwExposes,
OpenSquare,
CloseSquare,
KwPackages,
LowerIdent,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseCurly,
KwProvides,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,
LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_provides"))
	(statements
		(s-type-anno (name "pkg")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
# pen
pkg : 
# Cose
# Cd
# pen

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "pkg"))
		(e-anno-only)
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
