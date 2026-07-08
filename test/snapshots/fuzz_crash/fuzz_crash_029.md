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
EXPECTED PROVIDES - fuzz_crash_029.md:12:3:12:4
UNEXPECTED TYPE SYNTAX - fuzz_crash_029.md:13:6:13:7
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:7:13:10
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:10:13:11
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:11:13:12
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:13:13:17
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:19:13:20
UNEXPECTED STATEMENT - fuzz_crash_029.md:14:2:14:10
UNEXPECTED STATEMENT - fuzz_crash_029.md:15:3:15:4
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_029.md:15:14:15:15
UNEXPECTED STATEMENT - fuzz_crash_029.md:15:16:15:17
UNEXPECTED STATEMENT - fuzz_crash_029.md:15:17:15:18
UNEXPECTED STATEMENT - fuzz_crash_029.md:16:1:16:3
UNEXPECTED STATEMENT - fuzz_crash_029.md:16:3:16:4
UNEXPECTED STATEMENT - fuzz_crash_029.md:17:3:17:4
MALFORMED TYPE - fuzz_crash_029.md:13:6:13:7
DECLARATION HAS NO VALUE - fuzz_crash_029.md:13:1:13:7
# PROBLEMS

┌────────────────────────┐
│ EXPECTED OPENING BRACE ├─ I was parsing a `packages` section, and I ────────┐
└┬───────────────────────┘  expected an opening `{`.                          │
 │                                                                            │
 │  vides # Cd                                                                │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:11:3 ┘

    Package dependencies are written as record fields inside braces.

    For example:
        packages { base: "../base/main.roc" }

    I found `vides` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌───────────────────┐
│ EXPECTED PROVIDES ├─ I was parsing a platform header, and I expected the ───┐
└┬──────────────────┘  `provides` section.                                    │
 │                                                                            │
 │  { # pen                                                                   │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:12:3 ┘

    A platform header must map host symbols to Roc functions in a `provides`
    record.

    For example:
        provides { "roc_main": main }

    I found `{` here.


┌────────────────────────┐
│ UNEXPECTED TYPE SYNTAX ├─ I was parsing a type annotation, and this token ──┐
└┬───────────────────────┘  cannot start a type here.                         │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │       ‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:13:6 ┘

    Types can be type variables, uppercase type names, function types, tuples,
    records, or tag unions.

    For example:
        List(U64)

    I found `"` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │        ‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:13:7 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `..l` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │           ‾                                                                │
 └─────────────────────────────────────────────────── fuzz_crash_029.md:13:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `"` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │            ‾                                                               │
 └─────────────────────────────────────────────────── fuzz_crash_029.md:13:11 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `,` here.
    A comma separates items, but there must be a valid item on both sides of it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │              ‾‾‾‾                                                          │
 └─────────────────────────────────────────────────── fuzz_crash_029.md:13:13 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `mmen` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │                    ‾                                                       │
 └─────────────────────────────────────────────────── fuzz_crash_029.md:13:19 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `}` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  provides # Cd                                                             │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:14:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `provides` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  [ Ok(world), (n # pen                                                     │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:15:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `[` here.


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  [ Ok(world), (n # pen                                                     │
 │             ‾                                                              │
 └─────────────────────────────────────────────────── fuzz_crash_029.md:15:14 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `,` here.
    A comma separates items, but there must be a valid item on both sides of it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  [ Ok(world), (n # pen                                                     │
 │               ‾                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_029.md:15:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  [ Ok(world), (n # pen                                                     │
 │                ‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_029.md:15:17 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `n` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ar,                                                                       │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:16:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `ar` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ar,                                                                       │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:16:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `,` here.
    A comma separates items, but there must be a valid item on both sides of it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ]                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:17:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `]` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │       ‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:13:6 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  pkg: "..l", mmen  } # Cose                                                │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_029.md:13:1 ┘

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
