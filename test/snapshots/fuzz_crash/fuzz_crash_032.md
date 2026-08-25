# META
~~~ini
description=fuzz crash, unterminated single quote
type=snippet
~~~
# SOURCE
~~~roc
LocalStatus :lue => Loc= [Pending, Complete]

olor : _ -> tus
olor = |color| { import Color.RGB

    match color { RGB => LocalStatus.Pending
Green => LocalStatus-Complete
  B.Blue => LocalStatus.Pending
    }
}
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_032.md:1:24:1:25
UNEXPECTED STATEMENT - fuzz_crash_032.md:1:26:1:27
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_032.md:1:34:1:35
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_032.md:1:44:1:45
IMPORT MUST BE TOP LEVEL - fuzz_crash_032.md:4:18:4:24
UNEXPECTED PATTERN SYNTAX - fuzz_crash_032.md:7:21:7:22
MISSING MATCH ARROW - fuzz_crash_032.md:7:22:7:22
UNDECLARED TYPE VARIABLE - fuzz_crash_032.md:1:14:1:17
UNDECLARED TYPE - fuzz_crash_032.md:1:21:1:24
UNDECLARED TYPE - fuzz_crash_032.md:4:25:4:30
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:6:26:6:37
INVALID PATTERN - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:8:3:8:4
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:8:13:8:24
TYPE MISMATCH - fuzz_crash_032.md:7:10:7:21
# PROBLEMS
── ✗ unexpected statement ─────────────────────────────── fuzz_crash_032.md:1:24

I was parsing a statement, and this token cannot start a statement here.

LocalStatus :lue => Loc= [Pending, Complete]
                       ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found = here.

── ✗ unexpected statement ─────────────────────────────── fuzz_crash_032.md:1:26

I was parsing a statement, and this token cannot start a statement here.

LocalStatus :lue => Loc= [Pending, Complete]
                         ^

Statements can be declarations, type annotations, imports, expectations,
returns, crashes, loops, or expression statements inside a block.

For example:
    answer = 42

I found [ here.

── ✗ type application needs parentheses ───────────────── fuzz_crash_032.md:1:34

I was parsing a type annotation, and I found a type argument without
parentheses.

LocalStatus :lue => Loc= [Pending, Complete]
                                 ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found , here.
A comma separates items, but there must be a valid item on both sides of it.

── ✗ type application needs parentheses ───────────────── fuzz_crash_032.md:1:44

I was parsing a type annotation, and I found a type argument without
parentheses.

LocalStatus :lue => Loc= [Pending, Complete]
                                           ^

Roc type applications use parentheses around their arguments. Write List(U8),
not List U8.

For example:
    List(U8)

I found ] here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ import must be top level ─────────────────────────── fuzz_crash_032.md:4:18

I was parsing an import, but imports are only allowed at the top level.

olor = |color| { import Color.RGB
                 ^^^^^^

Move this import after the mod header and before declarations or executable
statements.

For example:
    import Json

    main = 1

I found import here.
That word is reserved by Roc, so it cannot be used as a name in this position.

── ✗ unexpected pattern syntax ────────────────────────── fuzz_crash_032.md:7:21

I was parsing a pattern, and this token cannot start a pattern here.

Green => LocalStatus-Complete
                    ^

Patterns can be lowercase names, tags, literals, lists, records, tuples,
underscores, or nested patterns.

For example:
    { name, age }

I found - here.

── ✗ missing match arrow ──────────────────────────────── fuzz_crash_032.md:7:22

I was parsing a match branch, and I expected `=>` before the branch body.

Green => LocalStatus-Complete
                     ^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ undeclared type variable ─────────────────────────── fuzz_crash_032.md:1:14

The type variable lue is not declared in this scope.

LocalStatus :lue => Loc= [Pending, Complete]
             ^^^

Type variables must be introduced in a type annotation before they can be used.

── ✗ undeclared type ──────────────────────────────────── fuzz_crash_032.md:1:21

The type Loc is not declared in this scope.

LocalStatus :lue => Loc= [Pending, Complete]
                    ^^^

── ✗ undeclared type ──────────────────────────────────── fuzz_crash_032.md:4:25

The type Color is not declared in this scope.

olor = |color| { import Color.RGB
                        ^^^^^

── ✗ expected nominal type ────────────────────────────── fuzz_crash_032.md:6:26

You are using the type LocalStatus like a nominal type, but it is an alias.

match color { RGB => LocalStatus.Pending
                     ^^^^^^^^^^^

Hint: You can declare this type with := to make it nominal.

── ✗ invalid pattern ───────────────────────────────────────────────────────────

This pattern contains invalid syntax or uses unsupported features.

── ✗ undeclared type ───────────────────────────────────── fuzz_crash_032.md:8:3

The type B is not declared in this scope.

B.Blue => LocalStatus.Pending
^

── ✗ expected nominal type ────────────────────────────── fuzz_crash_032.md:8:13

You are using the type LocalStatus like a nominal type, but it is an alias.

B.Blue => LocalStatus.Pending
          ^^^^^^^^^^^

Hint: You can declare this type with := to make it nominal.

── ✗ type mismatch ────────────────────────────────────── fuzz_crash_032.md:7:10

The second branch of this match does not match the previous branches .

Green => LocalStatus-Complete
         ^^^^^^^^^^^

The second branch is:

    [LocalStatus, ..]

But the previous branches result in:

    tus

All branches in a match must have compatible types.
Note: You can wrap branches values in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
UpperIdent,OpColon,LowerIdent,OpFatArrow,UpperIdent,OpAssign,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,Underscore,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,KwImport,UpperIdent,NoSpaceDotUpperIdent,
KwMatch,LowerIdent,OpenCurly,UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,OpFatArrow,UpperIdent,OpUnaryMinus,UpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "LocalStatus")
				(args))
			(ty-fn
				(ty-var (raw "lue"))
				(ty (name "Loc"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-type-anno (name "olor")
			(ty-fn
				(_)
				(ty-var (raw "tus"))))
		(s-decl
			(p-ident (raw "olor"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-block
					(statements
						(s-malformed (tag "import_must_be_top_level"))
						(e-tag (raw "Color.RGB"))
						(e-match
							(e-ident (raw "color"))
							(branches
								(branch
									(p-tag (raw "RGB"))
									(e-tag (raw "LocalStatus.Pending")))
								(branch
									(p-tag (raw "Green"))
									(e-tag (raw "LocalStatus")))
								(branch
									(p-malformed (tag "pattern_unexpected_token"))
									(e-tag (raw "Complete")))
								(branch
									(p-tag (raw ".Blue"))
									(e-tag (raw "LocalStatus.Pending")))))))))))
~~~
# FORMATTED
~~~roc
LocalStatus : lue => Loc


olor : _ -> tus
olor = |color| {
		Color.RGB

	match color {
		RGB => LocalStatus.Pending
		Green => LocalStatus
		 => Complete
		B.Blue => LocalStatus.Pending
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "olor"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-underscore)
				(ty-rigid-var (name "tus")))))
	(s-alias-decl
		(ty-header (name "LocalStatus"))
		(ty-fn (effectful true)
			(ty-malformed)
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> tus")))
	(type_decls
		(alias (type "Error")
			(ty-header (name "LocalStatus"))))
	(expressions
		(expr (type "_arg -> tus"))))
~~~
