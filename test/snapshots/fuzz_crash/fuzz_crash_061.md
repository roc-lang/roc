# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform"
requires{}{n:0[import S	exposing[
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_061.md:1:9:1:10
UNEXPECTED TYPE SYNTAX - fuzz_crash_061.md:2:14:2:15
EXPECTED CLOSING BRACE - fuzz_crash_061.md:1:1:1:9
EXPECTED CLOSING BRACKET - fuzz_crash_061.md:2:16:2:22
# PROBLEMS
── ✗ unclosed string ───────────────────────────────────── fuzz_crash_061.md:1:9

This string is missing a closing quote.

platform"
        ^

── ✗ unexpected type syntax ───────────────────────────── fuzz_crash_061.md:2:14

I was parsing a type annotation, and this token cannot start a type here.

requires{}{n:0[import S exposing[
             ^

Types can be type variables, uppercase type names, function types, tuples,
records, or tag unions.

For example:
    List(U64)

I found 0 here.

── ✗ expected closing brace ────────────────────────────── fuzz_crash_061.md:1:1

I was parsing a `requires` section, and I expected a closing `}`.

platform"
^^^^^^^^

Close the requires record after the final entrypoint signature.

For example:
    requires { main : {} => I32 }

I found platform here.
That word is reserved by Roc, so it cannot be used as a name in this position.

── ✗ expected closing bracket ─────────────────────────── fuzz_crash_061.md:2:16

I was parsing an import exposing clause, and I expected a closing `]`.

requires{}{n:0[import S exposing[
               ^^^^^^

Close the exposing list after the final imported name.

For example:
    import Json exposing [decode, encode]

I found import here.
That word is reserved by Roc, so it cannot be used as a name in this position.

# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,OpenCurly,LowerIdent,OpColon,Int,OpenSquare,KwImport,UpperIdent,KwExposing,OpenSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_requires_signatures_close_curly"))
	(statements
		(s-malformed (tag "import_exposing_no_close"))))
~~~
# FORMATTED
~~~roc
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
