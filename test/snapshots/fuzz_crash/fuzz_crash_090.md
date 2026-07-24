# META
~~~ini
description=Issue #10096: Invalid formatting - multiline function literal in tuple call
type=file
~~~
# SOURCE
~~~roc
e={({\\
.{f}{})
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_090.md:2:7:2:8
EXPECTED CLOSING BRACE - fuzz_crash_090.md:3:1:3:1
EXPECTED TUPLE SEPARATOR - fuzz_crash_090.md:3:1:3:1
EXPECTED CLOSING BRACE - fuzz_crash_090.md:3:1:3:1
UNRECOGNIZED SYNTAX - fuzz_crash_090.md:1:1:1:1
# PROBLEMS

┌──────────────────────────────┐
│ UNEXPECTED EXPRESSION SYNTAX ├─ I was parsing an expression, and this ──────┐
└┬─────────────────────────────┘  token cannot start an expression here.      │
 │                                                                            │
 │  .{f}{})                                                                   │
 │        ‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_090.md:2:7 ┘

    Expressions can be names, literals, tags, records, lists, tuples, lambdas,
    blocks, conditionals, matches, or function calls.

    For example:
        add(1, 2)

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌────────────────────────┐
│ EXPECTED CLOSING BRACE ├─ I was parsing a block expression, and I ──────────┐
└┬───────────────────────┘  expected `}` before the file ended.               │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_090.md:3:1 ┘

    Close the block after its final statement or expression.

    For example:
        {
            answer = 42
            answer
        }

    I reached the end of the file before this construct was complete.


┌──────────────────────────┐
│ EXPECTED TUPLE SEPARATOR ├─ I was parsing a parenthesized expression or ────┐
└┬─────────────────────────┘  tuple, and I expected `,` or `)`.               │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_090.md:3:1 ┘

    Separate tuple elements with commas and close the tuple or parenthesized
    expression with `)`.

    For example:
        (x, y)

    I reached the end of the file before this construct was complete.


┌────────────────────────┐
│ EXPECTED CLOSING BRACE ├─ I was parsing a block expression, and I ──────────┐
└┬───────────────────────┘  expected `}` before the file ended.               │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_090.md:3:1 ┘

    Close the block after its final statement or expression.

    For example:
        {
            answer = 42
            answer
        }

    I reached the end of the file before this construct was complete.


┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  e={({\\                                                                   │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_090.md:1:1 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,NoSpaceOpenRound,OpenCurly,MultilineStringStart,StringPart,
Dot,OpenCurly,LowerIdent,CloseCurly,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "e"))
			(e-block
				(statements
					(e-malformed (reason "expected_expr_close_round_or_comma")))))))
~~~
# FORMATTED
~~~roc
e = {
	
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "e"))
		(e-block
			(e-runtime-error (tag "expr_not_canonicalized")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
