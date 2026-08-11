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
── ✗ unexpected expression syntax ──────────────────────── fuzz_crash_090.md:2:7

I was parsing an expression, and this token cannot start an expression here.

.{f}{})
      ^

Expressions can be names, literals, tags, records, lists, tuples, lambdas,
blocks, conditionals, matches, or function calls.

For example:
    add(1, 2)

I found ) here.
This closes the current construct, so the parser was looking for the missing
item before it.

── ✗ expected closing brace ────────────────────────────── fuzz_crash_090.md:3:1

I was parsing a block expression, and I expected `}` before the file ended.


^

Close the block after its final statement or expression.

For example:
    {
        answer = 42
        answer
    }

I reached the end of the file before this construct was complete.

── ✗ expected tuple separator ──────────────────────────── fuzz_crash_090.md:3:1

I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.


^

Separate tuple elements with commas and close the tuple or parenthesized
expression with ).

For example:
    (x, y)

I reached the end of the file before this construct was complete.

── ✗ expected closing brace ────────────────────────────── fuzz_crash_090.md:3:1

I was parsing a block expression, and I expected `}` before the file ended.


^

Close the block after its final statement or expression.

For example:
    {
        answer = 42
        answer
    }

I reached the end of the file before this construct was complete.

── ✗ unrecognized syntax ───────────────────────────────── fuzz_crash_090.md:1:1

I don't recognize this syntax.

e={({\\
^

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
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
