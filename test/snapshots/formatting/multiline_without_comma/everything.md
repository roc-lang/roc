# META
~~~ini
description=Multiline without comma formatting everything
type=snippet
~~~
# SOURCE
~~~roc
# Import exposing
import I1 exposing [
	I11,
	I12
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2
]

# Where constraint
A(a) : a
	where
		module(a).a1 : (
			a,
			a
		) -> Str,
		module(a).a2 : (
			a,
			a
		) -> Str
B(b) : b
	where
		module(b).b1 : (
			b,
			b
		) -> Str,
		module(b).b2 : (
			b,
			b
		) -> Str

C(
	a,
	b
) : (
	a,
	b
)
D(
	a,
	b
) : C(
	a,
	b
)
E : {
	a : Str,
	b : Str
}
F : [
	A,
	B
]

g : e -> e where module(e).A, module(e).B

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y
		}
	}
	h2 = h(
		x,
		y
	)
	h3 = A(
		x,
		y
	)
	h4 = [
		x,
		y
	]
	h5 = (
		x,
		y
	)

	match x {
		Z1(
			(
				a,
				b
			)
		) => a
		Z2(
			a,
			b
		) => a
		Z3(
			{
				a,
				b
			}
		) => a
		Z4(
			[
				a,
				b
			]
		) => a
	}
}
~~~
# EXPECTED
EXPECTED WHERE CLAUSE LIST - everything.md:13:2:13:7
UNEXPECTED STATEMENT - everything.md:14:3:14:9
UNEXPECTED STATEMENT - everything.md:14:9:14:10
UNEXPECTED STATEMENT - everything.md:14:10:14:11
UNEXPECTED STATEMENT - everything.md:14:11:14:12
UNEXPECTED STATEMENT - everything.md:14:12:14:15
UNEXPECTED STATEMENT - everything.md:14:16:14:17
UNEXPECTED STATEMENT - everything.md:14:18:14:19
UNEXPECTED STATEMENT - everything.md:15:4:15:5
UNEXPECTED STATEMENT - everything.md:15:5:15:6
UNEXPECTED STATEMENT - everything.md:16:4:16:5
UNEXPECTED STATEMENT - everything.md:17:3:17:4
AMBIGUOUS FUNCTION TYPE - everything.md:17:5:17:7
TYPE APPLICATION NEEDS PARENTHESES - everything.md:17:11:17:12
UNEXPECTED STATEMENT - everything.md:18:3:18:9
UNEXPECTED STATEMENT - everything.md:18:9:18:10
UNEXPECTED STATEMENT - everything.md:18:10:18:11
UNEXPECTED STATEMENT - everything.md:18:11:18:12
UNEXPECTED STATEMENT - everything.md:18:12:18:15
UNEXPECTED STATEMENT - everything.md:18:16:18:17
UNEXPECTED STATEMENT - everything.md:18:18:18:19
UNEXPECTED STATEMENT - everything.md:19:4:19:5
UNEXPECTED STATEMENT - everything.md:19:5:19:6
UNEXPECTED STATEMENT - everything.md:20:4:20:5
UNEXPECTED STATEMENT - everything.md:21:3:21:4
AMBIGUOUS FUNCTION TYPE - everything.md:21:5:21:7
TYPE APPLICATION NEEDS PARENTHESES - everything.md:22:1:22:2
UNEXPECTED STATEMENT - everything.md:22:2:22:3
UNEXPECTED STATEMENT - everything.md:22:3:22:4
UNEXPECTED STATEMENT - everything.md:22:4:22:5
UNEXPECTED STATEMENT - everything.md:22:6:22:7
UNEXPECTED STATEMENT - everything.md:22:8:22:9
UNEXPECTED STATEMENT - everything.md:23:2:23:7
UNEXPECTED STATEMENT - everything.md:24:3:24:9
UNEXPECTED STATEMENT - everything.md:24:9:24:10
UNEXPECTED STATEMENT - everything.md:24:10:24:11
UNEXPECTED STATEMENT - everything.md:24:11:24:12
UNEXPECTED STATEMENT - everything.md:24:12:24:15
UNEXPECTED STATEMENT - everything.md:24:16:24:17
UNEXPECTED STATEMENT - everything.md:24:18:24:19
UNEXPECTED STATEMENT - everything.md:25:4:25:5
UNEXPECTED STATEMENT - everything.md:25:5:25:6
UNEXPECTED STATEMENT - everything.md:26:4:26:5
UNEXPECTED STATEMENT - everything.md:27:3:27:4
AMBIGUOUS FUNCTION TYPE - everything.md:27:5:27:7
TYPE APPLICATION NEEDS PARENTHESES - everything.md:27:11:27:12
UNEXPECTED STATEMENT - everything.md:28:3:28:9
UNEXPECTED STATEMENT - everything.md:28:9:28:10
UNEXPECTED STATEMENT - everything.md:28:10:28:11
UNEXPECTED STATEMENT - everything.md:28:11:28:12
UNEXPECTED STATEMENT - everything.md:28:12:28:15
UNEXPECTED STATEMENT - everything.md:28:16:28:17
UNEXPECTED STATEMENT - everything.md:28:18:28:19
UNEXPECTED STATEMENT - everything.md:29:4:29:5
UNEXPECTED STATEMENT - everything.md:29:5:29:6
UNEXPECTED STATEMENT - everything.md:30:4:30:5
UNEXPECTED STATEMENT - everything.md:31:3:31:4
AMBIGUOUS FUNCTION TYPE - everything.md:31:5:31:7
TYPE APPLICATION NEEDS PARENTHESES - everything.md:33:1:33:2
UNEXPECTED STATEMENT - everything.md:33:2:33:3
UNEXPECTED STATEMENT - everything.md:34:2:34:3
UNEXPECTED STATEMENT - everything.md:34:3:34:4
UNEXPECTED STATEMENT - everything.md:35:2:35:3
UNEXPECTED STATEMENT - everything.md:36:1:36:2
UNEXPECTED STATEMENT - everything.md:36:3:36:4
UNEXPECTED STATEMENT - everything.md:36:5:36:6
UNEXPECTED STATEMENT - everything.md:37:2:37:3
UNEXPECTED STATEMENT - everything.md:37:3:37:4
UNEXPECTED STATEMENT - everything.md:38:2:38:3
UNEXPECTED STATEMENT - everything.md:39:1:39:2
EXPECTED WHERE CLAUSE LIST - everything.md:56:12:56:17
UNEXPECTED STATEMENT - everything.md:56:18:56:24
UNEXPECTED STATEMENT - everything.md:56:24:56:25
UNEXPECTED STATEMENT - everything.md:56:25:56:26
UNEXPECTED STATEMENT - everything.md:56:26:56:27
UNEXPECTED STATEMENT - everything.md:56:27:56:29
UNEXPECTED STATEMENT - everything.md:56:29:56:30
UNEXPECTED STATEMENT - everything.md:56:31:56:37
UNEXPECTED STATEMENT - everything.md:56:37:56:38
UNEXPECTED STATEMENT - everything.md:56:38:56:39
UNEXPECTED STATEMENT - everything.md:56:39:56:40
UNEXPECTED STATEMENT - everything.md:56:40:56:42
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:12:1:13:7
UNDECLARED TYPE - everything.md:43:5:43:6
MALFORMED WHERE CLAUSE - everything.md:56:12:56:17
UNUSED VARIABLE - everything.md:88:5:88:6
UNUSED VARIABLE - everything.md:93:4:93:5
UNUSED VARIABLE - everything.md:98:5:98:6
UNUSED VARIABLE - everything.md:104:5:104:6
UNUSED VARIABLE - everything.md:59:2:59:4
UNUSED VARIABLE - everything.md:67:2:67:4
UNUSED VARIABLE - everything.md:71:2:71:4
UNUSED VARIABLE - everything.md:75:2:75:4
UNUSED VARIABLE - everything.md:79:2:79:4
DECLARATION HAS NO VALUE - everything.md:56:1:56:17
NON EXHAUSTIVE MATCH - everything.md:84:2:107:3
# PROBLEMS

┌────────────────────────────┐
│ EXPECTED WHERE CLAUSE LIST ├─ I was parsing a `where` clause, and I ────────┐
└┬───────────────────────────┘  expected `[`.                                 │
 │                                                                            │
 │  where                                                                     │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────────── everything.md:13:2 ┘

    Where constraints are written in a square-bracketed list after `where`.

    For example:
        where [a.hash : a -> U64]

    I found `where` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a1 : (                                                          │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────────── everything.md:14:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `module` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a1 : (                                                          │
 │        ‾                                                                   │
 └──────────────────────────────────────────────────────── everything.md:14:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a1 : (                                                          │
 │         ‾                                                                  │
 └─────────────────────────────────────────────────────── everything.md:14:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a1 : (                                                          │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────────── everything.md:14:11 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a1 : (                                                          │
 │           ‾‾‾                                                              │
 └─────────────────────────────────────────────────────── everything.md:14:12 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.a1` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a1 : (                                                          │
 │               ‾                                                            │
 └─────────────────────────────────────────────────────── everything.md:14:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a1 : (                                                          │
 │                 ‾                                                          │
 └─────────────────────────────────────────────────────── everything.md:14:18 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:15:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:15:5 ┘

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
 │  a                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:16:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ) -> Str,                                                                 │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:17:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌─────────────────────────┐
│ AMBIGUOUS FUNCTION TYPE ├─ I was parsing a function type, and multiple ─────┐
└┬────────────────────────┘  arrows need parentheses.                         │
 │                                                                            │
 │  ) -> Str,                                                                 │
 │    ‾‾                                                                      │
 └──────────────────────────────────────────────────────── everything.md:17:5 ┘

    Use parentheses to say whether the function returns another function or
    takes a function as an argument.

    For example:
        a -> (b -> c)
        (a -> b) -> c


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  ) -> Str,                                                                 │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────────── everything.md:17:11 ┘

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
 │  module(a).a2 : (                                                          │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────────── everything.md:18:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `module` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a2 : (                                                          │
 │        ‾                                                                   │
 └──────────────────────────────────────────────────────── everything.md:18:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a2 : (                                                          │
 │         ‾                                                                  │
 └─────────────────────────────────────────────────────── everything.md:18:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a2 : (                                                          │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────────── everything.md:18:11 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a2 : (                                                          │
 │           ‾‾‾                                                              │
 └─────────────────────────────────────────────────────── everything.md:18:12 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.a2` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a2 : (                                                          │
 │               ‾                                                            │
 └─────────────────────────────────────────────────────── everything.md:18:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(a).a2 : (                                                          │
 │                 ‾                                                          │
 └─────────────────────────────────────────────────────── everything.md:18:18 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:19:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:19:5 ┘

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
 │  a                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:20:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ) -> Str                                                                  │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:21:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌─────────────────────────┐
│ AMBIGUOUS FUNCTION TYPE ├─ I was parsing a function type, and multiple ─────┐
└┬────────────────────────┘  arrows need parentheses.                         │
 │                                                                            │
 │  ) -> Str                                                                  │
 │    ‾‾                                                                      │
 └──────────────────────────────────────────────────────── everything.md:21:5 ┘

    Use parentheses to say whether the function returns another function or
    takes a function as an argument.

    For example:
        a -> (b -> c)
        (a -> b) -> c


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  B(b) : b                                                                  │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:22:1 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `B` here.
    Names that start with uppercase letters are used for tags, type names, and
    module names in Roc.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  B(b) : b                                                                  │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:22:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  B(b) : b                                                                  │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────────── everything.md:22:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  B(b) : b                                                                  │
 │     ‾                                                                      │
 └──────────────────────────────────────────────────────── everything.md:22:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  B(b) : b                                                                  │
 │       ‾                                                                    │
 └──────────────────────────────────────────────────────── everything.md:22:6 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  B(b) : b                                                                  │
 │         ‾                                                                  │
 └──────────────────────────────────────────────────────── everything.md:22:8 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  where                                                                     │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────────── everything.md:23:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `where` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b1 : (                                                          │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────────── everything.md:24:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `module` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b1 : (                                                          │
 │        ‾                                                                   │
 └──────────────────────────────────────────────────────── everything.md:24:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b1 : (                                                          │
 │         ‾                                                                  │
 └─────────────────────────────────────────────────────── everything.md:24:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b1 : (                                                          │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────────── everything.md:24:11 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b1 : (                                                          │
 │           ‾‾‾                                                              │
 └─────────────────────────────────────────────────────── everything.md:24:12 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.b1` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b1 : (                                                          │
 │               ‾                                                            │
 └─────────────────────────────────────────────────────── everything.md:24:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b1 : (                                                          │
 │                 ‾                                                          │
 └─────────────────────────────────────────────────────── everything.md:24:18 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  b,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:25:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  b,                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:25:5 ┘

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
 │  b                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:26:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ) -> Str,                                                                 │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:27:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌─────────────────────────┐
│ AMBIGUOUS FUNCTION TYPE ├─ I was parsing a function type, and multiple ─────┐
└┬────────────────────────┘  arrows need parentheses.                         │
 │                                                                            │
 │  ) -> Str,                                                                 │
 │    ‾‾                                                                      │
 └──────────────────────────────────────────────────────── everything.md:27:5 ┘

    Use parentheses to say whether the function returns another function or
    takes a function as an argument.

    For example:
        a -> (b -> c)
        (a -> b) -> c


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  ) -> Str,                                                                 │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────────── everything.md:27:11 ┘

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
 │  module(b).b2 : (                                                          │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────────── everything.md:28:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `module` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b2 : (                                                          │
 │        ‾                                                                   │
 └──────────────────────────────────────────────────────── everything.md:28:9 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b2 : (                                                          │
 │         ‾                                                                  │
 └─────────────────────────────────────────────────────── everything.md:28:10 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b2 : (                                                          │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────────── everything.md:28:11 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b2 : (                                                          │
 │           ‾‾‾                                                              │
 └─────────────────────────────────────────────────────── everything.md:28:12 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.b2` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b2 : (                                                          │
 │               ‾                                                            │
 └─────────────────────────────────────────────────────── everything.md:28:16 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  module(b).b2 : (                                                          │
 │                 ‾                                                          │
 └─────────────────────────────────────────────────────── everything.md:28:18 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  b,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:29:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  b,                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:29:5 ┘

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
 │  b                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:30:4 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ) -> Str                                                                  │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:31:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌─────────────────────────┐
│ AMBIGUOUS FUNCTION TYPE ├─ I was parsing a function type, and multiple ─────┐
└┬────────────────────────┘  arrows need parentheses.                         │
 │                                                                            │
 │  ) -> Str                                                                  │
 │    ‾‾                                                                      │
 └──────────────────────────────────────────────────────── everything.md:31:5 ┘

    Use parentheses to say whether the function returns another function or
    takes a function as an argument.

    For example:
        a -> (b -> c)
        (a -> b) -> c


┌────────────────────────────────────┐
│ TYPE APPLICATION NEEDS PARENTHESES ├─ I was parsing a type annotation, ─────┐
└┬───────────────────────────────────┘  and I found a type argument without   │
 │                                      parentheses.                          │
 │                                                                            │
 │  C(                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:33:1 ┘

    Roc type applications use parentheses around their arguments. Write
    `List(U8)`, not `List U8`.

    For example:
        List(U8)

    I found `C` here.
    Names that start with uppercase letters are used for tags, type names, and
    module names in Roc.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  C(                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:33:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:34:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:34:3 ┘

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
 │  b                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:35:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ) : (                                                                     │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:36:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ) : (                                                                     │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────────── everything.md:36:3 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `:` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  ) : (                                                                     │
 │      ‾                                                                     │
 └──────────────────────────────────────────────────────── everything.md:36:5 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:37:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `a` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  a,                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:37:3 ┘

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
 │  b                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:38:2 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `b` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  )                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:39:1 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌────────────────────────────┐
│ EXPECTED WHERE CLAUSE LIST ├─ I was parsing a `where` clause, and I ────────┐
└┬───────────────────────────┘  expected `[`.                                 │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │             ‾‾‾‾‾                                                          │
 └─────────────────────────────────────────────────────── everything.md:56:12 ┘

    Where constraints are written in a square-bracketed list after `where`.

    For example:
        where [a.hash : a -> U64]

    I found `where` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                   ‾‾‾‾‾‾                                                   │
 └─────────────────────────────────────────────────────── everything.md:56:18 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `module` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                         ‾                                                  │
 └─────────────────────────────────────────────────────── everything.md:56:24 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                          ‾                                                 │
 └─────────────────────────────────────────────────────── everything.md:56:25 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `e` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                           ‾                                                │
 └─────────────────────────────────────────────────────── everything.md:56:26 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                            ‾‾                                              │
 └─────────────────────────────────────────────────────── everything.md:56:27 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.A` here.
    Names that start with uppercase letters are used for tags, type names, and
    module names in Roc.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                              ‾                                             │
 └─────────────────────────────────────────────────────── everything.md:56:29 ┘

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
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                                ‾‾‾‾‾‾                                      │
 └─────────────────────────────────────────────────────── everything.md:56:31 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `module` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                                      ‾                                     │
 └─────────────────────────────────────────────────────── everything.md:56:37 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                                       ‾                                    │
 └─────────────────────────────────────────────────────── everything.md:56:38 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `e` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                                        ‾                                   │
 └─────────────────────────────────────────────────────── everything.md:56:39 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │                                         ‾‾                                 │
 └─────────────────────────────────────────────────────── everything.md:56:40 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `.B` here.
    Names that start with uppercase letters are used for tags, type names, and
    module names in Roc.


┌──────────────────────────────────────────────┐
│ WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION ├─ You cannot define a ────────┐
└┬─────────────────────────────────────────────┘  `where` clause inside a     │
 │                                                type declaration.           │
 │                                                                            │
 │  A(a) : a                                                                  │
 │      where                                                                 │
 │                                                                            │
 └──────────────────────────────────────────────────────── everything.md:12:1 ┘

    You're attempting do this here:


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `C` is not declared in this scope. ─────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  ) : C(                                                                    │
 │      ‾                                                                     │
 └──────────────────────────────────────────────────────── everything.md:43:5 ┘



┌────────────────────────┐
│ MALFORMED WHERE CLAUSE ├─ This where clause could not be parsed correctly. ─┐
└┬───────────────────────┘                                                    │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │             ‾‾‾‾‾                                                          │
 └─────────────────────────────────────────────────────── everything.md:56:12 ┘

    Check the syntax of your where clause.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `b` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  b                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:88:5 ┘

    If you don't need this variable, prefix it with an underscore like `_b` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `b` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  b                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:93:4 ┘

    If you don't need this variable, prefix it with an underscore like `_b` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `b` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  b                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────────── everything.md:98:5 ┘

    If you don't need this variable, prefix it with an underscore like `_b` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `b` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  b                                                                         │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────────── everything.md:104:5 ┘

    If you don't need this variable, prefix it with an underscore like `_b` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `h1` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  h1 = {                                                                    │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:59:2 ┘

    If you don't need this variable, prefix it with an underscore like `_h1` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `h2` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  h2 = h(                                                                   │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:67:2 ┘

    If you don't need this variable, prefix it with an underscore like `_h2` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `h3` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  h3 = A(                                                                   │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:71:2 ┘

    If you don't need this variable, prefix it with an underscore like `_h3` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `h4` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  h4 = [                                                                    │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:75:2 ┘

    If you don't need this variable, prefix it with an underscore like `_h4` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `h5` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  h5 = (                                                                    │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────── everything.md:79:2 ┘

    If you don't need this variable, prefix it with an underscore like `_h5` to
    suppress this warning.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  g : e -> e where module(e).A, module(e).B                                 │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └──────────────────────────────────────────────────────── everything.md:56:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────┐
│ NON EXHAUSTIVE MATCH ├─ This match expression doesn't cover all possible ───┐
└┬─────────────────────┘  cases.                                              │
 │                                                                            │
 │  match x {                                                                 │
 │      Z1(                                                                   │
 │          (                                                                 │
 │              a,                                                            │
 │              b                                                             │
 │          )                                                                 │
 │      ) => a                                                                │
 │      Z2(                                                                   │
 │          a,                                                                │
 │          b                                                                 │
 │      ) => a                                                                │
 │      Z3(                                                                   │
 │          {                                                                 │
 │              a,                                                            │
 │              b                                                             │
 │          }                                                                 │
 │      ) => a                                                                │
 │      Z4(                                                                   │
 │          [                                                                 │
 │              a,                                                            │
 │              b                                                             │
 │          ]                                                                 │
 │      ) => a                                                                │
 │  }                                                                         │
 │                                                                            │
 └──────────────────────────────────────────────────────── everything.md:84:2 ┘

    The value being matched on has type:
            [Z1((c, _field)), Z2(c, _d), Z3({ a: c, b: _field }), Z4(List(c))]

    Missing patterns:
            Z4 []

    Hint: Add branches to handle these cases, or use `_` to match anything.

# TOKENS
~~~zig
KwImport,UpperIdent,KwExposing,OpenSquare,
UpperIdent,Comma,
UpperIdent,
CloseSquare,
KwImport,UpperIdent,KwExposing,OpenSquare,
UpperIdent,KwAs,UpperIdent,Comma,
UpperIdent,KwAs,UpperIdent,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,
KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,Comma,
KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,
KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,Comma,
KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpArrow,UpperIdent,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpColon,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,OpColon,OpenSquare,
UpperIdent,Comma,
UpperIdent,
CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotUpperIdent,Comma,KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
LowerIdent,OpAssign,OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,
OpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
OpenCurly,
LowerIdent,Comma,
LowerIdent,
CloseCurly,
CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,
OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "I1")
			(exposing
				(exposed-upper-ident (text "I11"))
				(exposed-upper-ident (text "I12"))))
		(s-import (raw "I2")
			(exposing
				(exposed-upper-ident (text "I21") (as "Ias1"))
				(exposed-upper-ident (text "I22") (as "Ias2"))))
		(s-type-decl
			(header (name "A")
				(args
					(ty-var (raw "a"))))
			(ty-var (raw "a")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "D")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-apply
				(ty (name "C"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "E")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "Str")))
				(anno-record-field (name "b")
					(ty (name "Str")))))
		(s-type-decl
			(header (name "F")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))
					(ty (name "B")))))
		(s-type-anno (name "g")
			(ty-fn
				(ty-var (raw "e"))
				(ty-var (raw "e")))
			(where
				(malformed (reason "where_expected_open_bracket"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "h"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "h1"))
							(e-record
								(field (field "h11")
									(e-ident (raw "x")))
								(field (field "h12")
									(e-ident (raw "x")))
								(field (field "h13")
									(e-record
										(field (field "h131")
											(e-ident (raw "x")))
										(field (field "h132")
											(e-ident (raw "y")))))))
						(s-decl
							(p-ident (raw "h2"))
							(e-apply
								(e-ident (raw "h"))
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(s-decl
							(p-ident (raw "h3"))
							(e-apply
								(e-tag (raw "A"))
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(s-decl
							(p-ident (raw "h4"))
							(e-list
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(s-decl
							(p-ident (raw "h5"))
							(e-tuple
								(e-ident (raw "x"))
								(e-ident (raw "y"))))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "Z1")
										(p-tuple
											(p-ident (raw "a"))
											(p-ident (raw "b"))))
									(e-ident (raw "a")))
								(branch
									(p-tag (raw "Z2")
										(p-ident (raw "a"))
										(p-ident (raw "b")))
									(e-ident (raw "a")))
								(branch
									(p-tag (raw "Z3")
										(p-record
											(field (name "a") (rest false))
											(field (name "b") (rest false))))
									(e-ident (raw "a")))
								(branch
									(p-tag (raw "Z4")
										(p-list
											(p-ident (raw "a"))
											(p-ident (raw "b"))))
									(e-ident (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
# Import exposing
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]

# Where constraint
A(a) : a
	where []


























D(
	a,
	b,
) : C(
	a,
	b,
)

E : {
	a : Str,
	b : Str,
}

F : [
	A,
	B,
]

g : e -> e where []


h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y,
		},
	}
	h2 = h(
		x,
		y,
	)
	h3 = A(
		x,
		y,
	)
	h4 = [
		x,
		y,
	]
	h5 = (
		x,
		y,
	)

	match x {
		Z1(
			(
				a,
				b,
			),
		) => a
		Z2(
			a,
			b,
		) => a
		Z3(
			{
				a,
				b,
			},
		) => a
		Z4(
			[
				a,
				b,
			],
		) => a
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "g"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "e"))
				(ty-rigid-var-lookup (ty-rigid-var (name "e"))))
			(where
				(malformed))))
	(d-let
		(p-assign (ident "h"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-block
				(s-let
					(p-assign (ident "h1"))
					(e-record
						(fields
							(field (name "h11")
								(e-lookup-local
									(p-assign (ident "x"))))
							(field (name "h12")
								(e-lookup-local
									(p-assign (ident "x"))))
							(field (name "h13")
								(e-record
									(fields
										(field (name "h131")
											(e-lookup-local
												(p-assign (ident "x"))))
										(field (name "h132")
											(e-lookup-local
												(p-assign (ident "y"))))))))))
				(s-let
					(p-assign (ident "h2"))
					(e-call (constraint-fn-var 147)
						(e-lookup-local
							(p-assign (ident "h")))
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))
				(s-let
					(p-assign (ident "h3"))
					(e-tag (name "A")
						(args
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))))
				(s-let
					(p-assign (ident "h4"))
					(e-list
						(elems
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))))
				(s-let
					(p-assign (ident "h5"))
					(e-tuple
						(elems
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "a")))))))))))
	(s-import (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-alias-decl
		(ty-header (name "A")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
	(s-alias-decl
		(ty-header (name "D")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "E"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "Str") (builtin)))
			(field (field "b")
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "F"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "e -> e"))
		(patt (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))], [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))] -> c")))
	(type_decls
		(alias (type "A(a)")
			(ty-header (name "A")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "D(a, b)")
			(ty-header (name "D")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "E")
			(ty-header (name "E")))
		(alias (type "F")
			(ty-header (name "F"))))
	(expressions
		(expr (type "e -> e"))
		(expr (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))], [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))] -> c"))))
~~~
