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
WHERE CLAUSE ERROR - everything.md:13:2:13:7
PARSE ERROR - everything.md:14:3:14:9
PARSE ERROR - everything.md:14:9:14:10
PARSE ERROR - everything.md:14:10:14:11
PARSE ERROR - everything.md:14:11:14:12
PARSE ERROR - everything.md:14:12:14:15
PARSE ERROR - everything.md:14:16:14:17
PARSE ERROR - everything.md:14:18:14:19
PARSE ERROR - everything.md:15:4:15:5
PARSE ERROR - everything.md:15:5:15:6
PARSE ERROR - everything.md:16:4:16:5
PARSE ERROR - everything.md:17:3:17:4
PARSE ERROR - everything.md:17:5:17:7
PARSE ERROR - everything.md:17:11:17:12
PARSE ERROR - everything.md:18:3:18:9
PARSE ERROR - everything.md:18:9:18:10
PARSE ERROR - everything.md:18:10:18:11
PARSE ERROR - everything.md:18:11:18:12
PARSE ERROR - everything.md:18:12:18:15
PARSE ERROR - everything.md:18:16:18:17
PARSE ERROR - everything.md:18:18:18:19
PARSE ERROR - everything.md:19:4:19:5
PARSE ERROR - everything.md:19:5:19:6
PARSE ERROR - everything.md:20:4:20:5
PARSE ERROR - everything.md:21:3:21:4
PARSE ERROR - everything.md:21:5:21:7
PARSE ERROR - everything.md:22:1:22:2
PARSE ERROR - everything.md:22:2:22:3
PARSE ERROR - everything.md:22:3:22:4
PARSE ERROR - everything.md:22:4:22:5
PARSE ERROR - everything.md:22:6:22:7
PARSE ERROR - everything.md:22:8:22:9
PARSE ERROR - everything.md:23:2:23:7
PARSE ERROR - everything.md:24:3:24:9
PARSE ERROR - everything.md:24:9:24:10
PARSE ERROR - everything.md:24:10:24:11
PARSE ERROR - everything.md:24:11:24:12
PARSE ERROR - everything.md:24:12:24:15
PARSE ERROR - everything.md:24:16:24:17
PARSE ERROR - everything.md:24:18:24:19
PARSE ERROR - everything.md:25:4:25:5
PARSE ERROR - everything.md:25:5:25:6
PARSE ERROR - everything.md:26:4:26:5
PARSE ERROR - everything.md:27:3:27:4
PARSE ERROR - everything.md:27:5:27:7
PARSE ERROR - everything.md:27:11:27:12
PARSE ERROR - everything.md:28:3:28:9
PARSE ERROR - everything.md:28:9:28:10
PARSE ERROR - everything.md:28:10:28:11
PARSE ERROR - everything.md:28:11:28:12
PARSE ERROR - everything.md:28:12:28:15
PARSE ERROR - everything.md:28:16:28:17
PARSE ERROR - everything.md:28:18:28:19
PARSE ERROR - everything.md:29:4:29:5
PARSE ERROR - everything.md:29:5:29:6
PARSE ERROR - everything.md:30:4:30:5
PARSE ERROR - everything.md:31:3:31:4
PARSE ERROR - everything.md:31:5:31:7
PARSE ERROR - everything.md:33:1:33:2
PARSE ERROR - everything.md:33:2:33:3
PARSE ERROR - everything.md:34:2:34:3
PARSE ERROR - everything.md:34:3:34:4
PARSE ERROR - everything.md:35:2:35:3
PARSE ERROR - everything.md:36:1:36:2
PARSE ERROR - everything.md:36:3:36:4
PARSE ERROR - everything.md:36:5:36:6
PARSE ERROR - everything.md:37:2:37:3
PARSE ERROR - everything.md:37:3:37:4
PARSE ERROR - everything.md:38:2:38:3
PARSE ERROR - everything.md:39:1:39:2
WHERE CLAUSE ERROR - everything.md:56:12:56:17
PARSE ERROR - everything.md:56:18:56:24
PARSE ERROR - everything.md:56:24:56:25
PARSE ERROR - everything.md:56:25:56:26
PARSE ERROR - everything.md:56:26:56:27
PARSE ERROR - everything.md:56:27:56:29
PARSE ERROR - everything.md:56:29:56:30
PARSE ERROR - everything.md:56:31:56:37
PARSE ERROR - everything.md:56:37:56:38
PARSE ERROR - everything.md:56:38:56:39
PARSE ERROR - everything.md:56:39:56:40
PARSE ERROR - everything.md:56:40:56:42
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:12:1:13:7
UNDECLARED TYPE - everything.md:43:5:43:6
MODULE NOT FOUND - everything.md:2:1:5:2
MODULE NOT FOUND - everything.md:6:1:9:2
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
# PROBLEMS
**WHERE CLAUSE ERROR**
Expected an opening bracket **[** after `where`.
Where clauses should look like:     where [a.method : Type]

**everything.md:13:2:13:7:**
```roc
	where
```
	^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:14:3:14:9:**
```roc
		module(a).a1 : (
```
		^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:14:9:14:10:**
```roc
		module(a).a1 : (
```
		      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:14:10:14:11:**
```roc
		module(a).a1 : (
```
		       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:14:11:14:12:**
```roc
		module(a).a1 : (
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:14:12:14:15:**
```roc
		module(a).a1 : (
```
		         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:14:16:14:17:**
```roc
		module(a).a1 : (
```
		             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:14:18:14:19:**
```roc
		module(a).a1 : (
```
		               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:15:4:15:5:**
```roc
			a,
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:15:5:15:6:**
```roc
			a,
```
			 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:16:4:16:5:**
```roc
			a
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:17:3:17:4:**
```roc
		) -> Str,
```
		^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**everything.md:17:5:17:7:**
```roc
		) -> Str,
```
		  ^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**everything.md:17:11:17:12:**
```roc
		) -> Str,
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:18:3:18:9:**
```roc
		module(a).a2 : (
```
		^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:18:9:18:10:**
```roc
		module(a).a2 : (
```
		      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:18:10:18:11:**
```roc
		module(a).a2 : (
```
		       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:18:11:18:12:**
```roc
		module(a).a2 : (
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:18:12:18:15:**
```roc
		module(a).a2 : (
```
		         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:18:16:18:17:**
```roc
		module(a).a2 : (
```
		             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:18:18:18:19:**
```roc
		module(a).a2 : (
```
		               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:19:4:19:5:**
```roc
			a,
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:19:5:19:6:**
```roc
			a,
```
			 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:20:4:20:5:**
```roc
			a
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:21:3:21:4:**
```roc
		) -> Str
```
		^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**everything.md:21:5:21:7:**
```roc
		) -> Str
```
		  ^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**everything.md:22:1:22:2:**
```roc
B(b) : b
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:22:2:22:3:**
```roc
B(b) : b
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:22:3:22:4:**
```roc
B(b) : b
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:22:4:22:5:**
```roc
B(b) : b
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:22:6:22:7:**
```roc
B(b) : b
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:22:8:22:9:**
```roc
B(b) : b
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:23:2:23:7:**
```roc
	where
```
	^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:24:3:24:9:**
```roc
		module(b).b1 : (
```
		^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:24:9:24:10:**
```roc
		module(b).b1 : (
```
		      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:24:10:24:11:**
```roc
		module(b).b1 : (
```
		       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:24:11:24:12:**
```roc
		module(b).b1 : (
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:24:12:24:15:**
```roc
		module(b).b1 : (
```
		         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:24:16:24:17:**
```roc
		module(b).b1 : (
```
		             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:24:18:24:19:**
```roc
		module(b).b1 : (
```
		               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:25:4:25:5:**
```roc
			b,
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:25:5:25:6:**
```roc
			b,
```
			 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:26:4:26:5:**
```roc
			b
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:27:3:27:4:**
```roc
		) -> Str,
```
		^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**everything.md:27:5:27:7:**
```roc
		) -> Str,
```
		  ^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**everything.md:27:11:27:12:**
```roc
		) -> Str,
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:28:3:28:9:**
```roc
		module(b).b2 : (
```
		^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:28:9:28:10:**
```roc
		module(b).b2 : (
```
		      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:28:10:28:11:**
```roc
		module(b).b2 : (
```
		       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:28:11:28:12:**
```roc
		module(b).b2 : (
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:28:12:28:15:**
```roc
		module(b).b2 : (
```
		         ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:28:16:28:17:**
```roc
		module(b).b2 : (
```
		             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:28:18:28:19:**
```roc
		module(b).b2 : (
```
		               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:29:4:29:5:**
```roc
			b,
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:29:5:29:6:**
```roc
			b,
```
			 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:30:4:30:5:**
```roc
			b
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:31:3:31:4:**
```roc
		) -> Str
```
		^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**everything.md:31:5:31:7:**
```roc
		) -> Str
```
		  ^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**everything.md:33:1:33:2:**
```roc
C(
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:33:2:33:3:**
```roc
C(
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:34:2:34:3:**
```roc
	a,
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:34:3:34:4:**
```roc
	a,
```
	 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:35:2:35:3:**
```roc
	b
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:36:1:36:2:**
```roc
) : (
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:36:3:36:4:**
```roc
) : (
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:36:5:36:6:**
```roc
) : (
```
    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:37:2:37:3:**
```roc
	a,
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:37:3:37:4:**
```roc
	a,
```
	 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:38:2:38:3:**
```roc
	b
```
	^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:39:1:39:2:**
```roc
)
```
^


**WHERE CLAUSE ERROR**
Expected an opening bracket **[** after `where`.
Where clauses should look like:     where [a.method : Type]

**everything.md:56:12:56:17:**
```roc
g : e -> e where module(e).A, module(e).B
```
           ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:18:56:24:**
```roc
g : e -> e where module(e).A, module(e).B
```
                 ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:24:56:25:**
```roc
g : e -> e where module(e).A, module(e).B
```
                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:25:56:26:**
```roc
g : e -> e where module(e).A, module(e).B
```
                        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:26:56:27:**
```roc
g : e -> e where module(e).A, module(e).B
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:27:56:29:**
```roc
g : e -> e where module(e).A, module(e).B
```
                          ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:29:56:30:**
```roc
g : e -> e where module(e).A, module(e).B
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:31:56:37:**
```roc
g : e -> e where module(e).A, module(e).B
```
                              ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:37:56:38:**
```roc
g : e -> e where module(e).A, module(e).B
```
                                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:38:56:39:**
```roc
g : e -> e where module(e).A, module(e).B
```
                                     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:39:56:40:**
```roc
g : e -> e where module(e).A, module(e).B
```
                                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**everything.md:56:40:56:42:**
```roc
g : e -> e where module(e).A, module(e).B
```
                                       ^^


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:12:1:13:7:**
```roc
A(a) : a
	where
```


**UNDECLARED TYPE**
The type _C_ is not declared in this scope.

This type is referenced here:
**everything.md:43:5:43:6:**
```roc
) : C(
```
    ^


**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:2:1:5:2:**
```roc
import I1 exposing [
	I11,
	I12
]
```


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:6:1:9:2:**
```roc
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2
]
```


**MALFORMED WHERE CLAUSE**
This where clause could not be parsed correctly.

**everything.md:56:12:56:17:**
```roc
g : e -> e where module(e).A, module(e).B
```
           ^^^^^

Check the syntax of your where clause.

**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:88:5:88:6:**
```roc
				b
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:93:4:93:5:**
```roc
			b
```
			^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:98:5:98:6:**
```roc
				b
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:104:5:104:6:**
```roc
				b
```
				^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:59:2:59:4:**
```roc
	h1 = {
```
	^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:67:2:67:4:**
```roc
	h2 = h(
```
	^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:71:2:71:4:**
```roc
	h3 = A(
```
	^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:75:2:75:4:**
```roc
	h4 = [
```
	^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:79:2:79:4:**
```roc
	h5 = (
```
	^^


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
		(p-assign (ident "h"))
		(e-closure
			(captures
				(capture (ident "a"))
				(capture (ident "a"))
				(capture (ident "a"))
				(capture (ident "a"))
				(capture (ident "h")))
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
						(e-call
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
											(p-assign (ident "a"))))))))))))
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
				(ty-lookup (name "Str") (external-module "Str")))
			(field (field "b")
				(ty-lookup (name "Str") (external-module "Str")))))
	(s-alias-decl
		(ty-header (name "F"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))))
	(s-import (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno (name "g")
		(ty-fn (effectful false)
			(ty-rigid-var (name "e"))
			(ty-rigid-var-lookup (ty-rigid-var (name "e"))))
		(where
			(malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c")))
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
		(expr (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c"))))
~~~
