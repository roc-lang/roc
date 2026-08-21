# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!, e!]

import Stdot
		exposing [ #tem
		] # Cose

import p

import Bae as Gooe
import
	Ba
Map(a, b) : List(a), (a -> b) -> List(b)
MapML( # Cere
	a, # Anre
	b,
) # Ag
	: # Aon
		List( #rg
		),
		(a -> b) -> # row
			List(			b	) #

Foo : (Bar, Baz)

line : ( # Cpen
	Bar, #
	Baz, #m
) # Co
Some(a) : { foo : Ok(a), bar : g }
Ml(a) : { # d
	bar : Som# Afld
}

Soine(a) : { #d
	bar : Som
} #
Maya) : [ #
] #se

Func(a) : Maybe(a), a -> Maybe(a)

ane = |num| if num 2 else 5

add_one : U64 -> U64
add_ne = |num| {
	other = 1
	if num {
		dbg # bug
() #r
		0
	} else {
		dbg 123
		other
	}
}

match_time = |
	a, #rg
	b,
| # As
	match a {lue | Red => {
			x x
		}
		Blue		=> 1
		"foo" => # ent
00
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # t
			=> ment
		[1, 2 | 5, 3, .. as rest] => 123
		[
		] => 1	3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
		{ # Afpen
oo #
				: #ue
	1, #eld
ar: 2,
			..} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
	o: 1,
			} =>212
		Ok(123) => 12
	}

expect # Cord
	blah == 1 # nt

main! : (String) -> Result({}, _)
ma= |_| { # Yee
	world = "d"
	var number = 123
	expect blah == 1
	tag = Blue
	return #d
		tag  Jus
	...
	match_time(
		...
	)
nc(
		dbg # bug
2,
	)
	crash "Unrnt
	tag_ = Ok(number)
	i= "H, ${world}"
t = [
		add_one(dbg # Afist
er, # afarg
		),	456, # ee
	]
	for n in list {
	line!("Ag ${n} to ${er}")
		+ n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
	tuple = (123, "World", tag, Ok(world), (nd, tuple), [1, 2, 3])
	mle = (
		123,
		"World",ag1,
		Ok(world), # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
le =(arg1)?.od()?.ned()?.recd?
	line!(
		"Ho${ #
			r(number) # xpr
		} ",
	)
} # Cocl

y : {}
e = {}

t : V((a,c))

expect {
	f= 1
h == foo
}
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
LEADING ZERO - :0:0:0:0
UNCLOSED STRING - fuzz_crash_028.md:111:8:111:14
INCOMPLETE IMPORT - fuzz_crash_028.md:10:1:10:7
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:12:12:12:14
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:13:1:13:7
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:15:1:15:4
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:4:15:5
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:5:15:6
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:6:15:7
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:8:15:9
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:9:15:10
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:11:15:12
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:15:20:15:21
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:22:15:23
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:23:15:24
AMBIGUOUS FUNCTION TYPE - fuzz_crash_028.md:15:25:15:27
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:28:15:29
UNEXPECTED STATEMENT - fuzz_crash_028.md:15:29:15:30
AMBIGUOUS FUNCTION TYPE - fuzz_crash_028.md:15:31:15:33
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:16:1:16:6
UNEXPECTED STATEMENT - fuzz_crash_028.md:16:6:16:7
UNEXPECTED STATEMENT - fuzz_crash_028.md:17:2:17:3
UNEXPECTED STATEMENT - fuzz_crash_028.md:17:3:17:4
UNEXPECTED STATEMENT - fuzz_crash_028.md:18:2:18:3
UNEXPECTED STATEMENT - fuzz_crash_028.md:18:3:18:4
UNEXPECTED STATEMENT - fuzz_crash_028.md:19:1:19:2
UNEXPECTED STATEMENT - fuzz_crash_028.md:20:2:20:3
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:22:4:22:5
UNEXPECTED STATEMENT - fuzz_crash_028.md:23:3:23:4
UNEXPECTED STATEMENT - fuzz_crash_028.md:23:4:23:5
AMBIGUOUS FUNCTION TYPE - fuzz_crash_028.md:23:6:23:8
UNEXPECTED STATEMENT - fuzz_crash_028.md:23:9:23:10
UNEXPECTED STATEMENT - fuzz_crash_028.md:23:10:23:11
AMBIGUOUS FUNCTION TYPE - fuzz_crash_028.md:23:12:23:14
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:26:1:26:4
UNEXPECTED STATEMENT - fuzz_crash_028.md:26:5:26:6
UNEXPECTED STATEMENT - fuzz_crash_028.md:26:7:26:8
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:26:11:26:12
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:26:16:26:17
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_028.md:40:5:40:6
UNEXPECTED STATEMENT - fuzz_crash_028.md:40:7:40:8
UNEXPECTED STATEMENT - fuzz_crash_028.md:40:9:40:10
UNEXPECTED STATEMENT - fuzz_crash_028.md:41:1:41:2
UNEXPECTED STATEMENT - fuzz_crash_028.md:48:1:48:5
EXPECTED RECORD ACCESSOR - fuzz_crash_028.md:103:2:103:5
MOD NOT FOUND - fuzz_crash_028.md:6:1:8:4
UNDECLARED TYPE - fuzz_crash_028.md:29:2:29:5
UNDECLARED TYPE - fuzz_crash_028.md:30:2:30:5
UNDECLARED TYPE - fuzz_crash_028.md:32:19:32:21
UNDECLARED TYPE VARIABLE - fuzz_crash_028.md:32:32:32:33
UNDECLARED TYPE - fuzz_crash_028.md:34:8:34:11
UNDECLARED TYPE - fuzz_crash_028.md:38:8:38:11
UNDECLARED TYPE - fuzz_crash_028.md:43:11:43:16
UNDECLARED TYPE - fuzz_crash_028.md:43:26:43:31
EMPTY TUPLE NOT ALLOWED - fuzz_crash_028.md:52:1:52:3
NAME NOT IN SCOPE - fuzz_crash_028.md:65:4:65:5
NAME NOT IN SCOPE - fuzz_crash_028.md:65:6:65:7
NAME NOT IN SCOPE - fuzz_crash_028.md:71:7:71:11
UNUSED VARIABLE - fuzz_crash_028.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_028.md:72:7:72:12
UNUSED VARIABLE - fuzz_crash_028.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_028.md:77:7:77:12
NAME NOT IN SCOPE - fuzz_crash_028.md:78:37:78:40
UNUSED VARIABLE - fuzz_crash_028.md:78:21:78:27
NOT IMPLEMENTED - fuzz_crash_028.md:85:18:85:23
UNUSED VARIABLE - fuzz_crash_028.md:62:2:62:3
NAME NOT IN SCOPE - fuzz_crash_028.md:93:2:93:6
UNDECLARED TYPE - fuzz_crash_028.md:95:10:95:16
UNDECLARED TYPE - fuzz_crash_028.md:95:21:95:27
NAME NOT IN SCOPE - fuzz_crash_028.md:99:9:99:13
UNRECOGNIZED SYNTAX - fuzz_crash_028.md:103:2:103:5
NAME NOT IN SCOPE - fuzz_crash_028.md:107:1:107:3
NAME NOT IN SCOPE - fuzz_crash_028.md:116:1:116:3
NAME NOT IN SCOPE - fuzz_crash_028.md:119:11:119:15
NAME NOT IN SCOPE - fuzz_crash_028.md:120:2:120:7
NAME NOT IN SCOPE - fuzz_crash_028.md:120:22:120:24
NAME NOT IN SCOPE - fuzz_crash_028.md:123:54:123:57
NAME NOT IN SCOPE - fuzz_crash_028.md:124:42:124:44
INVALID ASSIGNMENT TO ITSELF - fuzz_crash_028.md:124:46:124:51
NAME NOT IN SCOPE - fuzz_crash_028.md:127:11:127:14
NAME NOT IN SCOPE - fuzz_crash_028.md:132:10:132:13
NAME NOT IN SCOPE - fuzz_crash_028.md:133:6:133:10
NAME NOT IN SCOPE - fuzz_crash_028.md:134:2:134:7
NAME NOT IN SCOPE - fuzz_crash_028.md:136:4:136:5
UNUSED VARIABLE - fuzz_crash_028.md:112:2:112:6
UNUSED VARIABLE - fuzz_crash_028.md:113:2:113:3
UNUSED VARIABLE - fuzz_crash_028.md:114:1:114:2
UNUSED VARIABLE - fuzz_crash_028.md:123:2:123:4
UNUSED VARIABLE - fuzz_crash_028.md:125:2:125:5
UNUSED VARIABLE - fuzz_crash_028.md:132:2:132:3
UNUSED VARIABLE - fuzz_crash_028.md:133:1:133:3
UNDECLARED TYPE - fuzz_crash_028.md:144:5:144:6
NAME NOT IN SCOPE - fuzz_crash_028.md:148:1:148:2
NAME NOT IN SCOPE - fuzz_crash_028.md:148:6:148:9
UNUSED VARIABLE - fuzz_crash_028.md:147:2:147:3
EXPOSED BUT NOT DEFINED - fuzz_crash_028.md:2:6:2:11
DECLARATION HAS NO VALUE - fuzz_crash_028.md:28:1:31:2
DECLARATION HAS NO VALUE - fuzz_crash_028.md:47:1:47:21
TYPE MISMATCH - fuzz_crash_028.md:64:2:64:2
MISSING METHOD - fuzz_crash_028.md:68:3:68:8
MISSING METHOD - fuzz_crash_028.md:70:3:70:8
TYPE MISMATCH - fuzz_crash_028.md:64:2:64:2
DECLARATION HAS NO VALUE - fuzz_crash_028.md:95:1:95:34
TOO FEW ARGS - fuzz_crash_028.md:104:2:106:3
REFERENCE HAS NO VALUE - fuzz_crash_028.md:115:3:115:10
TYPE MISMATCH - fuzz_crash_028.md:133:5:133:12
DECLARATION HAS NO VALUE - fuzz_crash_028.md:141:1:141:7
DECLARATION HAS NO VALUE - fuzz_crash_028.md:144:1:144:13
MISSING METHOD - fuzz_crash_028.md:133:5:133:12
MISSING METHOD - fuzz_crash_028.md:133:5:133:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "ASCII Control Character")
		(headline
			(reflow "ASCII control characters are not allowed in Roc source code."))
		(document))
	(report
		(severity runtime_error)
		(title "ASCII Control Character")
		(headline
			(reflow "ASCII control characters are not allowed in Roc source code."))
		(document))
	(report
		(severity runtime_error)
		(title "Leading Zero")
		(headline
			(reflow "Numbers cannot have leading zeros."))
		(document))
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 111 8) (end 111 14))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 111 8) (end 111 14) (annotation error) (line-text "\tcrash \"Unrnt"))))
	(report
		(severity runtime_error)
		(title "Incomplete Import")
		(region (start 10 1) (end 10 7))
		(headline
			(reflow "I was parsing an import, and the mod path is incomplete."))
		(document
			(reflow "Imports must name a mod, optionally with a qualifier and exposing list.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "import Json/Decode exposing [decode]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 10 1) (end 10 7) (annotation error) (line-text "import p\u{01}"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 12 12) (end 12 14))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "as")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 12 12) (end 12 14) (annotation error) (line-text "import Bae as Gooe"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 13 1) (end 13 7))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 13 1) (end 13 7) (annotation error) (line-text "import"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 15 1) (end 15 4))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "Map")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 1) (end 15 4) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 4) (end 15 5))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 4) (end 15 5) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 5) (end 15 6))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "a")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 5) (end 15 6) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 6) (end 15 7))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 6) (end 15 7) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 8) (end 15 9))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "b")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 8) (end 15 9) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 9) (end 15 10))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 9) (end 15 10) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 11) (end 15 12))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 11) (end 15 12) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 15 20) (end 15 21))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 20) (end 15 21) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 22) (end 15 23))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 22) (end 15 23) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 23) (end 15 24))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "a")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 23) (end 15 24) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Ambiguous Function Type")
		(region (start 15 25) (end 15 27))
		(headline
			(reflow "I was parsing a function type, and multiple arrows need parentheses."))
		(document
			(reflow "Use parentheses to say whether the function returns another function or takes a function as an argument.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> (b -> c)")
			(line-break)
			(indent 1)
			(text "(a -> b) -> c")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 25) (end 15 27) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 28) (end 15 29))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "b")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 28) (end 15 29) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 29) (end 15 30))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 29) (end 15 30) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Ambiguous Function Type")
		(region (start 15 31) (end 15 33))
		(headline
			(reflow "I was parsing a function type, and multiple arrows need parentheses."))
		(document
			(reflow "Use parentheses to say whether the function returns another function or takes a function as an argument.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> (b -> c)")
			(line-break)
			(indent 1)
			(text "(a -> b) -> c")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 15 31) (end 15 33) (annotation error) (line-text "Map(a, b) : List(a), (a -> b) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 16 1) (end 16 6))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "MapML")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 16 1) (end 16 6) (annotation error) (line-text "MapML( # Cere"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 16 6) (end 16 7))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 16 6) (end 16 7) (annotation error) (line-text "MapML( # Cere"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 17 2) (end 17 3))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "a")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 17 2) (end 17 3) (annotation error) (line-text "\ta, # Anre"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 17 3) (end 17 4))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 17 3) (end 17 4) (annotation error) (line-text "\ta, # Anre"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 18 2) (end 18 3))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "b")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 18 2) (end 18 3) (annotation error) (line-text "\tb,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 18 3) (end 18 4))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 18 3) (end 18 4) (annotation error) (line-text "\tb,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 19 1) (end 19 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 19 1) (end 19 2) (annotation error) (line-text ") # Ag"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 20 2) (end 20 3))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 20 2) (end 20 3) (annotation error) (line-text "\t: # Aon"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 22 4) (end 22 5))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 22 4) (end 22 5) (annotation error) (line-text "\t\t),"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 3) (end 23 4))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 23 3) (end 23 4) (annotation error) (line-text "\t\t(a -> b) -> # row"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 4) (end 23 5))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "a")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 23 4) (end 23 5) (annotation error) (line-text "\t\t(a -> b) -> # row"))))
	(report
		(severity runtime_error)
		(title "Ambiguous Function Type")
		(region (start 23 6) (end 23 8))
		(headline
			(reflow "I was parsing a function type, and multiple arrows need parentheses."))
		(document
			(reflow "Use parentheses to say whether the function returns another function or takes a function as an argument.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> (b -> c)")
			(line-break)
			(indent 1)
			(text "(a -> b) -> c")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 23 6) (end 23 8) (annotation error) (line-text "\t\t(a -> b) -> # row"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 9) (end 23 10))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "b")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 23 9) (end 23 10) (annotation error) (line-text "\t\t(a -> b) -> # row"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 23 10) (end 23 11))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 23 10) (end 23 11) (annotation error) (line-text "\t\t(a -> b) -> # row"))))
	(report
		(severity runtime_error)
		(title "Ambiguous Function Type")
		(region (start 23 12) (end 23 14))
		(headline
			(reflow "I was parsing a function type, and multiple arrows need parentheses."))
		(document
			(reflow "Use parentheses to say whether the function returns another function or takes a function as an argument.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> (b -> c)")
			(line-break)
			(indent 1)
			(text "(a -> b) -> c")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 23 12) (end 23 14) (annotation error) (line-text "\t\t(a -> b) -> # row"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 26 1) (end 26 4))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "Foo")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 26 1) (end 26 4) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 26 5) (end 26 6))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 26 5) (end 26 6) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 26 7) (end 26 8))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 26 7) (end 26 8) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 26 11) (end 26 12))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 26 11) (end 26 12) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 26 16) (end 26 17))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 26 16) (end 26 17) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 40 5) (end 40 6))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 40 5) (end 40 6) (annotation error) (line-text "Maya) : [ #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 40 7) (end 40 8))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 40 7) (end 40 8) (annotation error) (line-text "Maya) : [ #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 40 9) (end 40 10))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 40 9) (end 40 10) (annotation error) (line-text "Maya) : [ #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 41 1) (end 41 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 41 1) (end 41 2) (annotation error) (line-text "] #se"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 48 1) (end 48 5))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "add_")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 48 1) (end 48 5) (annotation error) (line-text "add_\u{12}ne = |num| {"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 103 2) (end 103 5))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "...")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 103 2) (end 103 5) (annotation error) (line-text "\t..."))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 6 1) (end 8 4))
		(headline
			(text "The mod ")
			(annotated code "Stdot")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 6 1) (end 8 4) (annotation error) (line-text "import Stdot\n\t\texposing [ #tem\n\t\t] # Cose"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 29 2) (end 29 5))
		(headline
			(reflow "The type ")
			(annotated code "Bar")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 29 2) (end 29 5) (annotation error) (line-text "\tBar, #"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 30 2) (end 30 5))
		(headline
			(reflow "The type ")
			(annotated code "Baz")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 30 2) (end 30 5) (annotation error) (line-text "\tBaz, #m"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 32 19) (end 32 21))
		(headline
			(reflow "The type ")
			(annotated code "Ok")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 32 19) (end 32 21) (annotation error) (line-text "Some(a) : { foo : Ok(a), bar : g }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 32 32) (end 32 33))
		(headline
			(reflow "The type variable ")
			(annotated code "g")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 32 32) (end 32 33) (annotation error) (line-text "Some(a) : { foo : Ok(a), bar : g }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 34 8) (end 34 11))
		(headline
			(reflow "The type ")
			(annotated code "Som")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 34 8) (end 34 11) (annotation error) (line-text "\tbar : Som# Afld"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 38 8) (end 38 11))
		(headline
			(reflow "The type ")
			(annotated code "Som")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 38 8) (end 38 11) (annotation error) (line-text "\tbar : Som"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 43 11) (end 43 16))
		(headline
			(reflow "The type ")
			(annotated code "Maybe")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 43 11) (end 43 16) (annotation error) (line-text "Func(a) : Maybe(a), a -> Maybe(a)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 43 26) (end 43 31))
		(headline
			(reflow "The type ")
			(annotated code "Maybe")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 43 26) (end 43 31) (annotation error) (line-text "Func(a) : Maybe(a), a -> Maybe(a)"))))
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 52 1) (end 52 3))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 52 1) (end 52 3) (annotation error) (line-text "() #r"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 65 4) (end 65 5))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "x")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 65 4) (end 65 5) (annotation error) (line-text "\t\t\tx x"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 65 6) (end 65 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "x")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 65 6) (end 65 7) (annotation error) (line-text "\t\t\tx x"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 71 7) (end 71 11))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ment")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 71 7) (end 71 11) (annotation error) (line-text "\t\t\t=> ment"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 1 1) (end 1 1) (annotation error) (line-text "# Thnt!"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 72 7) (end 72 12))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 72 7) (end 72 12) (annotation error) (line-text "\t\t[1, 2 | 5, 3, .. as rest] => 123"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 1 1) (end 1 1) (annotation error) (line-text "# Thnt!"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 77 7) (end 77 12))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 77 7) (end 77 12) (annotation error) (line-text "\t\t(1, 2 | 5, 3) => 123"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 78 37) (end 78 40))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "add")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 78 37) (end 78 40) (annotation error) (line-text "\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 78 21) (end 78 27))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 78 21) (end 78 27) (annotation error) (line-text "\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 85 18) (end 85 23))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 85 18) (end 85 23) (annotation error) (line-text "\t\t{ foo: 1, bar: 2 | 7 } => 12"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 62 2) (end 62 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 62 2) (end 62 3) (annotation error) (line-text "\tb,"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 93 2) (end 93 6))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "blah")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 93 2) (end 93 6) (annotation error) (line-text "\tblah == 1 # nt"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 95 10) (end 95 16))
		(headline
			(reflow "The type ")
			(annotated code "String")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 95 10) (end 95 16) (annotation error) (line-text "main! : (String) -> Result({}, _)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 95 21) (end 95 27))
		(headline
			(reflow "The type ")
			(annotated code "Result")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 95 21) (end 95 27) (annotation error) (line-text "main! : (String) -> Result({}, _)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 99 9) (end 99 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "blah")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 99 9) (end 99 13) (annotation error) (line-text "\texpect blah == 1"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 103 2) (end 103 5))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 103 2) (end 103 5) (annotation error) (line-text "\t..."))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 107 1) (end 107 3))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "nc")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 107 1) (end 107 3) (annotation error) (line-text "nc("))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 116 1) (end 116 3))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "er")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 116 1) (end 116 3) (annotation error) (line-text "er, # afarg"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 119 11) (end 119 15))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "list")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 119 11) (end 119 15) (annotation error) (line-text "\tfor n in list {"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 120 2) (end 120 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 120 2) (end 120 7) (annotation error) (line-text "\tline!(\"Ag ${n} to ${er}\")"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 120 22) (end 120 24))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "er")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 120 22) (end 120 24) (annotation error) (line-text "\tline!(\"Ag ${n} to ${er}\")"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 123 54) (end 123 57))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ned")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 123 54) (end 123 57) (annotation error) (line-text "\trd = { foo: 123, bar: \"H\", baz: tag, qux: Ok(world),ned }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 124 42) (end 124 44))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "nd")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 124 42) (end 124 44) (annotation error) (line-text "\ttuple = (123, \"World\", tag, Ok(world), (nd, tuple), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Invalid Assignment To Itself")
		(region (start 124 46) (end 124 51))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "tuple")
			(reflow " is assigned to itself, which would cause an infinite loop at runtime."))
		(document
			(reflow "Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 124 46) (end 124 51) (annotation error) (line-text "\ttuple = (123, \"World\", tag, Ok(world), (nd, tuple), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 127 11) (end 127 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ag1")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 127 11) (end 127 14) (annotation error) (line-text "\t\t\"World\",ag1,"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 132 10) (end 132 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "foo")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 132 10) (end 132 13) (annotation error) (line-text "\tb = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 133 6) (end 133 10))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "arg1")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 133 6) (end 133 10) (annotation error) (line-text "le =(arg1)?.od()?.ned()?.recd?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 134 2) (end 134 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 134 2) (end 134 7) (annotation error) (line-text "\tline!("))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 136 4) (end 136 5))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "r")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 136 4) (end 136 5) (annotation error) (line-text "\t\t\tr(number) # xpr"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 112 2) (end 112 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "tag_")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_tag_")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 112 2) (end 112 6) (annotation error) (line-text "\ttag_ = Ok(number)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 113 2) (end 113 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "i")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_i")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 113 2) (end 113 3) (annotation error) (line-text "\ti= \"H, ${world}\""))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 114 1) (end 114 2))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "t")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_t")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 114 1) (end 114 2) (annotation error) (line-text "t = ["))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 123 2) (end 123 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rd")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rd")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 123 2) (end 123 4) (annotation error) (line-text "\trd = { foo: 123, bar: \"H\", baz: tag, qux: Ok(world),ned }"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 125 2) (end 125 5))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "mle")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_mle")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 125 2) (end 125 5) (annotation error) (line-text "\tmle = ("))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 132 2) (end 132 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 132 2) (end 132 3) (annotation error) (line-text "\tb = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 133 1) (end 133 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "le")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_le")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 133 1) (end 133 3) (annotation error) (line-text "le =(arg1)?.od()?.ned()?.recd?"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 144 5) (end 144 6))
		(headline
			(reflow "The type ")
			(annotated code "V")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 144 5) (end 144 6) (annotation error) (line-text "t : V((a,c))"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 148 1) (end 148 2))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "h")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 148 1) (end 148 2) (annotation error) (line-text "h == foo"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 148 6) (end 148 9))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "foo")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 148 6) (end 148 9) (annotation error) (line-text "h == foo"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 147 2) (end 147 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "f")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_f")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_028.md") (start 147 2) (end 147 3) (annotation error) (line-text "\tf= 1"))))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 2 6) (end 2 11))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "main!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 2 6) (end 2 11) (annotation error) (line-text "app [main!] { pf: platform \"c\" }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "main!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 28 1) (end 31 2))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 28 1) (end 31 2) (annotation error) (line-text "line : ( # Cpen\n\tBar, #\n\tBaz, #m\n) # Co"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 47 1) (end 47 21))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 47 1) (end 47 21) (annotation error) (line-text "add_one : U64 -> U64"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 64 2) (end 90 3))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "lue")
			(reflow " ")
			(reflow "binding in the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "pattern of the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the same binding in the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "pattern."))
		(document
			(source-underlines
				(display (file "fuzz_crash_028.md") (start 64 2) (end 90 3) (annotation dim) (line-text "\tmatch a {lue | Red => {\n\t\t\tx x\n\t\t}\n\t\tBlue\t\t=> 1\n\t\t\"foo\" => # ent\n00\n\t\t\"foo\" | \"bar\" => 20[1, 2, 3, .. as rest] # t\n\t\t\t=> ment\n\t\t[1, 2 | 5, 3, .. as rest] => 123\n\t\t[\n\t\t] => 1\t3.14 => 314\n\t\t3.14 | 6.28 => 314\n\t\t(1, 2, 3) => 123\n\t\t(1, 2 | 5, 3) => 123\n\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)\n\t\t{ # Afpen\noo #\n\t\t\t\t: #ue\n\t1, #eld\nar: 2,\n\t\t\t..} => 12\n\t\t{ foo: 1, bar: 2 | 7 } => 12\n\t\t{\n\to: 1,\n\t\t\t} =>212\n\t\tOk(123) => 12\n\t}"))
				(underline (start 64 17) (end 64 20) (annotation error)))
			(line-break)
			(reflow "In the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "pattern,")
			(reflow " ")
			(annotated code "lue")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Red, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But in the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "pattern,")
			(reflow " ")
			(annotated code "lue")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Red, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "A name shared across")
			(reflow " ")
			(annotated code "|")
			(reflow " ")
			(reflow "patterns in the same")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "branch must have one compatible type.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 68 3) (end 68 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 68 3) (end 68 8) (annotation error) (line-text "\t\t\"foo\" => # ent"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Red, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 70 3) (end 70 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 70 3) (end 70 8) (annotation error) (line-text "\t\t\"foo\" | \"bar\" => 20[1, 2, 3, .. as rest] # t"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Red, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 64 2) (end 90 3))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "fifth")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous ones."))
		(document
			(source-underlines
				(display (file "fuzz_crash_028.md") (start 64 2) (end 90 3) (annotation dim) (line-text "\tmatch a {lue | Red => {\n\t\t\tx x\n\t\t}\n\t\tBlue\t\t=> 1\n\t\t\"foo\" => # ent\n00\n\t\t\"foo\" | \"bar\" => 20[1, 2, 3, .. as rest] # t\n\t\t\t=> ment\n\t\t[1, 2 | 5, 3, .. as rest] => 123\n\t\t[\n\t\t] => 1\t3.14 => 314\n\t\t3.14 | 6.28 => 314\n\t\t(1, 2, 3) => 123\n\t\t(1, 2 | 5, 3) => 123\n\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)\n\t\t{ # Afpen\noo #\n\t\t\t\t: #ue\n\t1, #eld\nar: 2,\n\t\t\t..} => 12\n\t\t{ foo: 1, bar: 2 | 7 } => 12\n\t\t{\n\to: 1,\n\t\t\t} =>212\n\t\tOk(123) => 12\n\t}"))
				(underline (start 70 22) (end 70 43) (annotation error)))
			(line-break)
			(reflow "This")
			(reflow " ")
			(reflow "fifth")
			(reflow " ")
			(reflow "branch is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(d)")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    d.is_eq : d, d -> Bool,")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the expression between the")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "parenthesis has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Red, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 95 1) (end 95 34))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 95 1) (end 95 34) (annotation error) (line-text "main! : (String) -> Result({}, _)"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Too Few Args")
		(region (start 104 2) (end 106 3))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "match_time")
			(reflow " function expects")
			(reflow " ")
			(reflow "2")
			(reflow " ")
			(reflow "arguments")
			(reflow ",")
			(reflow " ")
			(reflow "but it got")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "instead."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 104 2) (end 106 3) (annotation error) (line-text "\tmatch_time(\n\t\t...\n\t)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "match_time")
			(reflow " function has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Red, ..], _arg -> Error")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Are there any missing commas?")))
	(report
		(severity runtime_error)
		(title "Reference Has No Value")
		(region (start 115 3) (end 115 10))
		(headline
			(reflow "This refers to a declaration that has a type annotation but no implementation, so there is no value here to use."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 115 3) (end 115 10) (annotation error) (line-text "\t\tadd_one(dbg # Afist"))
			(line-break)
			(line-break)
			(reflow "Give that declaration a value body, or stop referring to it here.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 133 5) (end 133 12))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 133 5) (end 133 12) (annotation error) (line-text "le =(arg1)?.od()?.ned()?.recd?"))
			(line-break)
			(reflow "On error, this would return:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(ok, err)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the function body evaluates to:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The error types from all")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "operators and the function body must be compatible since any of them could be the actual return value.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 141 1) (end 141 7))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 141 1) (end 141 7) (annotation error) (line-text "y : {}"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 144 1) (end 144 13))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 144 1) (end 144 13) (annotation error) (line-text "t : V((a,c))"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 133 5) (end 133 12))
		(headline
			(reflow "This is trying to dispatch a method named")
			(reflow " ")
			(annotated code "od")
			(reflow " ")
			(reflow "on an unresolved type variable, but unresolved type variables have no methods."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 133 5) (end 133 12) (annotation error) (line-text "le =(arg1)?.od()?.ned()?.recd?"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 133 5) (end 133 18))
		(headline
			(reflow "This is trying to dispatch a method named")
			(reflow " ")
			(annotated code "ned")
			(reflow " ")
			(reflow "on an unresolved type variable, but unresolved type variables have no methods."))
		(document
			(source-region (file "fuzz_crash_028.md") (start 133 5) (end 133 18) (annotation error) (line-text "le =(arg1)?.od()?.ned()?.recd?"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
KwImport,UpperIdent,
KwExposing,OpenSquare,
CloseSquare,
KwImport,LowerIdent,
KwImport,UpperIdent,KwAs,UpperIdent,
KwImport,
UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
OpColon,
UpperIdent,NoSpaceOpenRound,
CloseRound,Comma,
OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpColon,OpenRound,
UpperIdent,Comma,
UpperIdent,Comma,
CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,CloseRound,OpColon,OpenSquare,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,Int,KwElse,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
KwIf,LowerIdent,OpenCurly,
KwDbg,
OpenRound,CloseRound,
Int,
CloseCurly,KwElse,OpenCurly,
KwDbg,Int,
LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,
LowerIdent,Comma,
LowerIdent,Comma,
OpBar,
KwMatch,LowerIdent,OpenCurly,LowerIdent,OpBar,UpperIdent,OpFatArrow,OpenCurly,
LowerIdent,LowerIdent,
CloseCurly,
UpperIdent,OpFatArrow,Int,
StringStart,StringPart,StringEnd,OpFatArrow,
Int,
StringStart,StringPart,StringEnd,OpBar,StringStart,StringPart,StringEnd,OpFatArrow,Int,OpenSquare,Int,Comma,Int,Comma,Int,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,
OpFatArrow,LowerIdent,
OpenSquare,Int,Comma,Int,OpBar,Int,Comma,Int,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,
CloseSquare,OpFatArrow,Int,Float,OpFatArrow,Int,
Float,OpBar,Float,OpFatArrow,Int,
OpenRound,Int,Comma,Int,Comma,Int,CloseRound,OpFatArrow,Int,
OpenRound,Int,Comma,Int,OpBar,Int,Comma,Int,CloseRound,OpFatArrow,Int,
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,DoubleDot,LowerIdent,CloseCurly,OpFatArrow,Int,OpArrow,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
OpenCurly,
LowerIdent,
OpColon,
Int,Comma,
LowerIdent,OpColon,Int,Comma,
DoubleDot,CloseCurly,OpFatArrow,Int,
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,OpBar,Int,CloseCurly,OpFatArrow,Int,
OpenCurly,
LowerIdent,OpColon,Int,Comma,
CloseCurly,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,OpFatArrow,Int,
CloseCurly,
KwExpect,
LowerIdent,OpEquals,Int,
LowerIdent,OpColon,OpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
KwVar,LowerIdent,OpAssign,Int,
KwExpect,LowerIdent,OpEquals,Int,
LowerIdent,OpAssign,UpperIdent,
KwReturn,
LowerIdent,UpperIdent,
TripleDot,
LowerIdent,NoSpaceOpenRound,
TripleDot,
CloseRound,
LowerIdent,NoSpaceOpenRound,
KwDbg,
Int,Comma,
CloseRound,
KwCrash,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,NoSpaceOpenRound,KwDbg,
LowerIdent,Comma,
CloseRound,Comma,Int,Comma,
CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpenRound,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,
CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,Int,OpGreaterThan,Int,OpStar,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,OpBinaryMinus,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,OpSlash,Int,
LowerIdent,OpAssign,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpQuestion,
LowerIdent,NoSpaceOpenRound,
StringStart,StringPart,OpenStringInterpolation,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseStringInterpolation,StringPart,StringEnd,Comma,
CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
KwExpect,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpEquals,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "c"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "c"))))))
	(statements
		(s-import (raw "pf.Stdout")
			(exposing
				(exposed-lower-ident
					(text "line!"))
				(exposed-lower-ident
					(text "e!"))))
		(s-import (raw "Stdot"))
		(s-malformed (tag "incomplete_import"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
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
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-type-anno (name "line")
			(ty-tuple
				(ty (name "Bar"))
				(ty (name "Baz"))))
		(s-type-decl
			(header (name "Some")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "foo")
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a"))))
				(anno-record-field (name "bar")
					(ty-var (raw "g")))))
		(s-type-decl
			(header (name "Ml")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "bar")
					(ty (name "Som")))))
		(s-type-decl
			(header (name "Soine")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "bar")
					(ty (name "Som")))))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "Func")
				(args
					(ty-var (raw "a"))))
			(ty-fn
				(ty-apply
					(ty (name "Maybe"))
					(ty-var (raw "a")))
				(ty-var (raw "a"))
				(ty-apply
					(ty (name "Maybe"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "ane"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-if-then-else
					(e-ident (raw "num"))
					(e-int (raw "2"))
					(e-int (raw "5")))))
		(s-type-anno (name "add_one")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "ne"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "other"))
							(e-int (raw "1")))
						(e-if-then-else
							(e-ident (raw "num"))
							(e-block
								(statements
									(s-dbg
										(e-tuple))
									(e-int (raw "0"))))
							(e-block
								(statements
									(s-dbg
										(e-int (raw "123")))
									(e-ident (raw "other")))))))))
		(s-decl
			(p-ident (raw "match_time"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-match
					(e-ident (raw "a"))
					(branches
						(branch
							(p-alternatives
								(p-ident (raw "lue"))
								(p-tag (raw "Red")))
							(e-block
								(statements
									(e-ident (raw "x"))
									(e-ident (raw "x")))))
						(branch
							(p-tag (raw "Blue"))
							(e-int (raw "1")))
						(branch
							(p-string (raw """)
								(p-string-text (raw "foo")))
							(e-int (raw "00")))
						(branch
							(p-alternatives
								(p-string (raw """)
									(p-string-text (raw "foo")))
								(p-string (raw """)
									(p-string-text (raw "bar"))))
							(e-int (raw "20")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3"))
								(p-list-rest (name "rest")))
							(e-ident (raw "ment")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-alternatives
									(p-int (raw "2"))
									(p-int (raw "5")))
								(p-int (raw "3"))
								(p-list-rest (name "rest")))
							(e-int (raw "123")))
						(branch
							(p-list)
							(e-int (raw "1")))
						(branch
							(p-frac (raw "3.14"))
							(e-int (raw "314")))
						(branch
							(p-alternatives
								(p-frac (raw "3.14"))
								(p-frac (raw "6.28")))
							(e-int (raw "314")))
						(branch
							(p-tuple
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3")))
							(e-int (raw "123")))
						(branch
							(p-tuple
								(p-int (raw "1"))
								(p-alternatives
									(p-int (raw "2"))
									(p-int (raw "5")))
								(p-int (raw "3")))
							(e-int (raw "123")))
						(branch
							(p-record
								(field (name "foo") (rest false)
									(p-int (raw "1")))
								(field (name "bar") (rest false)
									(p-int (raw "2")))
								(field (name "rest") (rest true)))
							(e-arrow-call
								(e-int (raw "12"))
								(e-apply
									(e-ident (raw "add"))
									(e-int (raw "34")))))
						(branch
							(p-record
								(field (name "oo") (rest false)
									(p-int (raw "1")))
								(field (name "ar") (rest false)
									(p-int (raw "2")))
								(field (rest true)))
							(e-int (raw "12")))
						(branch
							(p-record
								(field (name "foo") (rest false)
									(p-int (raw "1")))
								(field (name "bar") (rest false)
									(p-alternatives
										(p-int (raw "2"))
										(p-int (raw "7")))))
							(e-int (raw "12")))
						(branch
							(p-record
								(field (name "o") (rest false)
									(p-int (raw "1"))))
							(e-int (raw "212")))
						(branch
							(p-tag (raw "Ok")
								(p-int (raw "123")))
							(e-int (raw "12")))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "blah"))
				(e-int (raw "1"))))
		(s-type-anno (name "main!")
			(ty-fn
				(ty (name "String"))
				(ty-apply
					(ty (name "Result"))
					(ty-record)
					(_))))
		(s-decl
			(p-ident (raw "ma"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "world"))
							(e-string
								(e-string-part (raw "d"))))
						(s-var (name "number")
							(e-int (raw "123")))
						(s-expect
							(e-binop (op "==")
								(e-ident (raw "blah"))
								(e-int (raw "1"))))
						(s-decl
							(p-ident (raw "tag"))
							(e-tag (raw "Blue")))
						(s-return
							(e-ident (raw "tag")))
						(e-malformed (reason "expr_dot_suffix_not_allowed"))
						(e-apply
							(e-ident (raw "match_time"))
							(e-ellipsis))
						(e-apply
							(e-ident (raw "nc"))
							(e-dbg
								(e-int (raw "2"))))
						(s-crash
							(e-string
								(e-string-part (raw "Unrnt"))))
						(s-decl
							(p-ident (raw "tag_"))
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "number"))))
						(s-decl
							(p-ident (raw "i"))
							(e-string
								(e-string-part (raw "H, "))
								(e-ident (raw "world"))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "t"))
							(e-list
								(e-apply
									(e-ident (raw "add_one"))
									(e-dbg
										(e-ident (raw "er"))))
								(e-int (raw "456"))))
						(s-for
							(p-ident (raw "n"))
							(e-ident (raw "list"))
							(e-block
								(statements
									(e-binop (op "+")
										(e-apply
											(e-ident (raw "line!"))
											(e-string
												(e-string-part (raw "Ag "))
												(e-ident (raw "n"))
												(e-string-part (raw " to "))
												(e-ident (raw "er"))
												(e-string-part (raw ""))))
										(e-ident (raw "n"))))))
						(s-decl
							(p-ident (raw "rd"))
							(e-record
								(field (field "foo")
									(e-int (raw "123")))
								(field (field "bar")
									(e-string
										(e-string-part (raw "H"))))
								(field (field "baz")
									(e-ident (raw "tag")))
								(field (field "qux")
									(e-apply
										(e-tag (raw "Ok"))
										(e-ident (raw "world"))))
								(field (field "ned"))))
						(s-decl
							(p-ident (raw "tuple"))
							(e-tuple
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "World")))
								(e-ident (raw "tag"))
								(e-apply
									(e-tag (raw "Ok"))
									(e-ident (raw "world")))
								(e-tuple
									(e-ident (raw "nd"))
									(e-ident (raw "tuple")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(s-decl
							(p-ident (raw "mle"))
							(e-tuple
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "World")))
								(e-ident (raw "ag1"))
								(e-apply
									(e-tag (raw "Ok"))
									(e-ident (raw "world")))
								(e-tuple
									(e-ident (raw "ne"))
									(e-ident (raw "tuple")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(s-decl
							(p-ident (raw "b"))
							(e-binop (op "or")
								(e-binop (op ">")
									(e-binop (op "??")
										(e-apply
											(e-tag (raw "Err"))
											(e-ident (raw "foo")))
										(e-int (raw "12")))
									(e-binop (op "*")
										(e-int (raw "5"))
										(e-int (raw "5"))))
								(e-binop (op "or")
									(e-binop (op "and")
										(e-binop (op "<")
											(e-binop (op "+")
												(e-int (raw "13"))
												(e-int (raw "2")))
											(e-int (raw "5")))
										(e-binop (op ">=")
											(e-binop (op "-")
												(e-int (raw "10"))
												(e-int (raw "1")))
											(e-int (raw "16"))))
									(e-binop (op "<=")
										(e-int (raw "12"))
										(e-binop (op "/")
											(e-int (raw "3"))
											(e-int (raw "5")))))))
						(s-decl
							(p-ident (raw "le"))
							(e-question-suffix
								(e-field-access
									(receiver
										(e-question-suffix
											(e-method-call (method ".ned")
												(receiver
													(e-question-suffix
														(e-method-call (method ".od")
															(receiver
																(e-question-suffix
																	(e-tuple
																		(e-ident (raw "arg1")))))
															(args))))
												(args))))
									(segment (mode "required") (field "recd")))))
						(e-apply
							(e-ident (raw "line!"))
							(e-string
								(e-string-part (raw "Ho"))
								(e-apply
									(e-ident (raw "r"))
									(e-ident (raw "number")))
								(e-string-part (raw " "))))))))
		(s-type-anno (name "y")
			(ty-record))
		(s-decl
			(p-ident (raw "e"))
			(e-record))
		(s-type-anno (name "t")
			(ty-apply
				(ty (name "V"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "c")))))
		(s-expect
			(e-block
				(statements
					(s-decl
						(p-ident (raw "f"))
						(e-int (raw "1")))
					(e-binop (op "==")
						(e-ident (raw "h"))
						(e-ident (raw "foo"))))))))
~~~
# FORMATTED
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!, e!]

import Stdot # Cose




# Cere
# Anre

# Ag

#



line : ( # Cpen
	Bar, #
	Baz, # m
) # Co

Some(a) : { foo : Ok(a), bar : g }

Ml(a) : { # d
	bar : Som, # Afld
}

Soine(a) : { # d
	bar : Som,
}
#
# se

Func(a) : Maybe(a), a -> Maybe(a)

ane = |num| if num 2 else 5

add_one : U64 -> U64
ne = |num| {
	other = 1
	if num {
		dbg # bug
			() # r
		0
	} else {
		dbg 123
		other
	}
}

match_time = |
	a, # rg
	b,
| # As
	match a {
		lue | Red => {
			x
			x
		}
		Blue => 1
		"foo" => # ent
			00
		"foo" | "bar" => 20
		[1, 2, 3, .. as rest] # t
			=> ment
		[1, 2 | 5, 3, .. as rest] => 123
		[] => 1
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12 |> add(34)
		{ # Afpen
			oo #
				: # ue
					1, # eld
			ar: 2,
			..,
		} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
			o: 1,
		} => 212
		Ok(123) => 12
	}

expect # Cord
	blah == 1 # nt

main! : (String) -> Result({}, _)

ma = |_| { # Yee
	world = "d"
	var number = 123
	expect blah == 1
	tag = Blue
	return # d
		tag
	
	match_time(...)
	nc(
		dbg # bug
			2,
	)
	crash "Unrnt"
	tag_ = Ok(number)
	i = "H, ${world}"
	t = [
		add_one(
			dbg # Afist
				er, # afarg
		),
		456, # ee
	]
	for n in list {
		line!("Ag ${n} to ${er}")
			+ n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world), ned }
	tuple = (123, "World", tag, Ok(world), (nd, tuple), [1, 2, 3])
	mle = (
		123,
		"World",
		ag1,
		Ok(world), # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
	le = (arg1)?.od()?.ned()?.recd?
	line!(
		"Ho${ #
			r(number) # xpr
		} ",
	)
} # Cocl

y : {}

e = {}

t : V((a, c))

expect {
	f = 1
	h == foo
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "line"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-tuple
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "ane"))
		(e-lambda
			(args
				(p-assign (ident "num")))
			(e-if
				(if-branches
					(if-branch
						(e-lookup-local
							(p-assign (ident "num")))
						(e-num (value "2"))))
				(if-else
					(e-num (value "5"))))))
	(d-let
		(p-assign (ident "add_one"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "ne"))
		(e-lambda
			(args
				(p-assign (ident "num")))
			(e-block
				(s-let
					(p-assign (ident "other"))
					(e-num (value "1")))
				(e-if
					(if-branches
						(if-branch
							(e-lookup-local
								(p-assign (ident "num")))
							(e-block
								(s-dbg
									(e-runtime-error (tag "empty_tuple")))
								(e-num (value "0")))))
					(if-else
						(e-block
							(s-dbg
								(e-num (value "123")))
							(e-lookup-local
								(p-assign (ident "other")))))))))
	(d-let
		(p-assign (ident "match_time"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-malformed))
				(ty-malformed))))
	(d-let
		(p-assign (ident "ma"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "y"))
		(e-anno-only)
		(annotation
			(ty-record)))
	(d-let
		(p-assign (ident "e"))
		(e-empty_record))
	(d-let
		(p-assign (ident "t"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(s-import (mod "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "e!") (wildcard false))))
	(s-import (mod "Stdot")
		(exposes))
	(s-alias-decl
		(ty-header (name "Some")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Ml")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Soine")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Func")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-fn (effectful false)
			(ty-malformed)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-malformed)))
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Error, Error)"))
		(patt (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "U64 -> U64"))
		(patt (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "[Blue, Red, ..], _arg -> Error"))
		(patt (type "Error -> Error"))
		(patt (type "_arg -> Error"))
		(patt (type "{}"))
		(patt (type "{}"))
		(patt (type "Error")))
	(type_decls
		(alias (type "Error")
			(ty-header (name "Some")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "Ml")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "Soine")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "Func")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "(Error, Error)"))
		(expr (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "U64 -> U64"))
		(expr (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "[Blue, Red, ..], _arg -> Error"))
		(expr (type "Error -> Error"))
		(expr (type "_arg -> Error"))
		(expr (type "{}"))
		(expr (type "{}"))
		(expr (type "Error"))))
~~~
