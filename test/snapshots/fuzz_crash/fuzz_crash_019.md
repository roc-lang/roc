# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!]

import Stdot
		exposing [ #tem
Cust]

import Bae as Gooe
import
	Ba
Map(a, b) : Lis, (ab) -> List(b)
MapML( # Cb,
) # Ag
	: # Aon
		List( #rg
		),
		(ab) -> # row
			List(			b	) #z)

line : ( # Cm
) # Co
Som : { foo : O, bar : g }
Ml(a) : { # ld
}

Soine(a) : { #
} #
Maybe(a) : [Somne]

Mayine(a) : [ #
] #)

ane = |num| if num 2 else 5

one : U6
add = |num| {
	1
	if num {
		dbg # bug
			s exp0
	} else {
		dbg 123
		r
	}
}

me = |
	a, Tb,
| # As
	match a {lue  {
	x
		}
		Blue=> {x
			}
	er #ent
			1	"for" => 20[1, ] # t
		ment
		[1, 2, 3,est]123
		[
		] 23
		3.1 314
		3.14 | 6.28 => 314
		(1, ) => 123
		(1, 2, 3)123
		{ 	} => 12
		Ok(123) => 12
	}

expect # Cord
	nt

main! : Listlt({}, _)
ma= |_| { e
	w = "d"
	var er = 123
	expect blaue
	return #d
		tag

	#
	...
	me(
		..., # r
	)crash ke"Unr!" #)
	i= "H, ${d}"
t = [
		one(er, 		),	456, # two
9, #ee
	]
	for n in list {
	line!("Ag ${n} to ${er}")
		ber + n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
	m (
		123,
		"World",ag1,
		O, # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
	Stdo!(
		"Ho${ #
			r(nu) # xpr
		} ",
	)
} # Cocl

y : {}
e = {}

t : V((a,c))

expect {
	foo == 1
h == foo
}
~~~
# EXPECTED
MISSING MATCH ARROW - fuzz_crash_019.md:52:16:52:16
MISSING MATCH ARROW - fuzz_crash_019.md:58:4:58:4
MISSING MATCH ARROW - fuzz_crash_019.md:59:3:59:3
MISSING MATCH ARROW - fuzz_crash_019.md:60:16:60:16
MISSING MATCH ARROW - fuzz_crash_019.md:62:5:62:5
MISSING MATCH ARROW - fuzz_crash_019.md:63:7:63:7
MISSING MATCH ARROW - fuzz_crash_019.md:66:12:66:12
EXPECTED RECORD ACCESSOR - fuzz_crash_019.md:83:2:83:5
MOD NOT FOUND - fuzz_crash_019.md:6:1:8:6
MOD NOT FOUND - fuzz_crash_019.md:10:1:10:19
MOD NOT FOUND - fuzz_crash_019.md:11:1:12:4
UNDECLARED TYPE - fuzz_crash_019.md:13:13:13:16
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:13:19:13:21
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:19:4:19:6
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:20:12:20:13
UNDECLARED TYPE - fuzz_crash_019.md:24:15:24:16
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:24:24:24:25
UNDECLARED TYPE - fuzz_crash_019.md:37:7:37:9
NAME NOT IN SCOPE - fuzz_crash_019.md:42:4:42:5
NAME NOT IN SCOPE - fuzz_crash_019.md:42:6:42:10
NAME NOT IN SCOPE - fuzz_crash_019.md:45:3:45:4
NAME NOT IN SCOPE - fuzz_crash_019.md:53:2:53:3
UNUSED VARIABLE - fuzz_crash_019.md:52:11:52:14
NAME NOT IN SCOPE - fuzz_crash_019.md:55:11:55:12
UNUSED VARIABLE - fuzz_crash_019.md:57:2:57:4
NAME NOT IN SCOPE - fuzz_crash_019.md:59:3:59:7
UNUSED VARIABLE - fuzz_crash_019.md:60:12:60:15
NAME NOT IN SCOPE - fuzz_crash_019.md:72:2:72:4
UNDECLARED TYPE - fuzz_crash_019.md:74:9:74:15
NAME NOT IN SCOPE - fuzz_crash_019.md:78:9:78:14
UNRECOGNIZED SYNTAX - fuzz_crash_019.md:83:2:83:5
NAME NOT IN SCOPE - fuzz_crash_019.md:86:9:86:11
NAME NOT IN SCOPE - fuzz_crash_019.md:87:11:87:12
NAME NOT IN SCOPE - fuzz_crash_019.md:92:11:92:15
NAME NOT IN SCOPE - fuzz_crash_019.md:93:2:93:7
NAME NOT IN SCOPE - fuzz_crash_019.md:94:3:94:6
NAME NOT IN SCOPE - fuzz_crash_019.md:96:34:96:37
NAME NOT IN SCOPE - fuzz_crash_019.md:96:47:96:52
NAME NOT IN SCOPE - fuzz_crash_019.md:96:54:96:57
DUPLICATE DEFINITION - fuzz_crash_019.md:97:2:97:3
NAME NOT IN SCOPE - fuzz_crash_019.md:97:21:97:24
NAME NOT IN SCOPE - fuzz_crash_019.md:97:30:97:32
INVALID ASSIGNMENT TO ITSELF - fuzz_crash_019.md:97:34:97:35
NAME NOT IN SCOPE - fuzz_crash_019.md:98:2:98:3
NAME NOT IN SCOPE - fuzz_crash_019.md:100:11:100:14
NAME NOT IN SCOPE - fuzz_crash_019.md:102:4:102:6
NAME NOT IN SCOPE - fuzz_crash_019.md:102:8:102:13
NAME NOT IN SCOPE - fuzz_crash_019.md:105:2:105:3
NAME NOT IN SCOPE - fuzz_crash_019.md:105:55:105:59
NAME NOT IN SCOPE - fuzz_crash_019.md:105:60:105:64
NAME NOT IN SCOPE - fuzz_crash_019.md:108:4:108:5
NAME NOT IN SCOPE - fuzz_crash_019.md:108:6:108:8
UNUSED VARIABLE - fuzz_crash_019.md:76:2:76:3
UNUSED VARIABLE - fuzz_crash_019.md:87:2:87:3
UNUSED VARIABLE - fuzz_crash_019.md:96:2:96:4
UNUSED VARIABLE - fuzz_crash_019.md:97:2:97:3
UNDECLARED TYPE - fuzz_crash_019.md:116:5:116:6
NAME NOT IN SCOPE - fuzz_crash_019.md:119:2:119:5
NAME NOT IN SCOPE - fuzz_crash_019.md:120:1:120:2
NAME NOT IN SCOPE - fuzz_crash_019.md:120:6:120:9
EXPOSED BUT NOT DEFINED - fuzz_crash_019.md:2:6:2:11
TOO FEW ARGS - fuzz_crash_019.md:17:3:18:4
DECLARATION HAS NO VALUE - fuzz_crash_019.md:22:1:23:2
DECLARATION HAS NO VALUE - fuzz_crash_019.md:37:1:37:9
MISSING METHOD - fuzz_crash_019.md:39:2:39:3
MISSING METHOD - fuzz_crash_019.md:58:6:58:11
TYPE MISMATCH - fuzz_crash_019.md:52:2:52:2
DECLARATION HAS NO VALUE - fuzz_crash_019.md:74:1:74:22
DECLARATION HAS NO VALUE - fuzz_crash_019.md:113:1:113:7
TOO FEW ARGS - fuzz_crash_019.md:84:2:86:3
MISSING METHOD - fuzz_crash_019.md:86:11:86:17
REFERENCE HAS NO VALUE - fuzz_crash_019.md:89:3:89:6
TYPE MISMATCH - fuzz_crash_019.md:98:4:104:3
TYPE MISMATCH - fuzz_crash_019.md:105:2:105:54
TYPE MISMATCH - fuzz_crash_019.md:93:22:93:24
DECLARATION HAS NO VALUE - fuzz_crash_019.md:116:1:116:13
MISSING METHOD - fuzz_crash_019.md:105:55:105:66
MISSING METHOD - fuzz_crash_019.md:105:55:105:72
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 52 16) (end 52 16))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 52 16) (end 52 16) (annotation error) (line-text "\tmatch a {lue  {"))))
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 58 4) (end 58 4))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 58 4) (end 58 4) (annotation error) (line-text "\t\t\t1\t\"for\" => 20[1, ] # t"))))
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 59 3) (end 59 3))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 59 3) (end 59 3) (annotation error) (line-text "\t\tment"))))
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 60 16) (end 60 16))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 60 16) (end 60 16) (annotation error) (line-text "\t\t[1, 2, 3,est]123"))))
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 62 5) (end 62 5))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 62 5) (end 62 5) (annotation error) (line-text "\t\t] 23"))))
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 63 7) (end 63 7))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 63 7) (end 63 7) (annotation error) (line-text "\t\t3.1 314"))))
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 66 12) (end 66 12))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 66 12) (end 66 12) (annotation error) (line-text "\t\t(1, 2, 3)123"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 83 2) (end 83 5))
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
			(source-region (file "fuzz_crash_019.md") (start 83 2) (end 83 5) (annotation error) (line-text "\t..."))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 6 1) (end 8 6))
		(headline
			(text "The mod ")
			(annotated code "Stdot")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 6 1) (end 8 6) (annotation error) (line-text "import Stdot\n\t\texposing [ #tem\nCust]"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 10 1) (end 10 19))
		(headline
			(text "The mod ")
			(annotated code "Bae")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 10 1) (end 10 19) (annotation error) (line-text "import Bae as Gooe"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 11 1) (end 12 4))
		(headline
			(text "The mod ")
			(annotated code "Ba")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 11 1) (end 12 4) (annotation error) (line-text "import\n\tBa"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 13 13) (end 13 16))
		(headline
			(reflow "The type ")
			(annotated code "Lis")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 13 13) (end 13 16) (annotation error) (line-text "Map(a, b) : Lis, (ab) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 13 19) (end 13 21))
		(headline
			(reflow "The type variable ")
			(annotated code "ab")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 13 19) (end 13 21) (annotation error) (line-text "Map(a, b) : Lis, (ab) -> List(b)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 19 4) (end 19 6))
		(headline
			(reflow "The type variable ")
			(annotated code "ab")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 19 4) (end 19 6) (annotation error) (line-text "\t\t(ab) -> # row"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 20 12) (end 20 13))
		(headline
			(reflow "The type variable ")
			(annotated code "b")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 20 12) (end 20 13) (annotation error) (line-text "\t\t\tList(\t\t\tb\t) #z)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 24 15) (end 24 16))
		(headline
			(reflow "The type ")
			(annotated code "O")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 24 15) (end 24 16) (annotation error) (line-text "Som : { foo : O, bar : g }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 24 24) (end 24 25))
		(headline
			(reflow "The type variable ")
			(annotated code "g")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 24 24) (end 24 25) (annotation error) (line-text "Som : { foo : O, bar : g }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 37 7) (end 37 9))
		(headline
			(reflow "The type ")
			(annotated code "U6")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 37 7) (end 37 9) (annotation error) (line-text "one : U6"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 42 4) (end 42 5))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "s")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 42 4) (end 42 5) (annotation error) (line-text "\t\t\ts exp0"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 42 6) (end 42 10))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "exp0")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 42 6) (end 42 10) (annotation error) (line-text "\t\t\ts exp0"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 45 3) (end 45 4))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "r")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 45 3) (end 45 4) (annotation error) (line-text "\t\tr"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 53 2) (end 53 3))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "x")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 53 2) (end 53 3) (annotation error) (line-text "\tx"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 52 11) (end 52 14))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "lue")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_lue")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 52 11) (end 52 14) (annotation error) (line-text "\tmatch a {lue  {"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 55 11) (end 55 12))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "x")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 55 11) (end 55 12) (annotation error) (line-text "\t\tBlue=> {x"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 57 2) (end 57 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "er")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_er")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 57 2) (end 57 4) (annotation error) (line-text "\ter #ent"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 59 3) (end 59 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ment")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 59 3) (end 59 7) (annotation error) (line-text "\t\tment"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 60 12) (end 60 15))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "est")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_est")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 60 12) (end 60 15) (annotation error) (line-text "\t\t[1, 2, 3,est]123"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 72 2) (end 72 4))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "nt")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 72 2) (end 72 4) (annotation error) (line-text "\tnt"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 74 9) (end 74 15))
		(headline
			(reflow "The type ")
			(annotated code "Listlt")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 74 9) (end 74 15) (annotation error) (line-text "main! : Listlt({}, _)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 78 9) (end 78 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "blaue")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 78 9) (end 78 14) (annotation error) (line-text "\texpect blaue"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 83 2) (end 83 5))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 83 2) (end 83 5) (annotation error) (line-text "\t..."))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 86 9) (end 86 11))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ke")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 86 9) (end 86 11) (annotation error) (line-text "\t)crash ke\"Unr!\" #)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 87 11) (end 87 12))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "d")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 87 11) (end 87 12) (annotation error) (line-text "\ti= \"H, ${d}\""))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 92 11) (end 92 15))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "list")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 92 11) (end 92 15) (annotation error) (line-text "\tfor n in list {"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 93 2) (end 93 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 93 2) (end 93 7) (annotation error) (line-text "\tline!(\"Ag ${n} to ${er}\")"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 94 3) (end 94 6))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ber")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 94 3) (end 94 6) (annotation error) (line-text "\t\tber + n"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 96 34) (end 96 37))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "tag")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 96 34) (end 96 37) (annotation error) (line-text "\trd = { foo: 123, bar: \"H\", baz: tag, qux: Ok(world),ned }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 96 47) (end 96 52))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "world")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 96 47) (end 96 52) (annotation error) (line-text "\trd = { foo: 123, bar: \"H\", baz: tag, qux: Ok(world),ned }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 96 54) (end 96 57))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ned")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 96 54) (end 96 57) (annotation error) (line-text "\trd = { foo: 123, bar: \"H\", baz: tag, qux: Ok(world),ned }"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 97 2) (end 97 3))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "t")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "fuzz_crash_019.md") (start 97 2) (end 97 3) (annotation error) (line-text "\tt = (123, \"World\", tag, O, (nd, t), [1, 2, 3])"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "t")
			(reflow " was already defined in ")
			(source-location
				(file "fuzz_crash_019.md")
				(line 88)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 88 1) (end 88 2) (annotation dim) (line-text "t = ["))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 97 21) (end 97 24))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "tag")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 97 21) (end 97 24) (annotation error) (line-text "\tt = (123, \"World\", tag, O, (nd, t), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 97 30) (end 97 32))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "nd")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 97 30) (end 97 32) (annotation error) (line-text "\tt = (123, \"World\", tag, O, (nd, t), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Invalid Assignment To Itself")
		(region (start 97 34) (end 97 35))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "t")
			(reflow " is assigned to itself, which would cause an infinite loop at runtime."))
		(document
			(reflow "Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 97 34) (end 97 35) (annotation error) (line-text "\tt = (123, \"World\", tag, O, (nd, t), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 98 2) (end 98 3))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "m")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 98 2) (end 98 3) (annotation error) (line-text "\tm ("))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 100 11) (end 100 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ag1")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 100 11) (end 100 14) (annotation error) (line-text "\t\t\"World\",ag1,"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 102 4) (end 102 6))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ne")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 102 4) (end 102 6) (annotation error) (line-text "\t\t(ne, tuple),"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 102 8) (end 102 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "tuple")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 102 8) (end 102 13) (annotation error) (line-text "\t\t(ne, tuple),"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 105 2) (end 105 3))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "b")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 105 2) (end 105 3) (annotation error) (line-text "\tb?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 105 55) (end 105 59))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "e_fn")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 105 55) (end 105 59) (annotation error) (line-text "\tb?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 105 60) (end 105 64))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "arg1")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 105 60) (end 105 64) (annotation error) (line-text "\tb?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 108 4) (end 108 5))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "r")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 108 4) (end 108 5) (annotation error) (line-text "\t\t\tr(nu) # xpr"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 108 6) (end 108 8))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "nu")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 108 6) (end 108 8) (annotation error) (line-text "\t\t\tr(nu) # xpr"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 76 2) (end 76 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "w")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_w")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 76 2) (end 76 3) (annotation error) (line-text "\tw = \"d\""))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 87 2) (end 87 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "i")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_i")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 87 2) (end 87 3) (annotation error) (line-text "\ti= \"H, ${d}\""))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 96 2) (end 96 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rd")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rd")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 96 2) (end 96 4) (annotation error) (line-text "\trd = { foo: 123, bar: \"H\", baz: tag, qux: Ok(world),ned }"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 97 2) (end 97 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "t")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_t")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 97 2) (end 97 3) (annotation error) (line-text "\tt = (123, \"World\", tag, O, (nd, t), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 116 5) (end 116 6))
		(headline
			(reflow "The type ")
			(annotated code "V")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 116 5) (end 116 6) (annotation error) (line-text "t : V((a,c))"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 119 2) (end 119 5))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "foo")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 119 2) (end 119 5) (annotation error) (line-text "\tfoo == 1"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 120 1) (end 120 2))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "h")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 120 1) (end 120 2) (annotation error) (line-text "h == foo"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 120 6) (end 120 9))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "foo")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_019.md") (start 120 6) (end 120 9) (annotation error) (line-text "h == foo"))))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 2 6) (end 2 11))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "main!")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 2 6) (end 2 11) (annotation error) (line-text "app [main!] { pf: platform \"c\" }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "main!")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Too Few Args")
		(region (start 17 3) (end 18 4))
		(headline
			(reflow "The type")
			(reflow " ")
			(annotated type "List")
			(reflow " ")
			(reflow "expects")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "argument,")
			(reflow " ")
			(reflow "but got")
			(reflow " ")
			(reflow "0")
			(reflow " ")
			(reflow "instead."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 17 3) (end 18 4) (annotation error) (line-text "\t\tList( #rg\n\t\t),"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 22 1) (end 23 2))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 22 1) (end 23 2) (annotation error) (line-text "line : ( # Cm\n) # Co"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 37 1) (end 37 9))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 37 1) (end 37 9) (annotation error) (line-text "one : U6"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 39 2) (end 39 3))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 39 2) (end 39 3) (annotation error) (line-text "\t1"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{}")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 58 6) (end 58 11))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 58 6) (end 58 11) (annotation error) (line-text "\t\t\t1\t\"for\" => 20[1, ] # t"))
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
			(text "[Blue, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 52 2) (end 69 3))
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
				(display (file "fuzz_crash_019.md") (start 52 2) (end 69 3) (annotation dim) (line-text "\tmatch a {lue  {\n\tx\n\t\t}\n\t\tBlue=> {x\n\t\t\t}\n\ter #ent\n\t\t\t1\t\"for\" => 20[1, ] # t\n\t\tment\n\t\t[1, 2, 3,est]123\n\t\t[\n\t\t] 23\n\t\t3.1 314\n\t\t3.14 | 6.28 => 314\n\t\t(1, ) => 123\n\t\t(1, 2, 3)123\n\t\t{ \t} => 12\n\t\tOk(123) => 12\n\t}"))
				(underline (start 58 17) (end 58 22) (annotation error)))
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
			(text "List(f)")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    f.is_eq : f, f -> Bool,")
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
			(text "[Blue, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 74 1) (end 74 22))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 74 1) (end 74 22) (annotation error) (line-text "main! : Listlt({}, _)"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 113 1) (end 113 7))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 113 1) (end 113 7) (annotation error) (line-text "y : {}"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Too Few Args")
		(region (start 84 2) (end 86 3))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "me")
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
			(source-region (file "fuzz_crash_019.md") (start 84 2) (end 86 3) (annotation error) (line-text "\tme(\n\t\t..., # r\n\t)crash ke\"Unr!\" #)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "me")
			(reflow " function has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, ..], [Tb] -> Error")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Are there any missing commas?")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 86 11) (end 86 17))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 86 11) (end 86 17) (annotation error) (line-text "\t)crash ke\"Unr!\" #)"))
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
			(text "{}")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Reference Has No Value")
		(region (start 89 3) (end 89 6))
		(headline
			(reflow "This refers to a declaration that has a type annotation but no implementation, so there is no value here to use."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 89 3) (end 89 6) (annotation error) (line-text "\t\tone(er, \t\t),\t456, # two"))
			(line-break)
			(line-break)
			(reflow "Give that declaration a value body, or stop referring to it here.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 98 4) (end 104 3))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 98 4) (end 104 3) (annotation error) (line-text "\tm (\n\t\t123,\n\t\t\"World\",ag1,\n\t\tO, # nt\n\t\t(ne, tuple),\n\t\t[1, 2, 3],\n\t)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(f, j, Error, [O, ..], (Error, Error), List(l))")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    j.from_quote : Str -> Try(j, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "    l.from_numeral : Numeral -> Try(l, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Since this expression is used as a statement, it must evaluate to")
			(reflow " ")
			(annotated code "{}")
			(reflow ".")
			(line-break)
			(reflow "If you don't need the value, you can ignore it with")
			(reflow " ")
			(annotated code "_ =")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 105 2) (end 105 54))
		(headline
			(reflow "This expression produces a value, but it's not being used."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 105 2) (end 105 54) (annotation error) (line-text "\tb?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Bool")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Since this expression is used as a statement, it must evaluate to")
			(reflow " ")
			(annotated code "{}")
			(reflow ".")
			(line-break)
			(reflow "If you don't need the value, you can ignore it with")
			(reflow " ")
			(annotated code "_ =")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 93 22) (end 93 24))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 93 22) (end 93 24) (annotation error) (line-text "\tline!(\"Ag ${n} to ${er}\")"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 116 1) (end 116 13))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 116 1) (end 116 13) (annotation error) (line-text "t : V((a,c))"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 105 55) (end 105 66))
		(headline
			(reflow "This is trying to dispatch a method named")
			(reflow " ")
			(annotated code "od")
			(reflow " ")
			(reflow "on an unresolved type variable, but unresolved type variables have no methods."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 105 55) (end 105 66) (annotation error) (line-text "\tb?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 105 55) (end 105 72))
		(headline
			(reflow "This is trying to dispatch a method named")
			(reflow " ")
			(annotated code "ned")
			(reflow " ")
			(reflow "on an unresolved type variable, but unresolved type variables have no methods."))
		(document
			(source-region (file "fuzz_crash_019.md") (start 105 55) (end 105 72) (annotation error) (line-text "\tb?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,CloseSquare,
KwImport,UpperIdent,
KwExposing,OpenSquare,
UpperIdent,CloseSquare,
KwImport,UpperIdent,KwAs,UpperIdent,
KwImport,
UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,Comma,OpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,
CloseRound,
OpColon,
UpperIdent,NoSpaceOpenRound,
CloseRound,Comma,
OpenRound,LowerIdent,CloseRound,OpArrow,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,OpenRound,
CloseRound,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,
CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,Int,KwElse,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Int,
KwIf,LowerIdent,OpenCurly,
KwDbg,
LowerIdent,LowerIdent,
CloseCurly,KwElse,OpenCurly,
KwDbg,Int,
LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,
LowerIdent,Comma,UpperIdent,Comma,
OpBar,
KwMatch,LowerIdent,OpenCurly,LowerIdent,OpenCurly,
LowerIdent,
CloseCurly,
UpperIdent,OpFatArrow,OpenCurly,LowerIdent,
CloseCurly,
LowerIdent,
Int,StringStart,StringPart,StringEnd,OpFatArrow,Int,OpenSquare,Int,Comma,CloseSquare,
LowerIdent,
OpenSquare,Int,Comma,Int,Comma,Int,Comma,LowerIdent,CloseSquare,Int,
OpenSquare,
CloseSquare,Int,
Float,Int,
Float,OpBar,Float,OpFatArrow,Int,
OpenRound,Int,Comma,CloseRound,OpFatArrow,Int,
OpenRound,Int,Comma,Int,Comma,Int,CloseRound,Int,
OpenCurly,CloseCurly,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,OpFatArrow,Int,
CloseCurly,
KwExpect,
LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,LowerIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
KwVar,LowerIdent,OpAssign,Int,
KwExpect,LowerIdent,
KwReturn,
LowerIdent,
TripleDot,
LowerIdent,NoSpaceOpenRound,
TripleDot,Comma,
CloseRound,KwCrash,LowerIdent,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,CloseRound,Comma,Int,Comma,
Int,Comma,
CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,UpperIdent,Comma,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpenRound,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,
UpperIdent,Comma,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,
CloseRound,
LowerIdent,OpDoubleQuestion,Int,OpGreaterThan,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,OpBinaryMinus,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,
StringStart,StringPart,OpenStringInterpolation,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseStringInterpolation,StringPart,StringEnd,Comma,
CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
KwExpect,OpenCurly,
LowerIdent,OpEquals,Int,
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
					(text "line!"))))
		(s-import (raw "Stdot")
			(exposing
				(exposed-upper-ident (text "Cust"))))
		(s-import (raw "Bae") (alias "Gooe"))
		(s-import (raw "Ba"))
		(s-type-decl
			(header (name "Map")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-fn
				(ty (name "Lis"))
				(ty-var (raw "ab"))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "MapML")
				(args))
			(ty-fn
				(ty-apply
					(ty (name "List")))
				(ty-var (raw "ab"))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-anno (name "line")
			(ty-tuple))
		(s-type-decl
			(header (name "Som")
				(args))
			(ty-record
				(anno-record-field (name "foo")
					(ty (name "O")))
				(anno-record-field (name "bar")
					(ty-var (raw "g")))))
		(s-type-decl
			(header (name "Ml")
				(args
					(ty-var (raw "a"))))
			(ty-record))
		(s-type-decl
			(header (name "Soine")
				(args
					(ty-var (raw "a"))))
			(ty-record))
		(s-type-decl
			(header (name "Maybe")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty (name "Somne")))))
		(s-type-decl
			(header (name "Mayine")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags)))
		(s-decl
			(p-ident (raw "ane"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-if-then-else
					(e-ident (raw "num"))
					(e-int (raw "2"))
					(e-int (raw "5")))))
		(s-type-anno (name "one")
			(ty (name "U6")))
		(s-decl
			(p-ident (raw "add"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-block
					(statements
						(e-int (raw "1"))
						(e-if-then-else
							(e-ident (raw "num"))
							(e-block
								(statements
									(s-dbg
										(e-ident (raw "s")))
									(e-ident (raw "exp0"))))
							(e-block
								(statements
									(s-dbg
										(e-int (raw "123")))
									(e-ident (raw "r")))))))))
		(s-decl
			(p-ident (raw "me"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-tag (raw "Tb")))
				(e-match
					(e-ident (raw "a"))
					(branches
						(branch
							(p-ident (raw "lue"))
							(e-block
								(statements
									(e-ident (raw "x")))))
						(branch
							(p-tag (raw "Blue"))
							(e-block
								(statements
									(e-ident (raw "x")))))
						(branch
							(p-ident (raw "er"))
							(e-int (raw "1")))
						(branch
							(p-string (raw """)
								(p-string-text (raw "for")))
							(e-int (raw "20")))
						(branch
							(p-list
								(p-int (raw "1")))
							(e-ident (raw "ment")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3"))
								(p-ident (raw "est")))
							(e-int (raw "123")))
						(branch
							(p-list)
							(e-int (raw "23")))
						(branch
							(p-frac (raw "3.1"))
							(e-int (raw "314")))
						(branch
							(p-alternatives
								(p-frac (raw "3.14"))
								(p-frac (raw "6.28")))
							(e-int (raw "314")))
						(branch
							(p-tuple
								(p-int (raw "1")))
							(e-int (raw "123")))
						(branch
							(p-tuple
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3")))
							(e-int (raw "123")))
						(branch
							(p-record)
							(e-int (raw "12")))
						(branch
							(p-tag (raw "Ok")
								(p-int (raw "123")))
							(e-int (raw "12")))))))
		(s-expect
			(e-ident (raw "nt")))
		(s-type-anno (name "main!")
			(ty-apply
				(ty (name "Listlt"))
				(ty-record)
				(_)))
		(s-decl
			(p-ident (raw "ma"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-ident (raw "e"))
						(s-decl
							(p-ident (raw "w"))
							(e-string
								(e-string-part (raw "d"))))
						(s-var (name "er")
							(e-int (raw "123")))
						(s-expect
							(e-ident (raw "blaue")))
						(s-return
							(e-malformed (reason "expr_dot_suffix_not_allowed")))
						(e-apply
							(e-ident (raw "me"))
							(e-ellipsis))
						(s-crash
							(e-ident (raw "ke")))
						(e-string
							(e-string-part (raw "Unr!")))
						(s-decl
							(p-ident (raw "i"))
							(e-string
								(e-string-part (raw "H, "))
								(e-ident (raw "d"))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "t"))
							(e-list
								(e-apply
									(e-ident (raw "one"))
									(e-ident (raw "er")))
								(e-int (raw "456"))
								(e-int (raw "9"))))
						(s-for
							(p-ident (raw "n"))
							(e-ident (raw "list"))
							(e-block
								(statements
									(e-apply
										(e-ident (raw "line!"))
										(e-string
											(e-string-part (raw "Ag "))
											(e-ident (raw "n"))
											(e-string-part (raw " to "))
											(e-ident (raw "er"))
											(e-string-part (raw ""))))
									(e-binop (op "+")
										(e-ident (raw "ber"))
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
							(p-ident (raw "t"))
							(e-tuple
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "World")))
								(e-ident (raw "tag"))
								(e-tag (raw "O"))
								(e-tuple
									(e-ident (raw "nd"))
									(e-ident (raw "t")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(e-ident (raw "m"))
						(e-tuple
							(e-int (raw "123"))
							(e-string
								(e-string-part (raw "World")))
							(e-ident (raw "ag1"))
							(e-tag (raw "O"))
							(e-tuple
								(e-ident (raw "ne"))
								(e-ident (raw "tuple")))
							(e-list
								(e-int (raw "1"))
								(e-int (raw "2"))
								(e-int (raw "3"))))
						(e-binop (op "or")
							(e-binop (op ">")
								(e-binop (op "??")
									(e-ident (raw "b"))
									(e-int (raw "12")))
								(e-int (raw "5")))
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
									(e-int (raw "3")))))
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
																(e-apply
																	(e-ident (raw "e_fn"))
																	(e-ident (raw "arg1")))))
														(args))))
											(args))))
								(segment (mode "required") (field "recd"))))
						(e-apply
							(e-tag (raw "Stdo!"))
							(e-string
								(e-string-part (raw "Ho"))
								(e-apply
									(e-ident (raw "r"))
									(e-ident (raw "nu")))
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
					(e-binop (op "==")
						(e-ident (raw "foo"))
						(e-int (raw "1")))
					(e-binop (op "==")
						(e-ident (raw "h"))
						(e-ident (raw "foo"))))))))
~~~
# FORMATTED
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!]

import Stdot
	exposing [ # tem
		Cust,
	]

import Bae as Gooe
import
	Ba
Map(a, b) : Lis, (ab) -> List(b)

MapML # Ag
	: # Aon
		List( # rg
		),
		(ab) -> # row
			List(b) # z)

line : ( # Cm
) # Co

Som : { foo : O, bar : g }

Ml(a) : { # ld
}

Soine(a) : { #
} #

Maybe(a) : [Somne]

Mayine(a) : [] # )

ane = |num| if num 2 else 5

one : U6

add = |num| {
	1
	if num {
		dbg # bug
			s
		exp0
	} else {
		dbg 123
		r
	}
}

me = |
	a,
	Tb,
| # As
	match a {
		lue => {
			x
		}
		Blue => {
			x
		}
		er # ent
			=> # ent
				1
		"for" => 20
		[
			1,
		] # t
			=> # t
				ment
		[1, 2, 3, est] => 123
		[] => 23
		3.1 => 314
		3.14 | 6.28 => 314
		(
			1,
		) => 123
		(1, 2, 3) => 123
		{} => 12
		Ok(123) => 12
	}

expect # Cord
	nt

main! : Listlt({}, _)

ma = |_| {
	e
	w = "d"
	var er = 123
	expect blaue
	return

	#
		
	me(
		..., # r
	)
	crash ke
	"Unr!" # )
	i = "H, ${d}"
	t = [
		one(
			er,
		),
		456, # two
		9, # ee
	]
	for n in list {
		line!("Ag ${n} to ${er}")
		ber + n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world), ned }
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
	m
	(
		123,
		"World",
		ag1,
		O, # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b ?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3
	e_fn(arg1)?.od()?.ned()?.recd?
	Stdo!(
		"Ho${ #
			r(nu) # xpr
		} ",
	)
} # Cocl

y : {}

e = {}

t : V((a, c))

expect {
	foo == 1
	h == foo
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "line"))
		(e-anno-only)
		(annotation
			(ty-tuple)))
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
		(p-assign (ident "one"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "add"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "me"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
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
			(exposed (name "line!") (wildcard false))))
	(s-import (mod "Stdot")
		(exposes
			(exposed (name "Cust") (wildcard false))))
	(s-import (mod "Bae")
		(exposes))
	(s-import (mod "Ba")
		(exposes))
	(s-alias-decl
		(ty-header (name "Map")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-fn (effectful false)
			(ty-malformed)
			(ty-parens
				(ty-malformed))
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "MapML"))
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin))
			(ty-parens
				(ty-malformed))
			(ty-apply (name "List") (builtin)
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Som"))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Ml")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record))
	(s-alias-decl
		(ty-header (name "Soine")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record))
	(s-alias-decl
		(ty-header (name "Maybe")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Somne"))))
	(s-alias-decl
		(ty-header (name "Mayine")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union))
	(s-expect
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "()"))
		(patt (type "Bool -> f where [f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "Bool -> Error"))
		(patt (type "[Blue, ..], [Tb] -> Error"))
		(patt (type "Error"))
		(patt (type "_arg -> Error"))
		(patt (type "{}"))
		(patt (type "{}"))
		(patt (type "Error")))
	(type_decls
		(alias (type "Error")
			(ty-header (name "Map")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Error")
			(ty-header (name "MapML")))
		(alias (type "Error")
			(ty-header (name "Som")))
		(alias (type "Ml(a)")
			(ty-header (name "Ml")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Soine(a)")
			(ty-header (name "Soine")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Maybe(a)")
			(ty-header (name "Maybe")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Mayine(a)")
			(ty-header (name "Mayine")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "()"))
		(expr (type "Bool -> f where [f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)])]"))
		(expr (type "Error"))
		(expr (type "Bool -> Error"))
		(expr (type "[Blue, ..], [Tb] -> Error"))
		(expr (type "Error"))
		(expr (type "_arg -> Error"))
		(expr (type "{}"))
		(expr (type "{}"))
		(expr (type "Error"))))
~~~
