# META
~~~ini
description=Match expression with tag patterns for different cases
type=expr
~~~
# SOURCE
~~~roc
match Answer {
    Answer => 1
    Zero => "hello"
    Greeting => 3
    10 => 4
}
~~~
# EXPECTED
MISSING METHOD - literal_patterns.md:2:15:2:16
MISSING METHOD - literal_patterns.md:4:17:4:18
TYPE DOES NOT HAVE METHODS - literal_patterns.md:5:5:5:7
MISSING METHOD - literal_patterns.md:5:11:5:12
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**literal_patterns.md:2:15:2:16:**
```roc
    Answer => 1
```
              ^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**literal_patterns.md:4:17:4:18:**
```roc
    Greeting => 3
```
                ^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_numeral` on a type that doesn't support methods:
**literal_patterns.md:5:5:5:7:**
```roc
    10 => 4
```
    ^^

This type doesn't support methods:
    _[Answer, Zero, Greeting]_others_



**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**literal_patterns.md:5:11:5:12:**
```roc
    10 => 4
```
          ^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

# TOKENS
~~~zig
KwMatch,UpperIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,Int,
Int,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-tag (raw "Answer"))
	(branches
		(branch
			(p-tag (raw "Answer"))
			(e-int (raw "1")))
		(branch
			(p-tag (raw "Zero"))
			(e-string
				(e-string-part (raw "hello"))))
		(branch
			(p-tag (raw "Greeting"))
			(e-int (raw "3")))
		(branch
			(p-int (raw "10"))
			(e-int (raw "4")))))
~~~
# FORMATTED
~~~roc
match Answer {
	Answer => 1
	Zero => "hello"
	Greeting => 3
	10 => 4
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-tag (name "Answer")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-string
						(e-literal (string "hello")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "3"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-num (value "10"))))
				(value
					(e-num (value "4")))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
