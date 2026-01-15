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
TYPE MISMATCH - literal_patterns.md:2:15:2:16
TYPE MISMATCH - literal_patterns.md:4:17:4:18
MISSING METHOD - literal_patterns.md:5:5:5:7
TYPE MISMATCH - literal_patterns.md:5:11:5:12
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**literal_patterns.md:2:15:2:16:**
```roc
    Answer => 1
```
              ^

The type was determined to be non-numeric here:
**literal_patterns.md:3:13:3:20:**
```roc
    Zero => "hello"
```
            ^^^^^^^

Other code expects this to have the type:

    Str

**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**literal_patterns.md:4:17:4:18:**
```roc
    Greeting => 3
```
                ^

Other code expects this to have the type:

    Str

**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**literal_patterns.md:5:5:5:7:**
```roc
    10 => 4
```
    ^^

The value's type, which does not have a method named **from_numeral**, is:

    [Answer, Zero, Greeting, .._others]

**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**literal_patterns.md:5:11:5:12:**
```roc
    10 => 4
```
          ^

Other code expects this to have the type:

    Str

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
