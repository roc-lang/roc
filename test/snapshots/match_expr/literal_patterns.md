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
UNCONDITIONAL CONDITION - literal_patterns.md:1:7:1:13
MISSING METHOD - literal_patterns.md:5:5:5:7
TYPE MISMATCH - literal_patterns.md:3:13:3:20
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 1 7) (end 1 13))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "match value")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this match will always inspect the same value."))
		(document
			(source-region (file "literal_patterns.md") (start 1 7) (end 1 13) (annotation warning) (line-text "match Answer {"))))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 5 5) (end 5 7))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "literal_patterns.md") (start 5 5) (end 5 7) (annotation error) (line-text "    10 => 4"))
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
			(text "[Answer, Greeting, Zero, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 13) (end 3 20))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "literal_patterns.md") (start 3 13) (end 3 20) (annotation error) (line-text "    Zero => \"hello\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end))))
~~~
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
					(e-runtime-error (tag "erroneous_value_expr"))))
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
(expr (type "Dec"))
~~~
