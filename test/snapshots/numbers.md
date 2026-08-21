# META
~~~ini
description=Number formats
type=expr
~~~
# SOURCE
~~~roc
(
    0X42,
    0x42,
    0B01,
    0b01,
    0O42,
    0o42,
    0.1e42,
    0.1E42,
    1.e42,
    1.E42,
    0xDEADBEEF,
    0xdeadbeef,
    0xDeAdBeEf,
)
~~~
# EXPECTED
UPPERCASE BASE - :0:0:0:0
UPPERCASE BASE - :0:0:0:0
UPPERCASE BASE - :0:0:0:0
INVALID NUMBER - numbers.md:8:5:8:11
INVALID NUMBER - numbers.md:9:5:9:11
INVALID NUMBER - numbers.md:10:5:10:10
INVALID NUMBER - numbers.md:11:5:11:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Uppercase Base")
		(headline
			(reflow "Number base prefixes must be lowercase (0x, 0o, 0b)."))
		(document))
	(report
		(severity runtime_error)
		(title "Uppercase Base")
		(headline
			(reflow "Number base prefixes must be lowercase (0x, 0o, 0b)."))
		(document))
	(report
		(severity runtime_error)
		(title "Uppercase Base")
		(headline
			(reflow "Number base prefixes must be lowercase (0x, 0o, 0b)."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 8 5) (end 8 11))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "numbers.md") (start 8 5) (end 8 11) (annotation error) (line-text "    0.1e42,"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 9 5) (end 9 11))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "numbers.md") (start 9 5) (end 9 11) (annotation error) (line-text "    0.1E42,"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 10 5) (end 10 10))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "numbers.md") (start 10 5) (end 10 10) (annotation error) (line-text "    1.e42,"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 11 5) (end 11 10))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "numbers.md") (start 11 5) (end 11 10) (annotation error) (line-text "    1.E42,"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end))))
~~~
# TOKENS
~~~zig
OpenRound,
Int,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
Float,Comma,
Float,Comma,
Float,Comma,
Float,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-int (raw "0X42"))
	(e-int (raw "0x42"))
	(e-int (raw "0B01"))
	(e-int (raw "0b01"))
	(e-int (raw "0O42"))
	(e-int (raw "0o42"))
	(e-frac (raw "0.1e42"))
	(e-frac (raw "0.1E42"))
	(e-frac (raw "1.e42"))
	(e-frac (raw "1.E42"))
	(e-int (raw "0xDEADBEEF"))
	(e-int (raw "0xdeadbeef"))
	(e-int (raw "0xDeAdBeEf")))
~~~
# FORMATTED
~~~roc
(
	0X42,
	0x42,
	0B01,
	0b01,
	0O42,
	0o42,
	0.1e42,
	0.1E42,
	1.e42,
	1.E42,
	0xDEADBEEF,
	0xdeadbeef,
	0xDeAdBeEf,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-num (value "66"))
		(e-num (value "66"))
		(e-num (value "1"))
		(e-num (value "1"))
		(e-num (value "34"))
		(e-num (value "34"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-num (value "3735928559"))
		(e-num (value "3735928559"))
		(e-num (value "3735928559"))))
~~~
# TYPES
~~~clojure
(expr (type "(Dec, Dec, Dec, Dec, Dec, Dec, Error, Error, Error, Error, Dec, Dec, Dec)"))
~~~
