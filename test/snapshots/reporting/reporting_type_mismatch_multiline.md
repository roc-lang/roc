# META
~~~ini
description=Renderer coverage for a type mismatch with a multiline source region and text that wraps differently per renderer
type=reporting
~~~
# SOURCE
~~~roc
result_of_a_computation_with_a_rather_long_name : Str
result_of_a_computation_with_a_rather_long_name = [
	1,
	2,
	3,
]
~~~
# EXPECTED
TYPE MISMATCH - reporting_type_mismatch_multiline.md:2:51:6:2
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 51) (end 6 2))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "reporting_type_mismatch_multiline.md") (start 2 51) (end 6 2) (annotation error) (line-text "result_of_a_computation_with_a_rather_long_name = [\n\t1,\n\t2,\n\t3,\n]"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# CLI
~~~text
── ✗ type mismatch ─────────────────── reporting_type_mismatch_multiline.md:2:51

This expression is used in an unexpected way.

result_of_a_computation_with_a_rather_long_name = [
    1,
    2,
    3,
]

It has the type:

    List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

But the annotation says it should be:

    Str

~~~
# MARKDOWN
~~~markdown
**Type Mismatch**
This expression is used in an unexpected way.
```roc
result_of_a_computation_with_a_rather_long_name = [
	1,
	2,
	3,
]
```

It has the type:

    List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

But the annotation says it should be:

    Str

~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">type mismatch</h1>
<div class="report-content">
This expression is used in an unexpected way.<br>
<div class="source-region"><pre class="error">result_of_a_computation_with_a_rather_long_name = [
	1,
	2,
	3,
]</pre></div><br>
It has the type:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;List(a) where [a.from_numeral : Numeral -&gt; Try(a, [InvalidNumeral(Str)])]</pre><br>
<br>
But the annotation says it should be:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;Str</pre></div>
</div>
~~~
# LSP
~~~text
type mismatch

This expression is used in an unexpected way.
result_of_a_computation_with_a_rather_long_name = [
	1,
	2,
	3,
]

It has the type:

  List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

But the annotation says it should be:

  Str
~~~
