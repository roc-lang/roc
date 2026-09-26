# META
~~~ini
description=Renderer coverage for a tokenizer/parser diagnostic with source regions
type=reporting
~~~
# SOURCE
~~~roc
x = (1 + 2
~~~
# EXPECTED
EXPECTED TUPLE SEPARATOR - reporting_syntax_error.md:2:1:2:1
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Tuple Separator")
		(region (start 2 1) (end 2 1))
		(headline
			(reflow "I was parsing a parenthesized expression or tuple, and I expected `,` or `)`."))
		(document
			(reflow "Separate tuple elements with commas and close the tuple or parenthesized expression with ")
			(annotated code ")")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(x, y)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "reporting_syntax_error.md") (start 2 1) (end 2 1) (annotation error) (line-text "")))))
~~~
# CLI
~~~text
── ✗ expected tuple separator ──────────────────── reporting_syntax_error.md:2:1

I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.


^

Separate tuple elements with commas and close the tuple or parenthesized
expression with ).

For example:
    (x, y)

I reached the end of the file before this construct was complete.

~~~
# MARKDOWN
~~~markdown
**Expected Tuple Separator**
I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.
Separate tuple elements with commas and close the tuple or parenthesized expression with `)`.

For example:
    (x, y)

I reached the end of the file before this construct was complete.

```roc

```
^


~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">expected tuple separator</h1>
<div class="report-content">
I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.<br>
Separate tuple elements with commas and close the tuple or parenthesized expression with <code class="code">)</code>.<br>
<br>
For example:<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;(x, y)</pre><br>
<br>
I reached the end of the file before this construct was complete.<br>
<br>
<div class="source-region"><pre class="error">
^
</pre></div></div>
</div>
~~~
# LSP
~~~text
expected tuple separator

I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.
Separate tuple elements with commas and close the tuple or parenthesized expression with ).

For example:
  (x, y)

I reached the end of the file before this construct was complete.


^
~~~
