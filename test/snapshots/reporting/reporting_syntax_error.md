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
UNRECOGNIZED SYNTAX - reporting_syntax_error.md:1:1:1:1
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
			(source-region (file "reporting_syntax_error.md") (start 2 1) (end 2 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "reporting_syntax_error.md") (start 1 1) (end 1 1) (annotation error) (line-text "x = (1 + 2"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# CLI
~~~text

┌──────────────────────────┐
│ EXPECTED TUPLE SEPARATOR ├─ I was parsing a parenthesized expression or ────┐
└┬─────────────────────────┘  tuple, and I expected `,` or `)`.               │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └───────────────────────────────────────────── reporting_syntax_error.md:2:1 ┘

    Separate tuple elements with commas and close the tuple or parenthesized
    expression with `)`.

    For example:
        (x, y)

    I reached the end of the file before this construct was complete.


┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  x = (1 + 2                                                                │
 │  ‾                                                                         │
 └───────────────────────────────────────────── reporting_syntax_error.md:1:1 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

~~~
# MARKDOWN
~~~markdown
**Expected Tuple Separator**
I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.
Separate tuple elements with commas and close the tuple or parenthesized expression with `)`.

For example:
    (x, y)

I reached the end of the file before this construct was complete.

**reporting_syntax_error.md:2:1:2:1:**
```roc

```
^


**Unrecognized Syntax**
I don't recognize this syntax.
**reporting_syntax_error.md:1:1:1:1:**
```roc
x = (1 + 2
```
^

This might be a syntax error, an unsupported language feature, or a typo.

~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">EXPECTED TUPLE SEPARATOR</h1>
<div class="report-content">
I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.<br>
Separate tuple elements with commas and close the tuple or parenthesized expression with <code class="code">)</code>.<br>
<br>
For example:<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;(x, y)</pre><br>
<br>
I reached the end of the file before this construct was complete.<br>
<br>
<div class="source-region"><span class="filename">reporting_syntax_error.md:2:1:2:1:</span> <pre class="error"></pre></div></div>
</div>
<div class="report error">
<h1 class="report-title">UNRECOGNIZED SYNTAX</h1>
<div class="report-content">
I don&#39;t recognize this syntax.<br>
<div class="source-region"><span class="filename">reporting_syntax_error.md:1:1:1:1:</span> <pre class="error">x = (1 + 2</pre></div><br>
This might be a syntax error, an unsupported language feature, or a typo.</div>
</div>
~~~
# LSP
~~~text
EXPECTED TUPLE SEPARATOR

I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.
Separate tuple elements with commas and close the tuple or parenthesized expression with ).

For example:
  (x, y)

I reached the end of the file before this construct was complete.

reporting_syntax_error.md:2:1:2:1: 
UNRECOGNIZED SYNTAX

I don't recognize this syntax.
reporting_syntax_error.md:1:1:1:1: x = (1 + 2

This might be a syntax error, an unsupported language feature, or a typo.
~~~
