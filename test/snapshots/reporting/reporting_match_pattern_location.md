# META
~~~ini
description=A focused match-pattern diagnostic locates and underlines the pattern while retaining the surrounding match expression (issue 13070)
type=reporting
~~~
# SOURCE
~~~roc
Color : [Red, Green]

name : Color -> Str
name = |color|
    match color {
        Red => "red"
        Gren => "green"
    }
~~~
# EXPECTED
TYPE MISMATCH - reporting_match_pattern_location.md:5:5:5:5
# REPORT
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 7 9) (end 7 13))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous ones."))
		(document
			(source-underlines
				(display (file "reporting_match_pattern_location.md") (start 5 5) (end 8 6) (annotation dim) (line-text "    match color {\n        Red => \"red\"\n        Gren => \"green\"\n    }"))
				(underline (start 7 9) (end 7 13) (annotation error)))
			(line-break)
			(reflow "This")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Gren]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the value being matched on has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Color")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Maybe")
			(reflow " ")
			(annotated code "Gren")
			(reflow " ")
			(reflow "should be")
			(reflow " ")
			(annotated code "Green")
			(reflow "?"))))
~~~
# CLI
~~~text
── ✗ type mismatch ───────────────────── reporting_match_pattern_location.md:7:9

The second branch of this match does not match the previous ones.

match color {
    Red => "red"
    Gren => "green"
    ^^^^
}

This second branch is trying to match:

    [Gren]

But the value being matched on has the type:

    Color

These can never match! Either the pattern or expression has a problem.
Hint: Maybe Gren should be Green?

~~~
# MARKDOWN
~~~markdown
**Type Mismatch**
The second branch of this `match` does not match the previous ones.
```roc
    match color {
        Red => "red"
        Gren => "green"
    }
```
        ^^^^

This second branch is trying to match:

    [Gren]

But the value being matched on has the type:

    Color

These can never match! Either the pattern or expression has a problem.
**Hint:** Maybe `Gren` should be `Green`?

~~~
# HTML
~~~html
<div class="report error">
<h1 class="report-title">type mismatch</h1>
<div class="report-content">
The second branch of this <code class="code">match</code> does not match the previous ones.<br>
<div class="source-region"><pre class="source-code">    match color {
        Red =&gt; &quot;red&quot;
        Gren =&gt; &quot;green&quot;
        ^^^^
    }
</pre></div><br>
This second branch is trying to match:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;[Gren]</pre><br>
<br>
But the value being matched on has the type:<br>
<br>
<pre class="code-block">&nbsp;&nbsp;&nbsp;&nbsp;Color</pre><br>
<br>
These can never match! Either the pattern or expression has a problem.<br>
<strong class="emphasis">Hint:</strong> Maybe <code class="code">Gren</code> should be <code class="code">Green</code>?</div>
</div>
~~~
# LSP
~~~text
The second branch of this match does not match the previous ones.
    match color {
        Red => "red"
        Gren => "green"
        ^^^^
    }

This second branch is trying to match:

  [Gren]

But the value being matched on has the type:

  Color

These can never match! Either the pattern or expression has a problem.
Hint: Maybe Gren should be Green?
~~~
