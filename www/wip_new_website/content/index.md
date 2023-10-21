<h1 id="homepage-h1">Roc</h1>

<p id="tagline">A fast, friendly, functional language.</p>

<!-- This exact sample was chosen for several reasons:

1. It's plausible to figure out what it's doing even if you don't know the language yet.
2. It uses a higher-order function, giving a functional first impression.
3. It shows some things not found in most mainstream languages, e.g. function calls without parens, lambda syntax.
4. It shows some things not found in most FP languages, e.g. string interpolation, passing a lambda without `<|` or `$`
5. It's horizontally small enough that it can be read on mobile without a scroll bar or shrinking the font size.
-->
<pre id="first-code-sample"><samp class="code-snippet">list <span class="kw">=</span> List<span class="punctuation section">.</span>map songs <span class="kw">\</span>song <span class="kw">-></span>
    <span class="string">"Artist: </span><span class="kw">\(</span>song<span class="punctuation section">.</span>artist<span class="kw">)</span><span class="string">"</span></samp></pre>

<p><b>Fast</b> - Roc code is designed to build fast and run fast. It compiles to machine code or to <a href="https://webassembly.org/">WebAssembly</a>. Like <a href="https://rust-lang.org">Rust</a> and <a href="https://clang.llvm.org/">clang</a>, it compiles to unboxed data structures using monomorphization and LLVM for optimizations.</p>
<p><b>Friendly</b></p>
<p><b>Functional</b></p>

## Try Roc

<link rel="stylesheet" href="/wip/repl.css" />
<div id="repl">
<code class="history">
  <div id="help-text"></div>
  <div id="history-text"><div id="loading-message">Loading REPL WebAssembly module…please wait!</div></div>
</code>
<section id="source-input-wrapper">
  <textarea rows="5" autofocus id="source-input" placeholder="You can enter Roc code here once the REPL loads!"
    disabled></textarea>
</section>
</div>
<script type="module" src="/wip/repl.js"></script>
</div>

## Examples

We have developed a number of smaller code [examples](https://github.com/roc-lang/examples) which demonstrate how to use Roc. These cover a range of topics from basic syntax to more advanced features such as random number generation and using the popular `Task` feature.

## Use cases

-   Tools & Scripts
-   Web (coming soon)
-   Networking & Servers (coming soon)
-   Graphical (coming soon)
-   Scientific (coming soon)
-   Embedded (coming soon)

## Platforms & Applications

TODO provide explanation of platform/application abstraction versus libraries as common in most other languages as this is one of the most unique features of Roc

## Talks and Publications

If you'd like to learn more about Roc check out one of these videos:

*   [Roc at Handmade Seattle](https://media.handmade-seattle.com/roc-lang) - November 12, 2021 (very low-level explanation of how Roc's compiler makes programs run fast)
*   [Outperforming Imperative with Pure Functional Languages](https://youtu.be/vzfy4EKwG_Y) - October 1, 2021 (about Roc's runtime performance and optimizer)
*   [A taste of Roc](https://youtu.be/6qzWm_eoUXM) - September 23, 2021 (syntax, application examples)
*   [Roc at the Philly ETE conference](https://youtu.be/cpQwtwVKAfU?t=75) - May 6, 2021 (platforms and applications)
*   [Roc on Zig Showtime](https://youtu.be/FMyyYdFSOHA) - April 24, 2021 (making a platform)
*   [Roc at the Berlin FP Meetup](https://youtu.be/ZnYa99QoznE?t=4790) - September 1, 2020 (overall vision for the language)
