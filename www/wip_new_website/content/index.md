<div role="presentation" id="homepage-intro-box">
  <h1 id="homepage-h1">Roc</h1>
  <svg id="homepage-logo" alt="The Roc logo, a purple origami bird made of six triangles" width="240" height="240" viewBox="0 0 51 53" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M23.6751 22.7086L17.655 53L27.4527 45.2132L26.4673 39.3424L23.6751 22.7086Z" fill="#612BDE"/>
      <path d="M37.2438 19.0101L44.0315 26.3689L45 22L45.9665 16.6324L37.2438 19.0101Z" fill="#8257E5"/>
      <path d="M23.8834 3.21052L0 0L23.6751 22.7086L23.8834 3.21052Z" fill="#8257E5"/>
      <path d="M44.0315 26.3689L23.6751 22.7086L26.4673 39.3424L44.0315 26.3689Z" fill="#8257E5"/>
      <path d="M50.5 22L45.9665 16.6324L45 22H50.5Z" fill="#612BDE"/>
      <path d="M23.6751 22.7086L44.0315 26.3689L37.2438 19.0101L23.8834 3.21052L23.6751 22.7086Z" fill="#612BDE"/>
  </svg>


  <p id="homepage-tagline">A fast, friendly, functional language.</p>

  <!-- This exact sample was chosen for several reasons:

  1. It's plausible to figure out what it's doing even if you don't know the language yet.
  2. It uses a higher-order function, giving a functional first impression.
  3. It shows some things not found in most mainstream languages, e.g. function calls without parens, lambda syntax.
  4. It shows some things not found in most FP languages, e.g. string interpolation, passing a lambda without `<|` or `$`
  5. It's horizontally small enough that it can be read on mobile without a scroll bar or shrinking the font size.
  -->
  <pre id="first-code-sample"><samp class="code-snippet">list <span class="kw">=</span> List<span class="punctuation section">.</span>map songs <span class="kw">\</span>song <span class="kw">-></span>
      <span class="string">"Artist: </span><span class="kw">\(</span>song<span class="punctuation section">.</span>artist<span class="kw">)</span><span class="string">"</span></samp></pre>
</div>

<section class="home-goals-container">
    <div class="home-goals-column">
        <div class="home-goals-content">
            <h3 class="home-goals-title"><a href="/fast">Fast</a></h3>
            <p class="home-goals-description">Roc code is designed to build fast and run fast. <span class="nobreak-on-mobile">It compiles to machine code or WebAssembly.</span></p>
            <p class="home-goals-learn-more"><a href="/fast">What does <i>fast</i> mean here?</a></p>
        </div>
    </div>
    <div class="home-goals-column">
        <div class="home-goals-content">
            <h3 class="home-goals-title"><a href="/friendly">Friendly</a></h3>
            <p class="home-goals-description">Roc’s syntax, semantics, and included toolset <span class="nobreak-on-mobile">all prioritize user-friendliness.</span></p>
            <p class="home-goals-learn-more"><a href="/friendly">What does <i>friendly</i> mean here?</a></p>
        </div>
    </div>
    <div class="home-goals-column">
        <div class="home-goals-content">
            <h3 class="home-goals-title"><a href="/functional">Functional</a></h3>
            <p class="home-goals-description">
             Roc has a small number of simple language primitives. <span class="nobreak-on-mobile">It’s a single-paradigm functional language.</span></p>
            <p class="home-goals-learn-more"><a href="/design_goals.html#functional">What does <i>functional</i> mean here?</a></p>
        </div>
    </div>
</section>

## Try Roc

<section id="homepage-repl-container">
    <div id="repl-description" role="presentation">
        <p>You can try out Roc using this read-eval-print loop (<a href="https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop">REPL</a>), which is running in your browser in <a href="https://webassembly.org">WebAssembly</a>.</p>
        <p><code>Shift-Enter</code> adds a newline.</p>
        <p><span id="repl-arrow">←</span> Try entering <code>0.1 + 0.2</code></p>
    </div>
    <div id="repl" role="presentation">
        <code class="history">
          <div id="help-text"></div>
          <div id="history-text"><div id="loading-message">Loading REPL WebAssembly module…please wait!</div></div>
        </code>
        <section id="source-input-wrapper">
          <textarea rows="5" id="source-input" placeholder="You can enter Roc code here once the REPL loads!"
            onfocus="document.getElementById('repl-arrow').style.display='none';"></textarea>
        </section>
    </div>
    <script type="module" src="/wip/repl.js"></script>
</section>

## Use Cases

Roc is a very young language (it doesn’t even have a numbered release yet, just nightly builds!) but it can already be used for several things if you’re up for being an early adopter—with all the bugs and missing features which come with that territory.

Currently these use cases are the best-supported:

### Command-Line Interfaces (CLIs)

### Web Servers

### Embedding

Calling Roc functions from another language

### Others

You can create your own! Learn about **platforms and applications**...

## Larger Example

Here’s a larger example that shows a few different aspects of Roc:
* File I/O and HTTP requests
* Pattern matching for error handling
* JSON deserialization via type inference
* Common syntax sugar: string interpolation, pipelines, and backpassing

The [tutorial](/tutorial) introduces these gradually and in more depth, but this gives you a brief overview.

<!-- ## More Examples

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

*   [Roc at Handmade Seattle](https://media.handmade-seattle.com/roc-lang) - November 12, 2021 (very low-level explanation of how Roc’s compiler makes programs run fast)
*   [Outperforming Imperative with Pure Functional Languages](https://youtu.be/vzfy4EKwG_Y) - October 1, 2021 (about Roc’s runtime performance and optimizer)
*   [A taste of Roc](https://youtu.be/6qzWm_eoUXM) - September 23, 2021 (syntax, application examples)
*   [Roc at the Philly ETE conference](https://youtu.be/cpQwtwVKAfU?t=75) - May 6, 2021 (platforms and applications)
*   [Roc on Zig Showtime](https://youtu.be/FMyyYdFSOHA) - April 24, 2021 (making a platform)
*   [Roc at the Berlin FP Meetup](https://youtu.be/ZnYa99QoznE?t=4790) - September 1, 2020 (overall vision for the language) -->
