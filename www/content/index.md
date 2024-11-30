<div role="presentation" id="homepage-intro-outer">
<div role="presentation" id="homepage-intro-box">
<h1 id="homepage-h1">Roc</h1>
<svg id="homepage-logo" aria-labelledby="logo-svg-title logo-svg-desc" width="240" height="240" viewBox="0 0 51 53" fill="none" xmlns="http://www.w3.org/2000/svg"><title id="logo-svg-title">The Roc logo</title><desc id="logo-svg-desc">A purple origami bird made of six triangles</desc><path d="M23.6751 22.7086L17.655 53L27.4527 45.2132L26.4673 39.3424L23.6751 22.7086Z" class="logo-dark"/><path d="M37.2438 19.0101L44.0315 26.3689L45 22L45.9665 16.6324L37.2438 19.0101Z" class="logo-light"/><path d="M23.8834 3.21052L0 0L23.6751 22.7086L23.8834 3.21052Z" class="logo-light"/><path d="M44.0315 26.3689L23.6751 22.7086L26.4673 39.3424L44.0315 26.3689Z" class="logo-light"/><path d="M50.5 22L45.9665 16.6324L45 22H50.5Z" class="logo-dark"/><path d="M23.6751 22.7086L44.0315 26.3689L37.2438 19.0101L23.8834 3.21052L23.6751 22.7086Z" class="logo-dark"/>
</svg>

<p id="homepage-tagline">A fast, friendly, functional language.</p>
<pre id="first-code-sample"><samp class="code-snippet">credits <span class="kw">=</span> List<span class="punctuation section">.</span>map songs <span class="kw">\</span>song <span class="kw">-></span>
    <span class="string">"Performed by </span><span class="kw">$(</span>song<span class="punctuation section">.</span>artist<span class="kw">)</span><span class="string">"</span></samp></pre>
</div>
</div>

<section class="home-goals-container" aria-label="Roc's Design: Fast, Friendly, Functional">
    <div role="presentation" class="home-goals-column">
        <a href="/fast" class="home-goals-content">
            <h3 class="home-goals-title">Fast</h3>
            <p class="home-goals-description">Roc code is designed to build fast and <span class="nowrap">run fast</span>. It compiles to machine code or WebAssembly.</p>
            <p class="home-goals-learn-more">What does <i>fast</i> mean here?</p>
        </a>
    </div>
    <div role="presentation" class="home-goals-column">
        <a href="/friendly" class="home-goals-content">
            <h3 class="home-goals-title">Friendly</h3>
            <p class="home-goals-description">Roc's syntax, semantics, and included toolset all prioritize user-friendliness.</p>
            <p class="home-goals-learn-more">What does <i>friendly</i> mean here?</p>
        </a>
    </div>
    <div role="presentation" class="home-goals-column">
        <a href="/functional" class="home-goals-content">
            <h3 class="home-goals-title">Functional</h3>
            <p class="home-goals-description">
             Roc has a small number of simple language primitives. It's a single-paradigm <span class="nowrap">functional language.</span></p>
            <p class="home-goals-learn-more">What does <i>functional</i> mean here?</p>
        </a>
    </div>
</section>

<section id="try-roc">
<h2><a href="#try-roc">Try Roc</a></h2>

<div id="homepage-repl-container" role="presentation">
    <div id="repl-description" role="presentation">
        <p>You can try Roc using this read-eval-print loop (<a href="https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop">REPL</a>), which is running in your browser in <a href="https://webassembly.org">WebAssembly</a>.</p>
        <p><code>Shift-Enter</code> adds a newline.</p>
        <p>Try entering <code>0.1 + 0.2</code>
        <svg id="repl-arrow" role="presentation" width="100" height="50" viewBox="0 0 100 50" xmlns="http://www.w3.org/2000/svg">
          <polygon points="70,20 30,20 30,15 0,25 30,35 30,30 70,30"/>
        </svg>
        </p>
    </div>
    <div id="repl" role="presentation">
        <code class="history">
          <div id="repl-intro-text">Enter an expression to evaluate, or a definition (like <span class="color-blue">x = 1</span>) to use later.</div>
          <div id="history-text" aria-live="polite"></div>
        </code>
        <div id="repl-prompt" role="presentation">»</div>
        <textarea aria-label="Input Roc code here, then press Enter to submit it to the REPL" rows="5" id="source-input" placeholder="Enter some Roc code here." spellcheck="false"></textarea>
    </div>
</div>
<script type="module" src="/site.js"></script>
</section>

## [Examples](#examples) {#examples}

Roc is a young language. It doesn't even have a numbered release yet, just nightly builds!

However, it can already be used for several things if you're up for being an early adopter—<br>
with all the bugs and missing features which come with that territory.

Here are some examples of how it can be used today.

<div role="presentation" class="home-examples-container">
    <div role="presentation" class="home-examples-column">
        <h3 class="home-examples-title">Command-Line Interfaces</h3>
    <pre><samp class="code-snippet">main <span class="kw">=</span>
    Stdout<span class="punctuation section">.</span>line<span class="punctuation section">!</span> <span class="literal">"Hello!"</span></samp></pre>
        <p>You can use Roc to create scripts and command-line interfaces (CLIs). The compiler produces binary executables, so Roc programs can run on devices that don't have Roc itself installed.</p>
        <p>As an example, the HTML for this website is generated using a simple Roc script. You can see <a href="https://github.com/roc-lang/roc/blob/main/www/main.roc">the code for it</a> in the main Roc code repository.</p>
        <p>If you’re looking for a starting point for building a command-line program in Roc, <a href="https://github.com/roc-lang/basic-cli">basic-cli</a> is a popular <a href="/platforms">platform</a> to check out.</p>
    </div>
    <div role="presentation" class="home-examples-column">
        <h3 class="home-examples-title">Web Servers</h3>
<pre><samp class="code-snippet">handleReq <span class="kw">=</span> <span class="kw">\</span>request <span class="kw">-&gt;</span>
    Task<span class="punctuation section">.</span>ok <span class="literal">{</span> body: <span class="comment">…</span> <span class="literal">}</span></samp></pre>
        <p>You can also build web servers in Roc. <a href="https://github.com/roc-lang/basic-webserver">basic-webserver</a> is a <a href="/platforms">platform</a> with
        a simple interface: you write a function which takes a <code>Request</code>, does some I/O, and returns a <code>Response</code>.</p>
        <p>Behind the scenes, it uses Rust's high-performance <a href="https://docs.rs/hyper/latest/hyper/">hyper</a> and <a href="https://tokio.rs/">tokio</a> libraries to execute your Roc function on incoming requests.</p>
        <p>For database access, <a href="https://github.com/agu-z/roc-pg">roc-pg</a> lets you access a <a href="https://www.postgresql.org/">PostgreSQL</a> database&mdash;with your Roc types checked against the types in your database's schema.</p>
    </div>
    <div role="presentation" class="home-examples-column">
        <h3 class="home-examples-title">Embedding</h3>
        <pre><samp class="code-snippet">fn <span class="kw">=</span> require(<span class="string">"foo.roc"</span>)<span class="kw">;</span>
log(<span class="string">`Roc says </span><span class="kw">${</span>fn()<span class="kw">}</span><span class="string">`</span>)<span class="kw">;</span></samp></pre>
        <p>You can call Roc functions from other languages. There are several <a href="https://github.com/roc-lang/roc/tree/main/examples">basic examples</a> of how to call Roc functions from Python, Node.js, Swift, WebAssembly, and JVM languages.</p>
        <p>Any language that supports C interop can call Roc functions, using similar techniques to the ones found in these examples.</p>
        <p>Most of those are minimal proofs of concept, but <a href="https://github.com/vendrinc/roc-esbuild">roc-esbuild</a> is a work in progress that's used at <a href="https://www.vendr.com/careers">Vendr</a> to call Roc functions from Node.js.</p>
    </div>
</div>

### [Other Examples](#other-examples) {#other-examples}

You can find more use cases and examples on the [examples page](/examples)!

</section>

## [Code Sample with Explanations](#code-sample) {#code-sample}

Here's a code sample that shows a few different aspects of Roc:

- File I/O and HTTP requests
- Pattern matching for error handling
- JSON deserialization via type inference
- Common syntax sugar: pipelines, the `!` operator, and string interpolation

The [tutorial](/tutorial) introduces these gradually and in more depth, but this gives a brief overview.

<!-- THIS COMMENT WILL BE REPLACED BY THE LARGER EXAMPLE -->

## [Sponsors](#sponsors) {#sponsors}

We are very grateful for our corporate sponsors! They are: [Tweede golf](https://tweedegolf.nl/en), <a href="https://www.ohne-makler.net"><span class="nowrap">ohne-makler</span></a>, and [Decem](https://www.decem.com.au).

<p id="sponsor-logos" aria-hidden="true"> <!-- aria-hidden because for screen readers this whole section is redundant with the preceding paragraph -->
    <a href="https://tweedegolf.nl/en"><svg class="logo-tweede-golf" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 175.12 40.4"><path class="svg-text" d="M54.42,27.74a4.55,4.55,0,0,1-.73.27,5.47,5.47,0,0,1-1.34.1A3,3,0,0,1,49.83,27a4.44,4.44,0,0,1-.86-2.9V17.26H47.54V14.12H49V11.57l3.11-1.34v3.89h2.36v3.14H52.08v6.48a1.17,1.17,0,0,0,.32.94,1.28,1.28,0,0,0,.89.26,2.15,2.15,0,0,0,.83-.16,2.88,2.88,0,0,0,.78-.45Z"></path><path class="svg-text" d="M59.23,27.88l-3.6-13.75H59l2,8.46,2-8.46h3.27l2,8.46,2-8.46h3.39L69.81,27.88H66.48L64.57,20.6l-2,7.28Z"></path><path class="svg-text" d="M77.7,22.8a2.34,2.34,0,0,0,.35.92,3,3,0,0,0,.62.67,1.87,1.87,0,0,0,.78.4,2.25,2.25,0,0,0,.86.13,3.07,3.07,0,0,0,1.12-.19,2.45,2.45,0,0,0,.89-.77l2.15,2a4.88,4.88,0,0,1-4.24,2.15A5.12,5.12,0,0,1,76,26.37a8,8,0,0,1-1.48-5.15A10.08,10.08,0,0,1,75,18.13a7.38,7.38,0,0,1,1.16-2.31,4.6,4.6,0,0,1,1.82-1.42,4.88,4.88,0,0,1,2.34-.51,5.94,5.94,0,0,1,2.2.43,4.59,4.59,0,0,1,1.72,1.31,6.07,6.07,0,0,1,1.1,2.18,10.35,10.35,0,0,1,.4,3.08c0,.57,0,1,0,1.27s-.06.51-.08.64ZM80.25,17a2.09,2.09,0,0,0-1.72.78,3.3,3.3,0,0,0-.83,2h4.83a4,4,0,0,0-.75-2A1.79,1.79,0,0,0,80.25,17Z"></path><path class="svg-text" d="M91.14,22.8a2.34,2.34,0,0,0,.35.92,3,3,0,0,0,.62.67,2.71,2.71,0,0,0,1.63.53,3.09,3.09,0,0,0,1.13-.19,2.85,2.85,0,0,0,.92-.77l2.12,2a4.88,4.88,0,0,1-4.24,2.15,5.12,5.12,0,0,1-4.22-1.75A8,8,0,0,1,88,21.22a10.08,10.08,0,0,1,.43-3.09,7.38,7.38,0,0,1,1.16-2.31,4.6,4.6,0,0,1,1.82-1.42,4.86,4.86,0,0,1,2.33-.51,5.58,5.58,0,0,1,2.21.43,4.56,4.56,0,0,1,1.71,1.31,5.76,5.76,0,0,1,1.1,2.18,9.74,9.74,0,0,1,.4,3.08c0,.57,0,1,0,1.27a2.8,2.8,0,0,1-.08.64ZM93.69,17a2.09,2.09,0,0,0-1.72.78,3.3,3.3,0,0,0-.83,2H96a4,4,0,0,0-.75-2A1.8,1.8,0,0,0,93.69,17Z"></path><path class="svg-text" d="M109.28,27a7.32,7.32,0,0,1-1.13.78,3.3,3.3,0,0,1-1.74.37,4.57,4.57,0,0,1-2-.48,5.12,5.12,0,0,1-1.58-1.42,6.71,6.71,0,0,1-1-2.2,10.77,10.77,0,0,1-.38-2.82,11.22,11.22,0,0,1,.38-2.88,6.39,6.39,0,0,1,1-2.31,5,5,0,0,1,1.64-1.55,4.34,4.34,0,0,1,2.17-.57,6.29,6.29,0,0,1,1.56.19,4.23,4.23,0,0,1,1.1.56V11l3.17-1.37V27.84h-3.17V27Zm0-7.9a2.55,2.55,0,0,0-.86-1.21,1.85,1.85,0,0,0-1.29-.48,2.13,2.13,0,0,0-2,1,5.73,5.73,0,0,0-.56,2.82,4.52,4.52,0,0,0,.64,2.66,2.16,2.16,0,0,0,1.86,1,2.13,2.13,0,0,0,1.42-.54,3.35,3.35,0,0,0,.78-1.24Z"></path><path class="svg-text" d="M118.34,22.8a2.34,2.34,0,0,0,.35.92,3,3,0,0,0,.62.67,2.22,2.22,0,0,0,.78.4,2.61,2.61,0,0,0,.88.13,2.93,2.93,0,0,0,1.1-.19A2.85,2.85,0,0,0,123,24l2.12,2a4.89,4.89,0,0,1-4.25,2.15,5.11,5.11,0,0,1-4.21-1.75,8,8,0,0,1-1.48-5.15,11.09,11.09,0,0,1,.43-3.09,7.38,7.38,0,0,1,1.16-2.31,4.68,4.68,0,0,1,1.82-1.42,4.88,4.88,0,0,1,2.34-.51,5.57,5.57,0,0,1,2.2.43,4.52,4.52,0,0,1,1.72,1.31,5.91,5.91,0,0,1,1.1,2.18,10,10,0,0,1,.4,3.08c0,.57,0,1,0,1.27a2.8,2.8,0,0,1-.08.64ZM120.89,17a2.09,2.09,0,0,0-1.72.78,3.53,3.53,0,0,0-.83,2h4.83a4,4,0,0,0-.75-2,1.8,1.8,0,0,0-1.53-.78Z"></path><path class="svg-text" d="M140.58,30.34a3.86,3.86,0,0,0,2-.91,3.12,3.12,0,0,0,1-2,2.39,2.39,0,0,1-.91.43,6.71,6.71,0,0,1-1.48.16,4.55,4.55,0,0,1-2.34-.56,4.48,4.48,0,0,1-1.61-1.54,8,8,0,0,1-.94-2.3,12.59,12.59,0,0,1-.32-2.8,8.13,8.13,0,0,1,.43-2.68,7.21,7.21,0,0,1,1.1-2.2,5.86,5.86,0,0,1,1.66-1.48,4.55,4.55,0,0,1,2.1-.54,4,4,0,0,1,2.39.81v-.59h3.14V26.63a7.73,7.73,0,0,1-.35,2.39,7.07,7.07,0,0,1-1.1,2.15,6.16,6.16,0,0,1-1.77,1.64,6.55,6.55,0,0,1-2.39.83Zm3-11.84a1.55,1.55,0,0,0-.7-1.05,1.89,1.89,0,0,0-1.1-.32,2.27,2.27,0,0,0-2,.94,4.94,4.94,0,0,0-.62,2.79,6.65,6.65,0,0,0,.56,3,1.88,1.88,0,0,0,1.8,1,2.08,2.08,0,0,0,2.07-1.13Z"></path><path class="svg-text" d="M155.17,28.12a4.45,4.45,0,0,1-2.29-.54,5.56,5.56,0,0,1-1.77-1.5A7.16,7.16,0,0,1,150,23.8a9.81,9.81,0,0,1,0-5.58A7.06,7.06,0,0,1,151.11,16a5.56,5.56,0,0,1,1.77-1.5,4.92,4.92,0,0,1,4.57,0,5.23,5.23,0,0,1,1.74,1.5,7.13,7.13,0,0,1,1.16,2.26,9.81,9.81,0,0,1,0,5.58,7.23,7.23,0,0,1-1.16,2.28,5.45,5.45,0,0,1-1.74,1.5A4.44,4.44,0,0,1,155.17,28.12Zm0-3.25A2.09,2.09,0,0,0,157,23.8a6.59,6.59,0,0,0,0-5.59,2.12,2.12,0,0,0-2.89-.76h0a2.24,2.24,0,0,0-.75.75,6.38,6.38,0,0,0,0,5.59,2.09,2.09,0,0,0,1.82,1.07Z"></path><path class="svg-text" d="M163.44,11l3.17-1.37V27.87h-3.17Z"></path><path class="svg-text" d="M172.81,17.26V27.87h-3.14V17.26h-1.29V14.12h1.29v-.4a4.57,4.57,0,0,1,1-3.22,3.7,3.7,0,0,1,2.87-1.08,4.1,4.1,0,0,1,.86.06,2.84,2.84,0,0,1,.7.16v2.9l-.56-.16h-.68a1.14,1.14,0,0,0-1,.32,1.63,1.63,0,0,0-.27,1.05v.4h2.47v3.14Z"></path><path class="svg-text" d="M20.26,0A20.2,20.2,0,1,0,40.4,20.26V20.2h0A20.18,20.18,0,0,0,20.26,0Zm-10,29.67H7.5c1.69-1.4,4.16-4.38,5.19-9.85,1.18-6.27,5.82-9.67,10.1-9.1h0c-3.62,1-6.39,4.47-7.25,9.07-1.07,5.66-3.65,8.49-5.1,9.7Zm.73,0c1.5-1.34,4-4.28,5-9.8.86-4.52,3.62-7.89,7.22-8.77a7.09,7.09,0,0,1,7.44,2.74,6.18,6.18,0,0,1,1.4,5.28,5.71,5.71,0,0,1-2.31,3.53,2.18,2.18,0,0,0,.14-.84,2.55,2.55,0,1,0-5.1-.14v.14S24.46,27.47,33,29.7ZM25.9,20.2h0a2.05,2.05,0,0,1,2.87.11h0a6.41,6.41,0,0,1-2.76,3A2.42,2.42,0,0,0,25.9,20.2Z"></path></svg></a>
    <a href="https://www.ohne-makler.net"><svg class="ohne-makler-logo" xmlns="http://www.w3.org/2000/svg" width="202" height="64" fill="none"><path fill="#236BE9" d="M147.206 38.4c-.312 1.722-.324 3.96-.324 6.653v7.89l6.228-5.388-1.296-.352v-1.161c1.261.07 2.557.088 3.8.088 1.008 0 2.034-.036 3.024-.088v1.161l-1.584.352c-2.395 1.584-4.699 3.434-6.932 5.3 2.16 2.342 6.736 7.219 8.931 9.244l.896.243 1.48-.278c.304-1.685.323-3.862.325-6.48v-8.3c-.002-2.598-.021-4.774-.325-6.471-.683-.123-1.423-.283-2.16-.407v-1.161c1.224-.07 3.798-.51 5.131-.845h1.674c-.304 1.683-.322 3.862-.324 6.478v10.75c0 2.596.021 4.772.324 6.471l1.872.352v1.162c-1.386-.035-2.808-.088-4.194-.088-1.123 0-2.272.046-3.403.073v.015c-2.413 0-3.962.123-5.258.387l-2.448-2.87-2.934-3.274-2.845-3.187v.8c0 2.675.013 4.912.323 6.648l1.872.352v1.161c-1.385-.035-2.808-.088-4.193-.088-1.388 0-2.81.07-4.196.088l.018-1.216 1.872-.352c.304-1.683.323-3.86.325-6.476v-8.304c-.002-2.596-.021-4.772-.325-6.47-.683-.122-1.422-.282-2.16-.406v-1.161c1.224-.07 3.798-.51 5.131-.845h1.675ZM9.614 45.654c6.572 0 9.616 4.261 9.616 9.136 0 3.999-2.648 9.175-9.616 9.175C3.637 63.965 0 60.514 0 54.792c0-5.264 4.123-9.138 9.614-9.138Zm60.244 0c3.456 0 7.417 1.99 7.417 6.901v.97H64.71c0 4.982 1.872 7.605 5.707 7.605 2.288 0 4.16-1.285 5.869-2.94l1.315 1.197c-1.62 1.92-4.357 4.578-8.318 4.578-6.77 0-9.093-4.754-9.093-8.416 0-6.656 5.006-9.895 9.669-9.895Zm108.369 0c3.456 0 7.418 1.99 7.418 6.901v.97h-12.567c0 4.982 1.872 7.605 5.708 7.605 2.288 0 4.16-1.285 5.87-2.94l1.314 1.197c-1.621 1.92-4.357 4.578-8.319 4.578-6.768 0-9.091-4.754-9.091-8.416 0-6.656 5.005-9.895 9.667-9.895Zm-47.099-.017c6.229 0 7.13 2.71 7.13 6.267 0 .8.105 3.187.22 5.682l.01.232.016.345.011.23c.048 1.079.098 2.148.138 3.089l1.765.916-.018 1.18a53.341 53.341 0 0 0-2.845-.088c-.864 0-1.747.035-2.611.088l-.323-3.839h-.072c-1.099 1.76-2.611 4.226-7.023 4.226-2.07 0-5.76-.95-5.76-4.754 0-5.352 7.021-6.656 12.476-6.656v-1.76c0-1.955-.736-2.87-3.637-2.87-2.322 0-3.637.528-4.736.968v1.726l-1.187.317a20.165 20.165 0 0 0-1.711-3.82c2.071-.67 4.573-1.48 8.157-1.48Zm-33.723.016-.234 3.029.072.035.058-.061.118-.123c1.475-1.528 3.223-2.88 5.819-2.88 2.756 0 3.856 1.456 4.608 3.115l.048.107.026.055c1.386-1.727 3.456-3.276 6.067-3.276 3.781 0 5.312 2.341 5.312 5.616v3.821c0 2.835 0 5.194.323 7.008l1.872.352v1.162a71.04 71.04 0 0 0-2.513-.08l-.176-.003-.768-.005a45.22 45.22 0 0 0-2.896.088c.07-1.392.16-2.941.16-4.947v-3.874c0-4.63-.736-6.269-3.186-6.269-3.205-.016-3.835 3.082-3.835 6.586 0 2.835 0 5.193.323 7.008l1.872.352v1.161c-1.385-.035-2.808-.088-4.193-.088-1.388 0-2.81.07-4.196.088v-1.179l1.872-.352c.301-1.669.324-3.822.325-6.645v-.662c0-4.63-.739-6.269-3.187-6.269-3.187 0-3.835 3.082-3.835 6.586 0 2.835 0 5.193.325 7.008l1.872.352v1.161c-1.386-.035-2.81-.088-4.196-.088-1.387 0-2.808.07-4.195.088V62.47l1.872-.352c.304-1.685.323-3.863.325-6.48v-2.571a37.87 37.87 0 0 0-.325-4.948c-.683-.123-1.422-.281-2.16-.404l-.017-1.215c1.224-.07 3.798-.512 5.13-.845h1.513v-.001Zm-51.168 0-.235 3.029.072.035c1.512-1.6 3.296-3.063 5.995-3.063 3.78 0 5.312 2.341 5.312 5.616v4.359c0 2.595.02 4.771.325 6.47l1.872.352v1.162a70.24 70.24 0 0 0-2.253-.075l-.176-.004-.175-.003a45.32 45.32 0 0 0-3.753.08c.054-1.389.163-2.939.163-4.945v-3.874c0-4.63-.74-6.269-3.187-6.269-3.312 0-4.123 3.082-4.123 6.586v.537c.001 2.597.02 4.772.324 6.47l1.872.353v1.161c-1.385-.035-2.808-.088-4.195-.088-1.385 0-2.808.07-4.195.088v-1.216l1.872-.352c.312-1.72.325-3.958.325-6.652v-2.396c0-1.656-.109-3.312-.325-4.948-.683-.124-1.422-.282-2.16-.405v-1.162c1.224-.07 3.798-.512 5.131-.845h1.512l.002-.001ZM26.339 38.4c-.297 1.646-.321 3.766-.323 6.304v4.013c1.512-1.6 3.294-3.063 5.995-3.063 3.781 0 5.312 2.341 5.312 5.616v3.821c0 2.835 0 5.194.323 7.008l1.872.352v1.162a75.87 75.87 0 0 0-3.456-.088c-1.099 0-2.09.035-2.899.088.072-1.392.162-2.941.162-4.947v-3.874c0-4.63-.738-6.269-3.187-6.269-3.312 0-4.124 3.082-4.124 6.586 0 2.835 0 5.193.325 7.008l1.872.352v1.161c-1.385-.035-2.808-.088-4.195-.088-1.386 0-2.808.07-4.195.088v-1.216l1.872-.352c.304-1.683.323-3.86.325-6.464v-8.315c0-2.597-.021-4.773-.324-6.47-.684-.123-1.424-.283-2.16-.407v-1.161c1.224-.07 3.799-.51 5.13-.845h1.675Zm166.237 7.272c-.162 1.392-.235 2.957-.36 4.613l.053.053c.649-1.584 2.521-4.666 5.87-4.666.72 0 1.44.158 2.088.475a25.95 25.95 0 0 0-1.584 4.42l-1.189-.194v-1.48a1.586 1.586 0 0 0-.737-.192c-1.189 0-3.997 1.161-3.997 7.253 0 2.078.126 4.192.251 6.144.811.124 1.584.23 2.395.352v1.163a150.74 150.74 0 0 0-4.896-.088c-1.712 0-3.187.07-3.926.088v-1.197l1.872-.352c.304-1.685.323-3.862.325-6.464v-2.57c0-1.654-.107-3.308-.325-4.947-.683-.123-1.422-.281-2.16-.405v-1.161c1.224-.07 3.798-.512 5.131-.845h1.189Zm-182.98 2.06c-3.924 0-5.294 3.24-5.294 7.06 0 3.997 1.837 7.096 5.295 7.096 2.971-.018 5.293-2.307 5.293-7.096 0-4.192-2.088-7.06-5.293-7.06Zm124.665 6.708h-.973c-2.358 0-7.184.915-7.184 4.138 0 1.284.973 2.552 3.098 2.552 3.006 0 4.896-2.552 5.059-6.69Zm-44.795-1.109a10.554 10.554 0 0 0-.973 2.27c-1.8-.035-3.475-.087-5.184-.087-1.712 0-3.386.07-5.096.088.413-.722.736-1.479.971-2.272 1.8.036 3.475.09 5.186.09 1.71 0 3.385-.073 5.096-.089ZM69.768 47.54c-3.098 0-4.555 2.042-4.86 4.295l8.047-.388c0-1.672-.198-3.908-3.187-3.908v.001Zm108.37 0c-3.096 0-4.556 2.042-4.861 4.295l8.048-.388c0-1.672-.199-3.908-3.187-3.908v.001ZM39.194 14.474l.17.172.01-.008 18.304 18.565V20.371h6.138v16.064l-8.046-.019-16.56-16.795L25.875 33.13H20.8l18.394-18.656ZM73.12 20.37v16.064h-5.04V20.371h5.04Zm-6.48 0v16.064H65.2V20.371h1.44ZM66.182 0l.167.17.006-.007 33.962 34.427h-5.093L66.192 5.15l-15.87 16.096-2.544-2.582L66.18 0h.001Zm64.986 2.027c.685 0 1.368.056 2.053.165 6.982 1.131 11.734 7.776 10.617 14.86-.864 5.487-5.632 10.058-11.179 10.81v10.452h-3.6V27.802c-.673-.103-1.481-.218-2.133-.39a12.786 12.786 0 0 1-7.584 4.954V41.6h-3.6v-8.98a11.915 11.915 0 0 1-3.204-.657c-6.696-2.32-10.26-9.677-7.991-16.467 2.285-6.79 9.538-10.405 16.234-8.104l-.163-.054.003-.004c.046.016.093.032.141.052l.012.003c2.42-3.248 6.384-5.36 10.394-5.36v-.002Zm6.174 5.514c-4.067-3.45-10.304-2.72-13.705 1.406l.069-.083.009.006a9.607 9.607 0 0 0-1.573 2.666l-.048.128a10.001 10.001 0 0 0-2.91-1.384l-.005.014c-3.6-1.004-7.451.22-9.864 3.12-3.419 4.11-2.88 10.261 1.171 13.71 4.048 3.468 10.116 2.92 13.516-1.186l.004.003c.634-.818 1.248-1.864 1.6-2.815a9.506 9.506 0 0 0 2.914 1.37l-.082-.024c3.552.827 7.752-.408 10.092-3.222 3.401-4.127 2.88-10.26-1.188-13.71Z"/></svg></a>
    <a href="https://www.decem.com.au"><svg class="logo-decem" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 667.8 239.15"><defs><style>.cls-2{fill:url(#decem-logo-linear-gradient);}</style><linearGradient id="decem-logo-linear-gradient" x1="32.29" y1="10.24" x2="171.87" y2="203.9" gradientUnits="userSpaceOnUse"><stop offset="0" stop-color="#04021e"/><stop offset=".12" stop-color="#1d1c2d"/><stop offset=".25" stop-color="#323239"/><stop offset=".38" stop-color="#3f3f41"/><stop offset=".5" stop-color="#444"/><stop offset=".57" stop-color="#3e4859"/><stop offset=".67" stop-color="#374d74"/><stop offset=".78" stop-color="#325186"/><stop offset=".88" stop-color="#2f5392"/><stop offset="1" stop-color="#2f5496"/></linearGradient></defs><g id="Logos"><path class="cls-1" d="m256.31,43.83h24.59c20.56,0,34.94,4.86,43.14,14.59,8.19,9.73,12.29,21.3,12.29,34.73,0,9.36-1.76,17.99-5.28,25.91-3.52,7.92-9.33,14.38-17.44,19.38-8.1,5-18.73,7.5-31.88,7.5h-25.42V43.83Zm27.09,88.36c12.78,0,22.06-3.63,27.85-10.91,5.79-7.27,8.68-16.41,8.68-27.44s-2.92-19.5-8.75-26.26c-5.84-6.76-15.1-10.14-27.79-10.14h-11.53v74.74h11.53Z"/><path class="cls-1" d="m362.03,142.81c-5.09-3.01-8.85-7.04-11.25-12.09-2.41-5.05-3.61-10.58-3.61-16.6,0-6.76,1.41-12.69,4.24-17.78,2.82-5.09,6.74-9.01,11.74-11.74,5-2.73,10.65-4.1,16.95-4.1,9.82,0,17.11,2.71,21.88,8.13,4.77,5.42,7.15,13.04,7.15,22.85,0,2.5-.14,5.09-.42,7.78h-45.84c1.02,5.65,3.03,9.72,6.04,12.22,3.01,2.5,7.2,3.75,12.57,3.75,4.45,0,8.1-.37,10.97-1.11,2.87-.74,5.51-1.76,7.92-3.06l4.58,10.7c-2.31,1.48-5.46,2.78-9.45,3.89-3.98,1.11-8.8,1.67-14.44,1.67-7.6,0-13.94-1.5-19.03-4.51Zm32.51-33.97c.09-5.74-1.16-9.79-3.75-12.16-2.6-2.36-6.3-3.54-11.12-3.54-9.72,0-15.33,5.23-16.81,15.7h31.67Z"/><path class="cls-1" d="m437.53,143.22c-5.05-2.73-9.05-6.67-12.02-11.81-2.96-5.14-4.44-11.18-4.44-18.13,0-6.57,1.36-12.34,4.1-17.3,2.73-4.95,6.58-8.77,11.53-11.46,4.95-2.68,10.67-4.03,17.16-4.03,4.91,0,9.14.62,12.71,1.88,3.56,1.25,6.37,2.8,8.4,4.65l-6.67,12.09c-3.89-3.89-8.29-5.83-13.2-5.83-5.93,0-10.45,1.8-13.55,5.42-3.11,3.61-4.66,8.43-4.66,14.45,0,6.85,1.6,12.18,4.79,15.97,3.2,3.8,7.67,5.7,13.41,5.7,2.87,0,5.67-.42,8.4-1.25,2.73-.83,4.84-1.81,6.32-2.92l5.42,10.56c-2.04,1.58-4.89,2.99-8.55,4.24-3.66,1.25-7.71,1.87-12.15,1.87-6.3,0-11.97-1.37-17.02-4.1Z"/><path class="cls-1" d="m500.67,142.81c-5.09-3.01-8.85-7.04-11.25-12.09-2.41-5.05-3.61-10.58-3.61-16.6,0-6.76,1.41-12.69,4.24-17.78,2.82-5.09,6.73-9.01,11.74-11.74,5-2.73,10.65-4.1,16.95-4.1,9.81,0,17.11,2.71,21.88,8.13,4.77,5.42,7.15,13.04,7.15,22.85,0,2.5-.14,5.09-.42,7.78h-45.84c1.02,5.65,3.03,9.72,6.04,12.22,3.01,2.5,7.2,3.75,12.57,3.75,4.44,0,8.1-.37,10.97-1.11,2.87-.74,5.51-1.76,7.92-3.06l4.58,10.7c-2.31,1.48-5.47,2.78-9.45,3.89-3.98,1.11-8.8,1.67-14.45,1.67-7.59,0-13.94-1.5-19.03-4.51Zm32.51-33.97c.09-5.74-1.16-9.79-3.75-12.16-2.6-2.36-6.3-3.54-11.11-3.54-9.73,0-15.33,5.23-16.81,15.7h31.67Z"/><path class="cls-1" d="m563.46,82.17h15v11.53c2.04-4.35,5.03-7.64,8.96-9.86,3.94-2.22,8.45-3.33,13.55-3.33,11.4.09,18.39,4.03,20.98,11.81,2.04-3.61,5.09-6.48,9.17-8.61,4.08-2.13,8.38-3.2,12.92-3.2,6.67,0,11.71,1.27,15.14,3.82,3.43,2.55,5.72,6.16,6.88,10.84,1.16,4.68,1.74,10.95,1.74,18.82v31.95h-15.14v-32.09c0-7.5-.76-12.87-2.29-16.12-1.53-3.24-4.47-4.86-8.82-4.86-5.84,0-10.26,1.97-13.27,5.91-3.01,3.94-4.51,9.1-4.51,15.49v31.67h-15v-34.31c0-4.91-.19-8.56-.56-10.97-.37-2.41-1.39-4.31-3.06-5.69-1.67-1.39-4.36-2.08-8.06-2.08-3.25,0-6.28.97-9.1,2.92-2.83,1.94-5.09,4.56-6.8,7.85-1.72,3.29-2.57,6.83-2.57,10.63v31.67h-15.14v-63.77Z"/><path class="cls-2" d="m150.14,60.99c-15.5,0-28.1,12.59-28.1,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1-12.59-28.1-28.1-28.1Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48Zm0,10.41c-15.5,0-28.1,12.59-28.1,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1-12.59-28.1-28.1-28.1Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48Zm-60.99-111.56c-15.5,0-28.1,12.59-28.1,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1-12.59-28.1-28.1-28.1Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48Zm0,10.41c-15.5,0-28.1,12.59-28.1,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1-12.59-28.1-28.1-28.1Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48ZM28.17,60.99C12.66,60.99.07,73.58.07,89.08s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1-12.59-28.1-28.1-28.1Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48ZM150.07,0c-15.5,0-28.1,12.59-28.1,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1S165.58,0,150.07,0Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48ZM89.08,0c-15.5,0-28.1,12.59-28.1,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1S104.59,0,89.08,0Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48ZM28.1,0C12.59,0,0,12.59,0,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1S43.61,0,28.1,0Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48S15.7,5.62,28.1,5.62s22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48Zm.07,71.4C12.66,121.97.07,134.57.07,150.07s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1-12.59-28.1-28.1-28.1Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48Zm121.97,10.41c-15.5,0-28.1,12.59-28.1,28.1s12.59,28.1,28.1,28.1,28.1-12.59,28.1-28.1-12.59-28.1-28.1-28.1Zm0,50.58c-12.39,0-22.48-10.08-22.48-22.48s10.08-22.48,22.48-22.48,22.48,10.08,22.48,22.48-10.08,22.48-22.48,22.48Z"/></g></svg>
    </a>
</p>

If you would like your organization to become an official sponsor of Roc's development, please [DM Richard Feldman on Zulip](https://roc.zulipchat.com/#narrow/pm-with/281383-user281383)!

We'd also like to express our gratitude to our generous [individual sponsors](https://github.com/sponsors/roc-lang/)! A special thanks to those sponsoring $25/month or more:

<ul id="individual-sponsors">
    <li><a href="https://github.com/pmarreck">Peter Marreck</a></li>
    <li><a href="https://github.com/chiroptical">Barry Moore</a></li>
    <li>Eric Andresen</li>
    <li><a href="https://github.com/jluckyiv">Jackson Lucky</a></li>
    <li><a href="https://github.com/agu-z">Agus Zubiaga</a></li>
    <li><a href="https://github.com/AngeloChecked">Angelo Ceccato</a></li>
    <li><a href="https://github.com/noverby">Niclas Overby</a></li>
    <li><a href="https://github.com/krzysztofgb">Krzysztof G.</a></li>
    <li><a href="https://github.com/smores56">Sam Mohr</a></li>
    <li><a href="https://github.com/megakilo">Steven Chen</a></li>
    <li><a href="https://github.com/asteroidb612">Drew Lazzeri</a></li>
    <li><a href="https://github.com/mrmizz">Alex Binaei</a></li>
    <li><a href="https://github.com/jonomallanyk">Jono Mallanyk</a></li>
    <li><a href="https://github.com/chris-packett">Chris Packett</a></li>
    <li><a href="https://github.com/jamesbirtles">James Birtles</a></li>
    <li><a href="https://github.com/Ivo-Balbaert">Ivo Balbaert</a></li>
    <li><a href="https://github.com/rvcas">Lucas Rosa</a></li>
    <li><a href="https://github.com/Ocupe">Jonas Schell</a></li>
    <li><a href="https://github.com/cdolan">Christopher Dolan</a></li>
    <li><a href="https://github.com/nick-gravgaard">Nick Gravgaard</a></li>
    <li><a href="https://github.com/popara">Zeljko Nesic</a></li>
    <li><a href="https://github.com/shritesh">Shritesh Bhattarai</a></li>
    <li><a href="https://github.com/rtfeldman">Richard Feldman</a></li>
    <li><a href="https://github.com/ayazhafiz">Ayaz Hafiz</a></li>
</ul>

Thank you all for your contributions! Roc would not be what it is without your generosity. 💜

We are currently trying to raise $4,000 USD/month in donations to fund one longtime Roc contributor to continue his work on Roc full-time. We are a small group trying to do big things, and every donation helps! You can donate using:

- [GitHub Sponsors](https://github.com/sponsors/roc-lang)
- [Liberapay](https://liberapay.com/roc_lang)

All donations go through the [Roc Programming Language Foundation](https://foundation.roc-lang.org/), a registered <a href="https://en.wikipedia.org/wiki/501(c)(3)_organization">US <span class="nowrap">501(c)(3)</span> nonprofit organization</a>, which means these donations are tax-exempt in the US.
