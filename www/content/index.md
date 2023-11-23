<div role="presentation" id="homepage-intro-outer">
<div role="presentation" id="homepage-intro-box">
<h1 id="homepage-h1">Roc</h1>
<svg id="homepage-logo" aria-labelledby="logo-svg-title logo-svg-desc" width="240" height="240" viewBox="0 0 51 53" fill="none" xmlns="http://www.w3.org/2000/svg"><title id="logo-svg-title">The Roc logo</title><desc id="logo-svg-desc">A purple origami bird made of six triangles</desc><path d="M23.6751 22.7086L17.655 53L27.4527 45.2132L26.4673 39.3424L23.6751 22.7086Z" class="logo-dark"/><path d="M37.2438 19.0101L44.0315 26.3689L45 22L45.9665 16.6324L37.2438 19.0101Z" class="logo-light"/><path d="M23.8834 3.21052L0 0L23.6751 22.7086L23.8834 3.21052Z" class="logo-light"/><path d="M44.0315 26.3689L23.6751 22.7086L26.4673 39.3424L44.0315 26.3689Z" class="logo-light"/><path d="M50.5 22L45.9665 16.6324L45 22H50.5Z" class="logo-dark"/><path d="M23.6751 22.7086L44.0315 26.3689L37.2438 19.0101L23.8834 3.21052L23.6751 22.7086Z" class="logo-dark"/>
</svg>

<p id="homepage-tagline">A fast, friendly, functional language.</p>
<pre id="first-code-sample"><samp class="code-snippet">list <span class="kw">=</span> List<span class="punctuation section">.</span>map songs <span class="kw">\</span>song <span class="kw">-></span>
    <span class="string">"Artist: </span><span class="kw">\(</span>song<span class="punctuation section">.</span>artist<span class="kw">)</span><span class="string">"</span></samp></pre>
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
        <textarea aria-label="Input Roc code here, then press Enter to submit it to the REPL" rows="5" id="source-input" placeholder="Enter some Roc code here."></textarea>
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
    Stdout<span class="punctuation section">.</span>line <span class="literal">"Hello!"</span></samp></pre>
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
* File I/O and HTTP requests
* Pattern matching for error handling
* JSON deserialization via type inference
* Common syntax sugar: string interpolation, pipelines, and backpassing

The [tutorial](/tutorial) introduces these gradually and in more depth, but this gives a brief overview.

<!-- THIS COMMENT WILL BE REPLACED BY THE LARGER EXAMPLE -->

## Sponsors

We are very grateful for our corporate sponsors [Vendr](https://www.vendr.com/), [RWX](https://www.rwx.com), and [Tweede golf](https://tweedegolf.nl/en):

<p id="sponsor-logos" aria-hidden="true"> <!-- aria-hidden because for screen readers this whole section is redundant with the preceding paragraph -->
    <a href="https://www.vendr.com"><svg class="logo-vendr" viewBox="0 0 193 48" fill="none" xmlns="http://www.w3.org/2000/svg">
    <path fill-rule="evenodd" clip-rule="evenodd" d="M14.8416 2.2909L29.9888 29.8607C31.2662 32.1859 31.4443 35.0285 30.4685 37.5183L26.5642 47.4807C26.3947 47.9134 25.9325 48.1156 25.532 47.9325C25.3756 47.861 25.2448 47.7367 25.1591 47.5784L0.107714 1.27994C-0.111718 0.874299 0.014776 0.353305 0.390264 0.1162C0.510862 0.0401174 0.64801 0 0.787671 0H11.1167C12.6456 0 14.0613 0.870673 14.8416 2.2909ZM38.3473 0.0154282C43.0052 0.0154282 43.0052 4.62745 42.2289 7.1431L41.9584 7.94976C41.2878 9.84288 39.8402 13.4148 37.6154 18.6656C37.5487 18.823 37.4392 18.9547 37.3014 19.043C36.9689 19.2562 36.5503 19.1748 36.3053 18.8704L36.2222 18.7459L26.6326 1.30039C26.5604 1.16897 26.5223 1.01894 26.5223 0.866095C26.5223 0.396285 26.8749 0.0154282 27.3098 0.0154282H38.3473Z" fill="#5E2DF5"/>
    <path d="M76.2416 42.456L65.3662 18.4911H71.4247L79.7178 38.0125H76.1423L84.4851 18.4911H90.2953L79.4198 42.456H76.2416Z" class="svg-text"/>
    <path d="M103.81 42.9552C101.426 42.9552 99.274 42.4227 97.3539 41.3576C95.4668 40.2592 93.9605 38.7614 92.8349 36.8642C91.7424 34.9669 91.1961 32.8367 91.1961 30.4735C91.1961 28.1103 91.7424 25.9967 92.8349 24.1328C93.9274 22.2356 95.4006 20.7378 97.2546 19.6394C99.1416 18.541 101.227 17.9918 103.512 17.9918C105.73 17.9918 107.683 18.5077 109.371 19.5395C111.093 20.5713 112.434 21.9859 113.394 23.7833C114.387 25.5807 114.884 27.6277 114.884 29.9243C114.884 30.3237 114.851 30.7398 114.784 31.1725C114.751 31.5719 114.685 32.0212 114.586 32.5205H95.0199V28.0271H111.755L109.719 29.8245C109.653 28.3599 109.371 27.1284 108.875 26.1299C108.378 25.1313 107.666 24.3658 106.74 23.8332C105.846 23.3007 104.737 23.0344 103.412 23.0344C102.022 23.0344 100.813 23.334 99.7872 23.9331C98.7609 24.5322 97.9663 25.381 97.4035 26.4794C96.8407 27.5445 96.5593 28.8259 96.5593 30.3237C96.5593 31.8215 96.8573 33.1363 97.4532 34.268C98.0491 35.3996 98.8933 36.2817 99.9858 36.9141C101.078 37.5132 102.336 37.8128 103.76 37.8128C104.985 37.8128 106.11 37.5964 107.137 37.1637C108.196 36.731 109.09 36.0986 109.818 35.2665L113.295 38.8113C112.136 40.176 110.729 41.2078 109.074 41.9068C107.418 42.6057 105.664 42.9552 103.81 42.9552Z" class="svg-text"/>
    <path d="M135.601 42.456V28.6262C135.601 27.0286 135.087 25.7138 134.061 24.682C133.068 23.6502 131.777 23.1343 130.188 23.1343C129.128 23.1343 128.185 23.3673 127.357 23.8332C126.529 24.2992 125.884 24.9483 125.42 25.7804C124.957 26.6125 124.725 27.5611 124.725 28.6262L122.59 27.428C122.59 25.5973 122.987 23.983 123.782 22.5851C124.576 21.1538 125.669 20.0388 127.059 19.24C128.483 18.4078 130.072 17.9918 131.826 17.9918C133.614 17.9918 135.203 18.4578 136.594 19.3897C137.984 20.2884 139.077 21.47 139.871 22.9346C140.666 24.3658 141.063 25.8636 141.063 27.428V42.456H135.601ZM119.263 42.456V18.4911H124.725V42.456H119.263Z" class="svg-text"/>
    <path d="M156.596 42.9552C154.444 42.9552 152.49 42.406 150.736 41.3077C149.014 40.2093 147.64 38.7281 146.614 36.8642C145.621 34.9669 145.124 32.8534 145.124 30.5234C145.124 28.1602 145.621 26.0467 146.614 24.1827C147.64 22.2855 149.014 20.7877 150.736 19.6893C152.49 18.5576 154.444 17.9918 156.596 17.9918C158.416 17.9918 160.022 18.3912 161.413 19.19C162.836 19.9556 163.962 21.0207 164.789 22.3854C165.617 23.75 166.031 25.2978 166.031 27.0286V33.9185C166.031 35.6493 165.617 37.197 164.789 38.5617C163.995 39.9263 162.886 41.0081 161.462 41.8069C160.039 42.5725 158.416 42.9552 156.596 42.9552ZM157.49 37.7628C158.847 37.7628 160.022 37.4633 161.015 36.8642C162.042 36.2317 162.836 35.3663 163.399 34.268C163.962 33.1696 164.243 31.9048 164.243 30.4735C164.243 29.0423 163.962 27.7775 163.399 26.6791C162.836 25.5807 162.042 24.7319 161.015 24.1328C160.022 23.5004 158.863 23.1842 157.539 23.1842C156.182 23.1842 154.99 23.5004 153.964 24.1328C152.97 24.7319 152.176 25.5807 151.58 26.6791C151.017 27.7775 150.736 29.0423 150.736 30.4735C150.736 31.9048 151.017 33.1696 151.58 34.268C152.176 35.3663 152.97 36.2317 153.964 36.8642C154.99 37.4633 156.165 37.7628 157.49 37.7628ZM169.358 42.456H163.896V36.0154L164.839 30.174L163.896 24.3824V6.50861H169.358V42.456Z" class="svg-text"/>
    <path d="M175.42 42.456V18.4911H180.883V42.456H175.42ZM180.883 29.1255L178.996 28.1769C178.996 25.148 179.658 22.7016 180.982 20.8376C182.34 18.9404 184.376 17.9918 187.09 17.9918C188.282 17.9918 189.358 18.2081 190.318 18.6408C191.278 19.0735 192.172 19.7725 193 20.7378L189.424 24.4324C188.994 23.9664 188.514 23.6335 187.984 23.4338C187.455 23.2341 186.842 23.1343 186.147 23.1343C184.624 23.1343 183.366 23.6169 182.373 24.5821C181.38 25.5474 180.883 27.0618 180.883 29.1255Z" class="svg-text"/>
    </svg></a>
    <a href="https://www.rwx.com"><svg class="logo-rwx" viewBox="0 0 166 70" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M 104 55 L 89 55 L 103 35 L 89 15 L 104 15 L 118 35 L 104 55 Z" fill="#10B981"/>
      <path d="M 119.181 30.973 C 119.579 31.542 120.421 31.542 120.819 30.973 L 130.899 16.574 C 131.363 15.911 130.888 15 130.079 15 L 119.083 15 C 117.777 15 116.554 15.637 115.806 16.706 L 112.901 20.855 C 112.66 21.2 112.66 21.658 112.901 22.002 L 119.181 30.973 Z" class="svg-text"/>
      <path d="M 119.181 39.028 C 119.579 38.459 120.421 38.459 120.819 39.028 L 130.899 53.427 C 131.363 54.09 130.888 55 130.079 55 L 119.083 55 C 117.777 55 116.554 54.363 115.806 53.294 L 112.901 49.145 C 112.66 48.801 112.66 48.342 112.901 47.998 L 119.181 39.028 Z" class="svg-text"/>
      <path d="M 39.152 52.057 C 39.628 53.795 41.208 55 43.01 55 L 47.338 55 C 49.135 55 50.712 53.801 51.192 52.069 L 55.944 34.935 C 56.214 33.957 57.6 33.957 57.871 34.935 L 62.622 52.069 C 63.102 53.801 64.679 55 66.477 55 L 70.818 55 C 72.614 55 74.189 53.804 74.671 52.074 L 85 15 L 78.502 15 C 76.667 15 75.068 16.248 74.622 18.028 L 68.359 43.03 L 60.923 15.737 C 60.804 15.302 60.409 15 59.958 15 L 54.037 15 C 53.589 15 53.195 15.299 53.074 15.731 L 45.455 43.03 L 39.364 18.053 C 38.927 16.261 37.322 15 35.478 15 L 29 15 L 39.152 52.057 Z" class="svg-text"/>
      <path d="M 4 15 C 1.791 15 0 16.791 0 19 L 0 54.978 L 9.194 54.978 L 9.194 28.2 C 9.194 25.991 10.985 24.2 13.194 24.2 L 25.759 24.2 C 26.397 24.2 26.872 23.611 26.736 22.988 L 25.171 15.788 C 25.071 15.328 24.665 15 24.194 15 L 4 15 Z" class="svg-text"/>
      <path d="M 136 47 L 166 47 L 166 55 L 136 55 L 136 47 Z" fill="#10B981"/>
    </svg></a>
    <a href="https://tweedegolf.nl/en"><svg class="logo-tweede-golf" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 175.12 40.4"><path class="svg-text" d="M54.42,27.74a4.55,4.55,0,0,1-.73.27,5.47,5.47,0,0,1-1.34.1A3,3,0,0,1,49.83,27a4.44,4.44,0,0,1-.86-2.9V17.26H47.54V14.12H49V11.57l3.11-1.34v3.89h2.36v3.14H52.08v6.48a1.17,1.17,0,0,0,.32.94,1.28,1.28,0,0,0,.89.26,2.15,2.15,0,0,0,.83-.16,2.88,2.88,0,0,0,.78-.45Z"></path><path class="svg-text" d="M59.23,27.88l-3.6-13.75H59l2,8.46,2-8.46h3.27l2,8.46,2-8.46h3.39L69.81,27.88H66.48L64.57,20.6l-2,7.28Z"></path><path class="svg-text" d="M77.7,22.8a2.34,2.34,0,0,0,.35.92,3,3,0,0,0,.62.67,1.87,1.87,0,0,0,.78.4,2.25,2.25,0,0,0,.86.13,3.07,3.07,0,0,0,1.12-.19,2.45,2.45,0,0,0,.89-.77l2.15,2a4.88,4.88,0,0,1-4.24,2.15A5.12,5.12,0,0,1,76,26.37a8,8,0,0,1-1.48-5.15A10.08,10.08,0,0,1,75,18.13a7.38,7.38,0,0,1,1.16-2.31,4.6,4.6,0,0,1,1.82-1.42,4.88,4.88,0,0,1,2.34-.51,5.94,5.94,0,0,1,2.2.43,4.59,4.59,0,0,1,1.72,1.31,6.07,6.07,0,0,1,1.1,2.18,10.35,10.35,0,0,1,.4,3.08c0,.57,0,1,0,1.27s-.06.51-.08.64ZM80.25,17a2.09,2.09,0,0,0-1.72.78,3.3,3.3,0,0,0-.83,2h4.83a4,4,0,0,0-.75-2A1.79,1.79,0,0,0,80.25,17Z"></path><path class="svg-text" d="M91.14,22.8a2.34,2.34,0,0,0,.35.92,3,3,0,0,0,.62.67,2.71,2.71,0,0,0,1.63.53,3.09,3.09,0,0,0,1.13-.19,2.85,2.85,0,0,0,.92-.77l2.12,2a4.88,4.88,0,0,1-4.24,2.15,5.12,5.12,0,0,1-4.22-1.75A8,8,0,0,1,88,21.22a10.08,10.08,0,0,1,.43-3.09,7.38,7.38,0,0,1,1.16-2.31,4.6,4.6,0,0,1,1.82-1.42,4.86,4.86,0,0,1,2.33-.51,5.58,5.58,0,0,1,2.21.43,4.56,4.56,0,0,1,1.71,1.31,5.76,5.76,0,0,1,1.1,2.18,9.74,9.74,0,0,1,.4,3.08c0,.57,0,1,0,1.27a2.8,2.8,0,0,1-.08.64ZM93.69,17a2.09,2.09,0,0,0-1.72.78,3.3,3.3,0,0,0-.83,2H96a4,4,0,0,0-.75-2A1.8,1.8,0,0,0,93.69,17Z"></path><path class="svg-text" d="M109.28,27a7.32,7.32,0,0,1-1.13.78,3.3,3.3,0,0,1-1.74.37,4.57,4.57,0,0,1-2-.48,5.12,5.12,0,0,1-1.58-1.42,6.71,6.71,0,0,1-1-2.2,10.77,10.77,0,0,1-.38-2.82,11.22,11.22,0,0,1,.38-2.88,6.39,6.39,0,0,1,1-2.31,5,5,0,0,1,1.64-1.55,4.34,4.34,0,0,1,2.17-.57,6.29,6.29,0,0,1,1.56.19,4.23,4.23,0,0,1,1.1.56V11l3.17-1.37V27.84h-3.17V27Zm0-7.9a2.55,2.55,0,0,0-.86-1.21,1.85,1.85,0,0,0-1.29-.48,2.13,2.13,0,0,0-2,1,5.73,5.73,0,0,0-.56,2.82,4.52,4.52,0,0,0,.64,2.66,2.16,2.16,0,0,0,1.86,1,2.13,2.13,0,0,0,1.42-.54,3.35,3.35,0,0,0,.78-1.24Z"></path><path class="svg-text" d="M118.34,22.8a2.34,2.34,0,0,0,.35.92,3,3,0,0,0,.62.67,2.22,2.22,0,0,0,.78.4,2.61,2.61,0,0,0,.88.13,2.93,2.93,0,0,0,1.1-.19A2.85,2.85,0,0,0,123,24l2.12,2a4.89,4.89,0,0,1-4.25,2.15,5.11,5.11,0,0,1-4.21-1.75,8,8,0,0,1-1.48-5.15,11.09,11.09,0,0,1,.43-3.09,7.38,7.38,0,0,1,1.16-2.31,4.68,4.68,0,0,1,1.82-1.42,4.88,4.88,0,0,1,2.34-.51,5.57,5.57,0,0,1,2.2.43,4.52,4.52,0,0,1,1.72,1.31,5.91,5.91,0,0,1,1.1,2.18,10,10,0,0,1,.4,3.08c0,.57,0,1,0,1.27a2.8,2.8,0,0,1-.08.64ZM120.89,17a2.09,2.09,0,0,0-1.72.78,3.53,3.53,0,0,0-.83,2h4.83a4,4,0,0,0-.75-2,1.8,1.8,0,0,0-1.53-.78Z"></path><path class="svg-text" d="M140.58,30.34a3.86,3.86,0,0,0,2-.91,3.12,3.12,0,0,0,1-2,2.39,2.39,0,0,1-.91.43,6.71,6.71,0,0,1-1.48.16,4.55,4.55,0,0,1-2.34-.56,4.48,4.48,0,0,1-1.61-1.54,8,8,0,0,1-.94-2.3,12.59,12.59,0,0,1-.32-2.8,8.13,8.13,0,0,1,.43-2.68,7.21,7.21,0,0,1,1.1-2.2,5.86,5.86,0,0,1,1.66-1.48,4.55,4.55,0,0,1,2.1-.54,4,4,0,0,1,2.39.81v-.59h3.14V26.63a7.73,7.73,0,0,1-.35,2.39,7.07,7.07,0,0,1-1.1,2.15,6.16,6.16,0,0,1-1.77,1.64,6.55,6.55,0,0,1-2.39.83Zm3-11.84a1.55,1.55,0,0,0-.7-1.05,1.89,1.89,0,0,0-1.1-.32,2.27,2.27,0,0,0-2,.94,4.94,4.94,0,0,0-.62,2.79,6.65,6.65,0,0,0,.56,3,1.88,1.88,0,0,0,1.8,1,2.08,2.08,0,0,0,2.07-1.13Z"></path><path class="svg-text" d="M155.17,28.12a4.45,4.45,0,0,1-2.29-.54,5.56,5.56,0,0,1-1.77-1.5A7.16,7.16,0,0,1,150,23.8a9.81,9.81,0,0,1,0-5.58A7.06,7.06,0,0,1,151.11,16a5.56,5.56,0,0,1,1.77-1.5,4.92,4.92,0,0,1,4.57,0,5.23,5.23,0,0,1,1.74,1.5,7.13,7.13,0,0,1,1.16,2.26,9.81,9.81,0,0,1,0,5.58,7.23,7.23,0,0,1-1.16,2.28,5.45,5.45,0,0,1-1.74,1.5A4.44,4.44,0,0,1,155.17,28.12Zm0-3.25A2.09,2.09,0,0,0,157,23.8a6.59,6.59,0,0,0,0-5.59,2.12,2.12,0,0,0-2.89-.76h0a2.24,2.24,0,0,0-.75.75,6.38,6.38,0,0,0,0,5.59,2.09,2.09,0,0,0,1.82,1.07Z"></path><path class="svg-text" d="M163.44,11l3.17-1.37V27.87h-3.17Z"></path><path class="svg-text" d="M172.81,17.26V27.87h-3.14V17.26h-1.29V14.12h1.29v-.4a4.57,4.57,0,0,1,1-3.22,3.7,3.7,0,0,1,2.87-1.08,4.1,4.1,0,0,1,.86.06,2.84,2.84,0,0,1,.7.16v2.9l-.56-.16h-.68a1.14,1.14,0,0,0-1,.32,1.63,1.63,0,0,0-.27,1.05v.4h2.47v3.14Z"></path><path class="svg-text" d="M20.26,0A20.2,20.2,0,1,0,40.4,20.26V20.2h0A20.18,20.18,0,0,0,20.26,0Zm-10,29.67H7.5c1.69-1.4,4.16-4.38,5.19-9.85,1.18-6.27,5.82-9.67,10.1-9.1h0c-3.62,1-6.39,4.47-7.25,9.07-1.07,5.66-3.65,8.49-5.1,9.7Zm.73,0c1.5-1.34,4-4.28,5-9.8.86-4.52,3.62-7.89,7.22-8.77a7.09,7.09,0,0,1,7.44,2.74,6.18,6.18,0,0,1,1.4,5.28,5.71,5.71,0,0,1-2.31,3.53,2.18,2.18,0,0,0,.14-.84,2.55,2.55,0,1,0-5.1-.14v.14S24.46,27.47,33,29.7ZM25.9,20.2h0a2.05,2.05,0,0,1,2.87.11h0a6.41,6.41,0,0,1-2.76,3A2.42,2.42,0,0,0,25.9,20.2Z"></path></svg></a>
</p>

If you would like your organization to become an official sponsor of Roc's development, please [DM Richard Feldman on Zulip](https://roc.zulipchat.com/#narrow/pm-with/281383-user281383)!

We'd also like to express our gratitude to our generous [individual sponsors](https://github.com/sponsors/roc-lang/)! A special thanks to those sponsoring $25/month or more:

<ul id="individual-sponsors">
    <li><a href="https://github.com/chris-packett">Chris Packett</a></li>
    <li><a href="https://github.com/jamesbirtles">James Birtles</a></li>
    <li><a href="https://github.com/Ivo-Balbaert">Ivo Balbaert</a></li>
    <li><a href="https://github.com/rvcas">Lucas Rosa</a></li>
    <li><a href="https://github.com/Ocupe">Jonas Schell</a></li>
    <li><a href="https://github.com/cdolan">Christopher Dolan</a></li>
    <li><a href="https://github.com/nickgravgaard">Nick Gravgaard</a></li>
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
