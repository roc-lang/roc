<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=PT+Serif:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Merriweather:ital,wght@0,400;0,700;1,700&family=PT+Serif:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Merriweather+Sans:ital,wght@0,400;0,500;0,600;0,700;0,800;1,400;1,500;1,600;1,700;1,800&family=Merriweather:ital,wght@0,400;0,700;1,700&family=PT+Serif:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Merriweather+Sans:ital,wght@0,400;0,500;0,600;0,700;0,800;1,400;1,500;1,600;1,700;1,800&family=Merriweather:ital,wght@0,400;0,700;1,700&family=PT+Sans:ital,wght@0,400;0,700;1,400;1,700&family=PT+Serif:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,300;0,400;1,300;1,400&family=Merriweather+Sans:ital,wght@0,400;0,500;0,600;0,700;0,800;1,400;1,500;1,600;1,700;1,800&family=Merriweather:ital,wght@0,400;0,700;1,700&family=PT+Sans:ital,wght@0,400;0,700;1,400;1,700&family=PT+Serif:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,300;0,400;1,300;1,400&family=Merriweather+Sans:ital,wght@0,400;0,500;0,600;0,700;0,800;1,400;1,500;1,600;1,700;1,800&family=Merriweather:ital,wght@0,400;0,700;1,700&family=PT+Sans:ital,wght@0,400;0,700;1,400;1,700&family=PT+Serif:ital,wght@0,400;0,700;1,400;1,700&family=Ubuntu:ital,wght@0,400;0,500;0,700;1,300;1,400;1,500;1,700&display=swap" rel="stylesheet">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,400;0,500;0,600;0,700;0,800;0,900;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" rel="stylesheet">

# Roc

<p id="tagline">A fast, friendly, functional programming language.</p>

<img src="roc-rocket.png" width=36><p><b>Fast</b> - Roc code is designed to compile fast and run fast. It compiles to machine code or to <a href="https://webassembly.org/">WebAssembly</a>. Like <a href="https://rust-lang.org">Rust</a> and <a href="https://clang.llvm.org/">clang</a>, it compiles to unboxed data structures using monomorphization and LLVM for optimizations.</p>
<p><img src="roc-rocket.png" width=36>Friendly</p>
<p><img src="roc-rocket.png" width=36>Functional</p>

```elixir
# Hover over anything here to see an explanation
main =
    cacheUserInfo (Path.fromStr "url.txt")
    |> Task.onErr handleErr

cacheUserInfo = \filename -> # Defining a function
    url <- File.readUtf8 filename |> Task.await
    { username, email } <- Http.get url Json.codec |> Task.await

    File.writeUtf8 "\(username).txt" email

handleErr = \err ->
    when err is
        FileReadErr path _ -> Stderr.line "Error reading \(Path.display path)"
        FileWriteErr path _ -> Stderr.line "Error writing \(Path.display path)"
        HttpErr url _ -> Stderr.line "Error getting \(url)"
```

> TODO maybe make some notes about the above code, e.g. it uses basic-cli and Luke's JSON package,
> this is demonstrating how type annotations are optional; this code is 100% type-checked even though
> there are no types mentioned anywhere. That also includes the JSON decoding, which is done via
> type inference, and which is not specific to JSON (or any particular encoding).

## Try Roc

> TODO only show this if you have JavaScript enabled!
The fastest way to try Roc is in this REPL. It runs in your browser via WebAssembly, and will keep working if you lose your connection.

```
TODO Web REPL goes here!
```

Roc is still a work in progress. It doesn't have a numbered release yet, but you can download a [nightly release](https://github.com/roc-lang/roc/releases/tag/nightly) to try out building things with it. In the (likely) event that you encounter a bug, we’d really appreciate if you’d [open an issue](https://github.com/roc-lang/roc/issues) to let us know about it.

The code below shows a Roc application which prints `Hello World!` to the terminal. It does this using the [roc-lang/basic-cli](https://github.com/roc-lang/basic-cli) platform.

```roc
app "hello-world"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    Stdout.line "Hello, World!"
```

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

<script src="repl.js"></script>
