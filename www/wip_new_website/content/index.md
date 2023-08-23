
# Roc

A work-in-progress programming language that aims to be fast, friendly, and functional.

<!-- TODO turn these into nice buttons -->
- [tutorial](/wip/tutorial.html)

- [install](/wip/install.html)

- [help / group chat](https://roc.zulipchat.com), we're friendly!

## Goals
<section class="home-goals-container">
    <div class="home-goals-column">
      <h3 class="home-goals-title">Fast</h4>
      <p class="home-goals-description">Delightful software runs fast. The Roc compiler should run fast, and it should be able to produce programs that run fast too. <a class="home-goals-learn-more" href="/design_goals.html#fast">What does <i>fast</i> mean here?</a></p>
    </div>
    <div class="home-goals-column">
      <h3 class="home-goals-title">Friendly</h3>
      <h2 class="home-goals-title">Friendly</h2>
      <p class="home-goals-description">Roc aims to be a user-friendly language with a friendly community of users. This involves the set of tools Roc includes, and also the spirit of the community of Roc programmers. <a class="home-goals-learn-more" href="/design_goals.html#friendly">What does <i>friendly</i> mean here?</a></p>
    </div>
    <div class="home-goals-column">
      <h3 class="home-goals-title">Functional</h3>
      <p class="home-goals-description">Roc is a purely functional programming language. The language is built on a small set of simple primitives, which together give you a toolset that's more than the sum of its parts. <a class="home-goals-learn-more" href="/design_goals.html#functional">What does <i>functional</i> mean here?</a></p>
</section>

## Try Roc

<!-- TODO WebREPL to go here -->

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
