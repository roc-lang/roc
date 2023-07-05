<!--
The purpose of the Home page is to function as the first point of contact for all visitors. It should seamlessly connect people with the most suitable content for their immediate needs; it provides information at a high level of abstraction and guides visitors towards more relevant and detailed content within the site.

This page contains minimal content, so that visitors are able to quickly scan the page. It should not be cluttered or overwhelming. It should reflect Roc's goals (fast, friendly, functional) and values (welcoming community).
-->

# Roc lang

A systems programming language that is fast, friendly, and functional.

<section class="home-goals-container">
    <div class="home-goals-column">
      <h2 class="home-goals-title">Fast</h2>
      <p class="home-goals-description">We want Roc to run faster than any non-systems language that sees mainstream use in industry. <a class="home-goals-learn-more" href="/design_goals.html#fast">Learn more</a></p>
    </div>
    <div class="home-goals-column">
      <h2 class="home-goals-title">Friendly</h2>
      <p class="home-goals-description">Roc aims to be a user-friendly language with a friendly community of users. <a class="home-goals-learn-more" href="/design_goals.html#friendly">Learn more</a></p>
    </div>
    <div class="home-goals-column">
      <h2 class="home-goals-title">Functional</h2>
      <p class="home-goals-description">Roc aims to be a purely functional programming language. <a class="home-goals-learn-more" href="/design_goals.html#functional">Learn more</a></p>
</section>

## Try Roc

<!-- WebREPL should go here, first impression -->

```roc
app "hello-world"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

main = Stdout.line "Hello, World!"
```

## Use cases

-   Tools & Scripts
-   Web (coming soon) <!-- richard starting on this -->
-   Networking & Servers (coming soon)
-   Graphical (coming soon)
-   Scientific (coming soon)
-   Embedded (coming soon)

## Roc Platforms/Applications (vs libraries)

<!--
provide explamation of platform/application abstraction versus libraries as common in most other languages as this is one of the most unique features of Roc
-->
TODO 

## Talks and Publications

If you'd like to learn more about Roc check out one of these videos:

*   [Roc at Handmade Seattle](https://media.handmade-seattle.com/roc-lang) - November 12, 2021 (very low-level explanation of how Roc's compiler makes programs run fast)
*   [Outperforming Imperative with Pure Functional Languages](https://youtu.be/vzfy4EKwG_Y) - October 1, 2021 (about Roc's runtime performance and optimizer)
*   [A taste of Roc](https://youtu.be/6qzWm_eoUXM) - September 23, 2021 (syntax, application examples)
*   [Roc at the Philly ETE conference](https://youtu.be/cpQwtwVKAfU?t=75) - May 6, 2021 (platforms and applications)
*   [Roc on Zig Showtime](https://youtu.be/FMyyYdFSOHA) - April 24, 2021 (making a platform)
*   [Roc at the Berlin FP Meetup](https://youtu.be/ZnYa99QoznE?t=4790) - September 1, 2020 (overall vision for the language)
