
# Design Documentation

This engineering blog is designed to facilitate collaboration and communication within the community. It provides a platform to share design documentation and to develop ideas through comments and feedback. The documentation not only gives the design intent, but also serves as a reference for contributors to implement and for newcomers to understand the features.

## [Crash Reporting](/crash-reporting.html)

    Status: Proposal seeking feedback

A proposal to make it easy for users to report crashes. Whenever the compiler crashes, a message will be printed to the user with instructions on how to automatically report the crash to the public crash tracker. The report will include details about the operating system, the version of Roc being used, the compiler error, and a backtrace of the compiler's call stack.

[Zulip discussion link](https://roc.zulipchat.com/#narrow/stream/231635-compiler-development/topic/crash.20reporter/near/313538759)

## [Debugging `dbg` Feature](/dbg-feature.html)

    Status: Implemented

A proposal for a new debugging keyword `dbg`. It works in a similar manner toElm's `Debug.log` and Rust's `dbg!` macro by accepting a string and a value, printing the string and the value to the console, and then returning the value. `dbg` calls would only work in `roc dev` and `roc test` and are discarded in `roc build`.

