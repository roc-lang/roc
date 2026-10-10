# Serial compile-time dependency scheduling

These fixtures preserve the language contracts needed by a serial SCC schedule:
forward value dependencies through an invoked callable, valid mutual function
recursion, and guarded failure ownership. `Cycle.roc` is an eager value cycle and
must be rejected; recursive functions are not such a cycle.

Run directly with an isolated compiler cache. Checking and native test execution
must preserve values and diagnostics before and after cache population.

`check_schedule.py` runs the focused workflows without building platform hosts.
It preserves exact failure diagnostics and executes the successful expectations
after editing each consumer.
