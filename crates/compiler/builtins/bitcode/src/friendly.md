# Friendly

Roc is intended to be a user-friendly language with a friendly community.

## User-friendly language

- Concise syntax, e.g. just `=` for declarations, no parentheses or commas needed when calling functions
- anonymous structural literals for records, tuples, and tag unions, all of which have Eq, Hash, and Ord inferred automatically
- String interpolation
- `Dec` by default

### Helpful compiler

- Nice error messages
- If you like, you can run a program that has compile-time errors. It will crash when it gets to the error. (Note that this currently fails a lot, and has a ways to go.)

### Serialization inference
- Type inference is used for schema inference, but you can also spell it out if you like
- Reports errors immediately

### Tools built in

You can run `roc test` to run all your tests. Each test is declared with the `expect` keyword, and can be as short as one line. For example, this is a complete test:

```
## One plus one should equal two.
expect 1 + 1 == 2
```

If the test fails, `roc test` will show you the source code of the `expect`, along with the values of any named variables inside it, so you don't have to separately check what they were. If you write a documentation comment right before it (like `## One plus one should equal two` here), that will also be included in the test output, so you can use that to optionally describe the test if you want to.

In the future, there are plans to add built-in support for [benchmarking](https://en.wikipedia.org/wiki/Benchmark_(computing)), [generative tests](https://en.wikipedia.org/wiki/Software_testing#Property_testing), [snapshot tests](https://en.wikipedia.org/wiki/Software_testing#Output_comparison_testing), simulated I/O (so you don't have to actually run the real I/O operations, but also don't have to change your code to accommodate the tests), and "reproduction replays"—tests generated from a recording of what actually happened during a particular run of your program, which deterministically simulate all the I/O that happened.

- also note: future plan to cache tests so we only re-run tests whose answers could possibly have changed. also maybe note: tests that don't perform I/O are guaranteed not to flake b/c pure functions.

#### Formatting

`roc format` neatly formats your source code so you don't have to. It also doesn't have any configuration options, so collaborators can spend time on things other than negotiating settings.

#### Package management
- Currently just URLs (content-hashed to make them immutable)
- No installation step, global cache of immutable downloads instead of per-project folders (no need to .gitignore anything)
- There is a design for a searchable package index, but it hasn't been implemented yet.

#### Future plans
- Step debugger with replay
- Customizable "linter" (e.g. code mods, project-specific rules to enforce)
- Editor plugin ecosystem that works across editors, where plugins ship with packages
- `roc edit`

## Friendly community

<3
