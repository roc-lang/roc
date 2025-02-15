# Fuzzing Roc

Currently, Roc is using [zig-afl-kit](https://github.com/kristoff-it/zig-afl-kit) for fuzzing.
This is a thin wrapper around [afl++](https://aflplus.plus) to enable coverage guided fuzzing.

## Getting Compiling

> **Note**: system llvm is not needed if you just want to repro a fuzz failures.
Even without system llvm, the `repro-*` executables will be built.

Unlike the rest of roc, afl++ needs a full llvm installation on the system to compile
and can not use the vendored version that is automatically downloaded.
The exact version of llvm shouldn't matter, but it should still probably be as new as possible.
You can verify which llvm afl++ will grab by running `llvm-config --version`.
You can also explicitly pass a path to search for llvm with `-Dllvm-path=...`.

Once you have a llvm, building the fuzz executables should be as easy as `zig build -Dfuzz`.
This will build a handful of executables.
Firstly, it will build `zig-out/AFLplusplus/bin` with all of the expect afl++ executables.
Secondly, it will build `zig-out/bin/fuzz-*` executables to run fuzzing on.
Lastly, it will build `zig-out/bin/repro-*` executables to help with easily reproducing fuzzing failures (often with more debug prints).


## Basic Usage

afl++ can be a bit of a beast to use. The basics may be simple enough, but it has a lot of things to learn.

If you just want to run a fuzzer single threaded for a short bit, this is the only command you need:
`./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz/tokenize-corpus/ -o /tmp/tokenize-out zig-out/bin/fuzz-tokenize`
 Just change `tokenize` to the fuzzer you want to run.

At this point, the fuzzer should start running and looking for crashes.
The found crashes will get dumped to files under `/tmp/tokenize-out/default/crashes`.
Theses files are not guaranteed to be valid ascii, but often they are.
You can try to view them with a standard text editor.

Once you have a failure, it is often easier to debug via the repro executable.
Run `./zig-out/bin/repro-tokenize /tmp/tokenize-out/default/crashes/...`, replacing `...` with one of the crash file names.
The `repro-*` executables try to add in extra prints for more useful debugging. They also should give full stacktraces on failures.
Feel free to modify and make them better as you debug.

If your test case is too complex, you can run `afl-tmin` to create a simpler test case.
`./zig-out/AFLplusplus/bin/afl-tmin -i /tmp/tokenize-out/default/crashes/... -o /tmp/tokenize-out/min zig-out/bin/fuzz-tokenize`, replacing `...` with one of the crash file names.
After it finishes, `/tmp/tokenize-out/min` should contain the minimized test case.

Once you have fixed the bug, make a PR and loop back to fuzzing again.
If the test case is complex or interesting, it likely is worthwhile to add a snapshot test that covers it.

### **------> IMPORTANT <------**

For better fuzzing performance you will want to modify `std.mem.backend_can_use_eql_bytes` to return false, otherwise AFL++ will not be able to observe char-by-char string comparisons and its fuzzing capabilities will be greatly reduced.

This means modifying your copy of the Zig stdlib. If you have ZLS you can simply write `std.mem` anywhere in your code and goto definiton, otherwise you can invoke `zig env` and modify `$std_dir/mem.zig`.

**Also don't forget to revert this change after you're done!**

## Advanced Usage

afl++ has a ton of bells and whistle to get way more performance and better results out of it.
The largest one is probably [running multiple copies of afl (preferrably with different configs)](https://aflplus.plus/docs/fuzzing_in_depth/#c-using-multiple-cores) to use all your compute power.

Probably the largest other gain is in keeping around and managing a corpus of examples.
This enables the fuzzer to build up more and more information about the target executable of different execution sessions.

Just explore the [afl++ website](https://aflplus.plus) for various other tips.
