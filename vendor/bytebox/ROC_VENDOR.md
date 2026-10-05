# Bytebox source origin

The sources are from [rtfeldman/bytebox](https://github.com/rtfeldman/bytebox/tree/6565220e5d16eb230b05a85fd9609f280dc249c6)
at commit `6565220e5d16eb230b05a85fd9609f280dc249c6` (version 0.0.1).
The MIT license is retained in `LICENSE`. This revision includes Roc's
full-width instruction indices for if continuations; replacing it with an older
upstream release would lose that fix.

The repository had no published Zig 0.17-compatible revision when this upgrade
was prepared. Roc therefore vendors the pinned runtime and a library-only build
description. The exported `bytebox` module, its configuration options, and its
exact pinned `stable_array` dependency are preserved. The standalone CLI, C FFI
library, benchmarks, and external WebAssembly specification tests are omitted
from the build graph because Roc consumes the Zig module directly.

Runtime changes use Zig 0.17's reflected enum arrays, optimization mode names,
`@Int`, sentinel formatting API, and explicit arrays in place of array repetition.
`Val.V128` uses C-compatible `[4]f32 align(16)` storage instead of an extern vector field.
Vectors coerce to and from this array, preserving 16-byte union size and alignment and the
scalar reinterpretation used by VM marshalling. `roc_smoke_test.zig` validates
WebAssembly decoding, instantiation, and execution under safety checks. Further
changes are recorded in the Git diff against the source revision above. `README.md` is the upstream documentation;
its standalone build instructions do not describe this narrowed build graph.
