# Layout

Memory layout computation and management for data structures during code generation and optimization.

- [src/layout/layout.zig](./layout.zig): Core layout computation algorithms for determining memory layouts, alignment, and size requirements for Roc data types.
- [src/layout/store.zig](./store.zig): Layout storage and caching system for efficiently managing computed layouts across compilation phases.
- [src/layout/work.zig](./work.zig): Work queue and task management for parallel layout computation and dependency resolution.
- [src/layout/store_test.zig](./store_test.zig): Unit tests for the layout storage system ensuring correctness of layout computations and caching behavior.