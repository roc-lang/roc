# Cache

Cache intermediate compilation artefacts to speed up subsequent builds. We can tokenize->parse->canonicalize each file in isolation then cache and re-use this information. Only if the source code changes do we need to recompute the canonicalized representation.

- [src/cache/mod.zig](./mod.zig): Entry point and common utilities for the caching system.
- [src/cache/CacheManager.zig](./CacheManager.zig): Core cache management including storage, retrieval, and invalidation logic.
- [src/cache/CacheModule.zig](./CacheModule.zig): Module-level caching for storing compiled module artifacts and metadata.
- [src/cache/CacheKey.zig](./CacheKey.zig): Cache key generation and hashing for identifying cached compilation artifacts.
- [src/cache/CacheConfig.zig](./CacheConfig.zig): Configuration settings and policies for cache behavior and storage locations.
- [src/cache/CacheReporting.zig](./CacheReporting.zig): Cache hit/miss reporting and performance diagnostics.
