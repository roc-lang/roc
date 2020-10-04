# Rebuilding the host from source

Run `build.sh` to manually rebuild this platform's host.

Note that the compiler currently has its own logic for rebuilding these hosts
(in `link.rs`). It's hardcoded for now, but the long-term goal is that
hosts will be precompiled by platform authors and distributed in packages,
at which point only package authors will need to think about rebuilding hosts.
