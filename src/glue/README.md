# Roc Glue Release Files

The checked-in glue specs under `src/glue/src/` use the source-tree platform at
`../platform/main.roc`. Keep that form for local compiler development.

Nightly releases should use generated copies instead:

```sh
zig build build-glue-release -Dglue-release-tag=nightly-YYYY-Month-DD-SHORTSHA
```

This writes `zig-out/glue-release/`:

```text
package/<hash>.tar.zst
glue/RustGlue.roc
glue/ZigGlue.roc
glue/CGlue.roc
glue/README.md
glue/env
```

The package archive is produced by running `roc bundle` in `src/glue/platform`
with `main.roc` as the root file. The release helper only copies that archive
and rewrites the release specs to point at its content-hash URL.

Upload `package/<hash>.tar.zst` to the matching `roc-lang/nightlies` release.
Include the generated `glue/` directory in every platform compiler archive for
that same release. The generated specs reference the exact package URL, so
downstream repos do not need a Roc source checkout.

When `roc-lang/setup-roc` installs a nightly that contains `glue/`, it exports:

```text
ROC_GLUE_DIR
ROC_RUST_GLUE
ROC_ZIG_GLUE
ROC_C_GLUE
ROC_GLUE_PLATFORM_URL
```

Downstream CI can then run:

```sh
roc glue "$ROC_RUST_GLUE" ./platform/main.roc --output-dir ./platform
```
