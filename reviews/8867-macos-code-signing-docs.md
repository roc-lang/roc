# PR #8867 — Document macOS code signing and notarization setup

- **Author:** rtfeldman · **Draft:** yes · **Base:** `main`
- **Size:** +120 / −0 — one new file, `ci/MACOS_CODE_SIGNING.md`
- **Opened:** 2026-01-01 — the oldest PR in the set

Documents the foundation's Apple Developer Program enrollment, the seven
repository secrets, and the setup steps for signing and notarizing macOS
nightlies.

## Verdict

Docs-only, no code risk, and the content is accurate as far as it goes — the
enrollment requirements, the Account-Holder-only certificate constraint, the
Team-Key-not-Individual-Key gotcha, and the fee-waiver pointer are all correct
and are exactly the things people get wrong.

**One finding is worth acting on before anyone enables signing** (#1): the
hardened runtime this document prescribes is very likely to break Roc's dev
backend on macOS. The rest are gaps that will cost someone an afternoon each.

---

## Findings

### 1. (Important) `-o runtime` will probably break the dev backend's in-process execution

The document instructs signing with hardened runtime:

> The binary is signed with `codesign` using the Developer ID certificate and
> hardened runtime (`-o runtime`)

Roc's dev backend executes generated machine code **in the `roc` process**.
`src/backend/dev/ExecutableMemory.zig` allocates anonymous RW pages, writes code
into them, and then flips them executable (`:398-401`):

```zig
.posix => {
    const prot: std.posix.PROT = .{ .READ = true, .EXEC = true };
    if (std.c.mprotect(@ptrCast(memory.ptr), memory.len, prot) != 0) return error.MprotectFailed;
},
```

Under the hardened runtime on macOS, making previously-writable anonymous memory
executable is blocked unless the binary carries one of:

- `com.apple.security.cs.allow-jit` — **and** the pages were allocated with
  `MAP_JIT` (which this code does not use; I grepped, there is no `MAP_JIT`
  anywhere in the tree), or
- `com.apple.security.cs.allow-unsigned-executable-memory`.

Without an entitlement, the likely outcome is `mprotect` returning `EACCES`, so
`roc run` / `roc test` / the REPL fail with `error.MprotectFailed` on signed
nightlies — while working perfectly on every unsigned local build, which is the
worst possible way to discover it.

The document should:

- Note the constraint and say which entitlement Roc needs (my read is
  `allow-unsigned-executable-memory`, since the code uses plain `mprotect` rather
  than `MAP_JIT`; `allow-jit` would require an `ExecutableMemory.zig` change
  too).
- Show the `--entitlements roc.entitlements` flag alongside `-o runtime`, and
  either include the plist or say where it lives.
- Add a verification step: run `roc test` on some fixture *after* signing,
  before publishing. Notarization succeeding tells you nothing about whether the
  binary still works.

This is the one item I'd want resolved before the signing steps are wired up in
`roc-lang/nightlies`, because everything else in this doc fails loudly and this
one fails only for users.

### 2. (Should fix) `codesign` guidance omits `--timestamp`

Apple's notarization service **rejects** submissions whose signature has no
secure timestamp. The "How It Works" section mentions only `-o runtime`, and
the "The executable does not have the hardened runtime enabled" troubleshooting
entry reinforces that as the whole story.

A reader following this doc will get their first submission rejected with
`The signature does not include a secure timestamp`. Add `--timestamp` to the
described invocation and, ideally, that rejection to Troubleshooting.

### 3. (Should fix) The troubleshooting command uses credentials the doc never creates

```sh
xcrun notarytool log <submission-id> --keychain-profile "notarytool-profile"
```

Nothing in the setup instructions creates a keychain profile. The doc
establishes `APPLE_NOTARIZATION_KEY_ID` / `_ISSUER` / `_KEY` for CI, which
`notarytool` consumes as `--key-id` / `--issuer` / `--key`. Someone debugging a
rejection will run this command and get "profile not found."

Either show `xcrun notarytool store-credentials "notarytool-profile" --key … --key-id … --issuer …`
as a prerequisite, or change the troubleshooting command to the same
`--key/--key-id/--issuer` form the pipeline uses.

### 4. (Should add) Stapling is omitted, and for a bare executable that's a real constraint

Step 5 of "How It Works" says:

> When users download and run the binary, macOS checks Apple's servers to verify
> it's notarized

That's true for an *unstapled* artifact, and the doc never mentions
`xcrun stapler staple` — which is the normal final step. The reason it's absent
is probably correct: **`stapler` cannot attach a ticket to a bare Mach-O
executable**, only to `.dmg`, `.pkg`, or `.app` bundles.

But that's a meaningful consequence the doc should state rather than leave
implicit:

- First run on a machine with no network (or with Apple's OCSP endpoint blocked)
  can hang or fail Gatekeeper's check.
- If nightlies are ever repackaged as a `.pkg` or `.dmg`, stapling becomes
  possible and should be added.

One paragraph: "we do not staple because the artifact is a bare executable;
Gatekeeper therefore verifies online on first launch."

### 5. (Should add) State which distribution path this actually protects

Gatekeeper only evaluates files carrying the `com.apple.quarantine` extended
attribute, which is set by browsers and some archive utilities — **not** by
`curl`, `wget`, or `tar` on the command line.

If Roc nightlies are installed via a `curl | sh` style script, notarization
changes nothing about the user experience today. If they're downloaded from a
GitHub Releases page in a browser, it changes everything. The doc justifies the
whole effort with "so downloaded nightlies run without Gatekeeper warnings" but
never says which download path is meant. Worth one sentence, because it's the
difference between this being urgent and being optional.

### 6. (Should add) Rotation and expiry

Nothing covers the operational tail:

- **Developer ID Application certificates expire after 5 years.** Signed
  binaries stay valid past expiry only because of the secure timestamp (see #2)
  — another reason `--timestamp` isn't optional. But a new certificate must be
  issued and `MACOS_CERTIFICATE` / `MACOS_CERTIFICATE_NAME` re-uploaded before
  then, and the Team ID in the name may change.
- **The App Store Connect API key doesn't expire but can be revoked**, and it can
  only be downloaded once — the doc says this for the `.p8` (good) but not for
  the `.p12`, which has the same problem and is arguably more sensitive since it
  contains the private key.

A short "Rotation" section, or even just a note that the `.p12` also needs a
backup in the secrets manager, would close this.

### 7. (Should add) Who can read these secrets

Seven secrets, one of which is a code-signing private key that can sign anything
as the Roc Programming Language Foundation. Anyone who can merge a workflow
change to `roc-lang/nightlies` can exfiltrate all of them in one commit.

The doc is the natural place to say that the nightlies repo needs restricted
write access, and/or that the signing job should run in a GitHub **Environment**
with required reviewers so secrets aren't available to arbitrary workflow runs.
That's a one-paragraph addition with a large blast-radius payoff.

### 8. (Nit) `base64 -i/-o` is BSD-only

```sh
base64 -i certificate.p12 -o certificate-base64.txt
```

GNU coreutils `base64` rejects `-i`/`-o`. This step is performed in Keychain
Access on a Mac so it's fine in context — but if anyone ever regenerates the
secret from a Linux box, `base64 -w0 certificate.p12 > certificate-base64.txt`
is the portable form. Worth a parenthetical.

### 9. (Question) Location, and the seven-month gap

The doc lives at `ci/MACOS_CODE_SIGNING.md` in `roc-lang/roc`, but every secret
and every workflow step it describes belongs to `roc-lang/nightlies`. The PR
body explains why (the workflow moved after the PR opened), and the doc itself
is careful to say "not in `roc-lang/roc`" — good.

Still worth deciding deliberately rather than by inertia: someone configuring
the nightlies repo will look for these instructions in the nightlies repo. If it
stays here, a one-line pointer from `roc-lang/nightlies`' README would close the
loop. If it moves, this PR becomes a redirect stub.

Related: this has been a draft since 2026-01-01 and the situation it documents
has already changed once. Either it's blocked on the foundation's enrollment (in
which case say so at the top of the doc — "signing is not yet enabled; this
documents the intended setup"), or it can land as-is with the fixes above, since
documentation of an unimplemented process is still better than none.

## What's good

- **The non-obvious Apple gotchas are all there and all correct**: organization
  enrollment requiring a D-U-N-S number, Developer ID Application certificates
  requiring the Account Holder role, Team Keys vs. Individual Keys (individual
  keys genuinely cannot drive `notarytool`), and the one-time `.p8` download.
  Those four are where most people lose a day.
- **The fee-waiver pointer** is a genuinely useful thing to surface for a
  nonprofit foundation and is easy to miss on Apple's site.
- **Secrets are tabulated with descriptions**, including the example
  `MACOS_CERTIFICATE_NAME` format with the Team ID in parentheses — which is the
  form `codesign -s` actually needs.
- **The References section** links the primary Apple sources rather than blog
  posts, so it won't rot as fast.
