app [main!] { pf: platform "./platform/main.roc" }

# The platform requires `main! : () => {}`, but this returns a Str.
# The `msg : Str` annotation pins the body to a concrete Str so the mismatch
# against the platform's required `{}` return surfaces as a TYPE MISMATCH.
# (A bare `"hello"` would stay an open quote literal and the entry-point check
# would instead report POLYMORPHIC VALUE — see fx_platform_test.)
main! = || {
    msg : Str
    msg = "hello"
    msg
}
