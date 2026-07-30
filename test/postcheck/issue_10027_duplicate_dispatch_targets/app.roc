app [main!] { pf: platform "platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10027
# A custom quoted-string conversion and an exposed package wrapper must check.
import pf.OsStr exposing [OsStr]

main! : () => {}
main! = || {
    _os_str : OsStr
    _os_str = "echo"

    {}
}
