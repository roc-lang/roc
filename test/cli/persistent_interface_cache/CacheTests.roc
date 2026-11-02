CacheTests := [].{}

import Generic
import Defaults

expect {
    dbg "persistent test observation"
    Generic.f7(2.U64) == 10
}

unused = |_| crash "unused failing interface root"

expect Defaults.parses_defaults({})
