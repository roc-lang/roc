app [main!] { pf: platform "./platform/main.roc" }

# The wasm32 side of the host-ABI pin (design.md "Host Symbol ABI"): a hosted
# function declared Try(Str, [HostErr(Str)]) whose host always returns Ok("ok"),
# unwrapped with `?` into a wider error row. The widened use site must be
# bridged by an adapter rather than by an extern emitted at the wider row, so
# this cart's output is the host's own "ok"; a misread would return the Err text
# instead and fail the expected-output check.

import pf.FallibleHost

main! : () => Str
main! = || {
    match widened!({}) {
        Ok(value) => value
        Err(HostErr(message)) => "misread as Err(HostErr(${message}))"
        Err(Widened(_)) => "misread as Err(Widened)"
    }
}

widened! : {} => Try(Str, [HostErr(Str), Widened(I32)])
widened! = |{}| Ok(FallibleHost.str_ok!({})?)
