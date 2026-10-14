app [main!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/12230
#
# `Loud` matches `Enc.Form`'s backing but declares its own encoder, so passing
# it to `Enc.encode` must keep a specialization of its own, separate from the
# one every plain-record caller shares.

import Enc
import Site1
import Site2

Loud := { name : Str, email : Str, phone : Str }.{
    encoder_for = |encoding| |_loud, state| Json.encode_str(encoding, "LOUD", state)
}

main! = |args| {
    arg = List.first(args) ?? ""
    loud : Loud
    loud = { name: arg, email: arg, phone: arg }
    lifted =
        match Enc.encode(loud) {
            Ok(json) => json
            Err(_) => ""
        }
    plain =
        match List.len(args) {
            1 => Site1.run({ name: arg, email: arg, phone: arg })
            _ => Site2.run({ name: arg, email: arg, phone: arg })
        }
    if Str.is_empty(lifted) or Str.is_empty(plain) Err(Exit(1)) else Ok({})
}
