app [main!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/12230
#
# `Loud` matches `Enc.Form`'s fields but declares its own encoder. An annotated
# record is never a named type, so `Loud` goes through `Enc.encode_any`, whose
# encoder specialization stays separate from the one every plain-record caller
# of `Enc.encode` shares.

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
        match Enc.encode_any(loud) {
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
