app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A derived record parser whose format iterates the field names it is given,
# then reports how many it walked as its error.
# Every step of the iteration calls an erased callable owned, so each step
# must release (or reuse) the callable it consumed.
St := { walked : U64 }

F := [F].{
    parse_u64 : F, St -> Try({ value : U64, rest : St }, [Bad, Walked(U64), Missing(Str)])
    parse_u64 = |_, s| Ok({ value: 7, rest: s })

    parse_record_start : F, St -> Try([Counted({ len : U64, rest : St }), Uncounted(St)], [Bad, Walked(U64), Missing(Str)])
    parse_record_start = |_, s| Ok(Uncounted(s))

    parse_record_field : F, Encoding.FieldName.FieldNames(_shape), St -> Try([Field({ field : Encoding.FieldName(_shape), rest : St }), TryField({ name : Str, rest : St }), TryFieldCaseless({ name : Str, rest : St }), Continue(St), Done(St)], [Bad, Walked(U64), Missing(Str)])
    parse_record_field = |_, names, s| {
        var $n = 0.U64
        for name in names.iter() {
            _ = name
            $n = $n + 1
        }
        Err(Walked(s.walked + $n))
    }

    parse_record_after_field : F, St -> Try([Continue(St), Done(St)], [Bad, Walked(U64), Missing(Str)])
    parse_record_after_field = |_, s| Ok(Done(s))

    skip_record_field : F, St -> Try(St, [Bad, Walked(U64), Missing(Str)])
    skip_record_field = |_, _| Err(Bad)

    missing_field : F, Str -> [Bad, Walked(U64), Missing(Str)]
    missing_field = |_, name| Missing(name)

    rename_field : F, Str -> Str
    rename_field = |_, name| name
}

Shape : { lo : U64, hi : U64 }

main! = || {
    parse = Shape.parser_for(F.F)
    var $walked = 0.U64
    var $i = 0.U64
    while $i < 3 {
        $walked = match parse(St.{ walked: $i }) {
            Err(Walked(walked)) => $walked + walked
            _ => $walked + 100
        }
        $i = $i + 1
    }
    Stdout.line!("walked ${$walked.to_str()}")
}
