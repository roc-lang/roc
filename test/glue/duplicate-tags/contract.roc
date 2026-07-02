app [main!] { pf: platform "./main.roc" }

import pf.Host
import pf.A
import pf.B
import pf.C
import pf.D

is_ok : Try(a, err) -> Bool
is_ok = |result|
    match result {
        Ok(_) => True
        Err(_) => False
    }

main! = || {
    host_unit = Host.fallible_unit!("host-unit")
    host_str = Host.fallible_str!("host-str")
    host_bytes = Host.fallible_bytes!("host-bytes")
    host_record = Host.fallible_record!("host-record")
    host_nested = Host.fallible_nested!("host-nested")
    expect is_ok(host_unit)
    expect is_ok(host_str)
    expect is_ok(host_bytes)
    expect is_ok(host_record)
    expect is_ok(host_nested)

    a_unit = A.unit!("a-unit")
    a_str = A.str!("a-str")
    a_bytes = A.bytes!("a-bytes")
    a_record = A.record!("a-record")
    a_nested = A.nested!("a-nested")
    expect is_ok(a_unit)
    expect is_ok(a_str)
    expect is_ok(a_bytes)
    expect is_ok(a_record)
    expect is_ok(a_nested)

    b_unit = B.unit!("b-unit")
    b_str = B.str!("b-str")
    b_bytes = B.bytes!("b-bytes")
    b_record = B.record!("b-record")
    b_nested = B.nested!("b-nested")
    expect is_ok(b_unit)
    expect is_ok(b_str)
    expect is_ok(b_bytes)
    expect is_ok(b_record)
    expect is_ok(b_nested)

    c_unit = C.unit!("c-unit")
    c_str = C.str!("c-str")
    c_bytes = C.bytes!("c-bytes")
    c_record = C.record!("c-record")
    c_nested = C.nested!("c-nested")
    expect is_ok(c_unit)
    expect is_ok(c_str)
    expect is_ok(c_bytes)
    expect is_ok(c_record)
    expect is_ok(c_nested)

    d_unit = D.unit!("d-unit")
    d_str = D.str!("d-str")
    d_bytes = D.bytes!("d-bytes")
    d_record = D.record!("d-record")
    d_nested = D.nested!("d-nested")
    expect is_ok(d_unit)
    expect is_ok(d_str)
    expect is_ok(d_bytes)
    expect is_ok(d_record)
    expect is_ok(d_nested)

    {}
}
