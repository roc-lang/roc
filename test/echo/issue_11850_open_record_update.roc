# repro for https://github.com/roc-lang/roc/issues/11850
#
# A value whose record row is open carries the complete record it was built
# from, so updating, returning, or mapping it keeps the fields its row does not
# name, including fields that sort before the named one.
birthday = |person| { ..person, age: person.age + 1 }
bump = |r| { ..r, z: r.z + 1 }
twice = |r| bump(bump(r))
pass = |r| { _ = r.z
 r }
zs = |items| items.map(|r| r.z)
later = |r| |n| { ..r, z: r.z + n }
get_z = |{ z, .. }| z

main! = |_| {
    echo!("${Str.inspect(birthday({ name: "Ada", age: 36 }))}\n")
    echo!("${Str.inspect(bump({ a: "x", m: [1, 2], z: 1 }))} ${Str.inspect(twice({ a: "x", z: 1 }))}\n")
    echo!("${Str.inspect(pass({ b: 2.U8, z: "s" }))} ${Str.inspect(zs([{ a: "p", z: 3 }, { a: "q", z: 4 }]))}\n")
    echo!("${Str.inspect(later({ a: "kept", z: 10 })(5))} ${Str.inspect(get_z({ a: "o", z: 9 }))}\n")
    echo!("${Str.inspect([{ a: "p", z: 3 }, { a: "q", z: 4 }].map(bump))}\n")
    Ok({})
}
