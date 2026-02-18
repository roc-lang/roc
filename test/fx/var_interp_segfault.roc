app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

print! : Str => {}
print! = |msg| msg.split_on("\n").for_each!(Stdout.line!)

fnA! : Str => Try(I64, _)
fnA! = |_input| {
    var $x = 1
    Ok($x)
}

fnB! : Str => Try(I64, _)
fnB! = |_input| {
    var $y = 2
    Ok($y)
}

run! = || {
    print!("A1: ${fnA!("test")?.to_str()}")
    print!("A2: ${fnA!("test")?.to_str()}")
    print!("A3: ${fnA!("test")?.to_str()}")
    Ok({})
}

main! = || {
    _ignore = run!()
}
