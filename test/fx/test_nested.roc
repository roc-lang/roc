app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    list = [1, 2]
    result = check(list)
    Stdout.line!("Len: ${result.len().to_str()}")
}

check = |list| {
    var $built = []
    for item in list {
        Stdout.line!("Before my_any")
        _x = my_any($built, |x| x == item)
        Stdout.line!("After my_any")
        $built = $built.append(item)
    }
    $built
}

my_any = |lst, pred| {
    Stdout.line!("In my_any")
    for e in lst {
        Stdout.line!("Calling pred")
        if pred(e) {
            return True
        }
    }
    Stdout.line!("Done")
    False
}
