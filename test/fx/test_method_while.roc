app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Simple function that uses while loop, called via method dispatch
my_while = |start| {
    var $i = start
    var $result = 0
    while $i > 0 {
        $i = $i - 1
        $result = $result + 1
    }
    $result
}

main! = || {
    Stdout.line!("Testing method while")
    # Call via qualified syntax
    r1 = my_while(3)
    Stdout.line!("Qualified: ${r1.to_str()}")

    # # Call via method syntax (shouldn't work - my_while not attached to a type)
    # Actually can't call my_while via method syntax since it's not a method
    Stdout.line!("Done")
}
