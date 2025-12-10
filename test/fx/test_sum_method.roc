app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

my_sum = |list| {
    var $state = 0
    var $index = list.len()
    while $index > 0 {
        $index = $index - 1
        # Use list.get with method syntax
        match list.get($index) {
            Ok(elem) => { $state = $state + elem }
            Err(_e) => {}
        }
    }
    $state
}

main! = || {
    Stdout.line!("Testing while loop with method lookup")
    list = [1, 2, 3]
    
    Stdout.line!("Using user-defined my_sum")
    r1 = my_sum(list)
    Stdout.line!("User my_sum: ${r1.to_str()}")
    
    Stdout.line!("Using my_sum as method")
    # This is different - it doesn't go through method dispatch
    # because my_sum is in the app module, not List module
}
