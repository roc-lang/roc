app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

my_sum = |list| {
    var $state = 0
    var $index = list.len()
    while $index > 0 {
        $index = $index - 1
        # Use List.get instead of list_get_unsafe (which we can't access)
        match List.get(list, $index) {
            Ok(elem) => { $state = $state + elem }
            Err(_e) => {}
        }
    }
    $state
}

main! = || {
    Stdout.line!("Testing while loop lookup")
    list = [1, 2, 3]
    r = my_sum(list)
    Stdout.line!("Sum: ${r.to_str()}")
}
