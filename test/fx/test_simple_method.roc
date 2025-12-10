app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("Testing simple methods")
    list = [1, 2, 3]
    
    # len works
    Stdout.line!("len: ${list.len().to_str()}")
    
    # fold works
    r1 = list.fold(0, |acc, elem| acc + elem)
    Stdout.line!("fold: ${r1.to_str()}")
    
    # append works?
    list2 = list.append(4)
    Stdout.line!("append: ${list2.len().to_str()}")
    
    Stdout.line!("Done")
}
