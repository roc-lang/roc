app [main!] {
    hlp: "./helper_pkg/main.roc",
}

import hlp.Greeting

expect Greeting.greet("Roc") == "Hello, Roc!"

main! = |_| {
    echo!(Greeting.greet("World"))
    Ok({})
}
