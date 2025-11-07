platform ""
    requires {} { main! : () => {} }
    exposes [Stdout, Stderr, Host]
    packages {}
    provides { main_for_host! }

main_for_host! : () => {}
main_for_host! = main!
