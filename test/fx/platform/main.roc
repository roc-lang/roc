platform ""
    requires {} { main! : () => {} }
    exposes []
    packages {}
    provides { main_for_host! }

main_for_host! : () => {}
main_for_host! = |_| main!()
