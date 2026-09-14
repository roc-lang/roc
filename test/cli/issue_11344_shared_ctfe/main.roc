import Helper

main! = |args| {
    echo!(Helper.render("constant"))
    for arg in args {
        echo!(Helper.render(arg))
    }
    Ok({})
}
