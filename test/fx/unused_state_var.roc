app [main!] { pf: platform "./platform/main.roc" }

Step := [Start].{
    init : Step
    init = Start
}

main! = || {

    start : Step
    state = Start

    {}
}
