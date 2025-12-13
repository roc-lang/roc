app [main] { pf: platform "./platform/main.roc" }

Model : { value: I64 }

main = {
    init: |{}| { value: 0 },
    update: |m, delta| { value: m.value + delta },
    render: |m| m.value,
}
