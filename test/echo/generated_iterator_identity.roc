app [main!] {
    u: "./iterator_identity_pkg/main.roc",
}

# Two modules call the same custom iterator at one request type, each seeing
# the item's nominal types through its own checked type ids; a third uses it
# as a procedure value, which keeps its own local specialization.
import u.Shape
import u.Measure
import u.Count

main! = |_| {
    echo!("${Str.inspect(Shape.ends("ab"))}\n")
    echo!("${Str.inspect(Measure.starts("ab"))}\n")
    echo!("${Str.inspect(Count.total(["ab", "c"]))}\n")
    Ok({})
}
