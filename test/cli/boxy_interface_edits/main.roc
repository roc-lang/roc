import C
import B

main! = |args| {
    n = args.len()
    s = if n > 5 "x" else "abc"
    echo!("${C.size(s).to_str()} ${B.label} ${B.runtime(n).to_str()} ${B.bumped(n).to_str()} ${B.point(n).to_str()}")
    Ok({})
}
