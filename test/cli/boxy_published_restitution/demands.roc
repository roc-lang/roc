# Reads the list again on failure, so its call demands the published variant.
import Lib

main! = |args| {
    var $xs = List.repeat(0, 10 + args.len())
    for i in 0..<20 {
        $xs = Lib.poke($xs, i, i) ?? $xs
    }
    echo!(List.sum($xs).to_str())
    Ok({})
}
