# Never reads the list again, so its call is at the base signature.
import Lib

main! = |args| {
    xs = List.repeat(0, 10 + args.len())
    total = match Lib.poke(xs, 3, 7) {
        Ok(changed) => List.sum(changed)
        Err(_) => 0
    }
    echo!(total.to_str())
    Ok({})
}
