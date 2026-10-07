# Iterators grown by recursion take the forced-dynamic representation, whose
# step functions are erased callables. These programs drive such iterators
# through closures, returned values, and destructuring `next!` calls, where
# public `Iter`/`Stream` types view the forced-dynamic values. Every backend
# must print the same results.

grow_iter : Iter(U64), U64 -> Iter(U64)
grow_iter = |it, n| if n == 0 { it } else { grow_iter(it.map(|x| x + 1), n - 1) }

grow_stream : Stream(U64), U64 -> Stream(U64)
grow_stream = |s, n| if n == 0 { s } else { grow_stream(s.map(|x| x + 1), n - 1) }

iter_fold_in_closure : U64 -> ({} -> U64)
iter_fold_in_closure = |n| {
    it = grow_iter([1.U64, 2].iter(), n)
    |{}| it.fold(0, |acc, x| acc + x)
}

stream_next_in_closure : U64 -> ({} => U64)
stream_next_in_closure = |n| {
    stream = grow_stream([1.U64, 2].iter().stream(), n)
    |{}| match stream.next!() {
        One({ item, .. }) => item
        _ => 0
    }
}

stream_collect_in_closure : U64 -> ({} => U64)
stream_collect_in_closure = |n| {
    stream = grow_stream([1.U64, 2].iter().stream(), n)
    |{}| stream.collect!().sum()
}

returned_stream : U64 -> Stream(U64)
returned_stream = |n| grow_stream([1.U64, 2].iter().stream(), n)

main! = |_args| {
    iter_fold = iter_fold_in_closure(5)
    next! = stream_next_in_closure(5)
    collect! = stream_collect_in_closure(5)
    stream = grow_stream([1.U64, 2].iter().stream(), 5)
    head_and_rest = match stream.next!() {
        One({ item, rest }) => item + rest.collect!().sum()
        _ => 0
    }
    returned = returned_stream(5).collect!().sum()
    echo!("${iter_fold({}).to_str()} ${next!({}).to_str()} ${collect!({}).to_str()} ${head_and_rest.to_str()} ${returned.to_str()}")
    Ok({})
}
