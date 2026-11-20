app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
}

read_source : Stream(U8) -> Stream(U8)
read_source = |chunks| Stream.custom(
    Reading({ chunks, done: False }),
    Err(Unknown),
    |cursor| match cursor {
        Stopped => Err(NoMore)
        Reading(start) => {
            var $cursor = start
            while True {
                if $cursor.done {
                    return Err(NoMore)
                }
                match $cursor.chunks.next!() {
                    Done => {
                        $cursor = { ..$cursor, done: True }
                    }
                    Skip({ rest }) => {
                        $cursor = { ..$cursor, chunks: rest }
                    }
                    One({ item, rest }) => return Ok((item, Reading({ ..$cursor, chunks: rest })))
                }
            }
        }
    },
)

main! = |_args| {
    for! _item in read_source([1.U8].iter().stream()) {
    }
    Ok({})
}
