pub fn foo(x: i64) -> (i64, i64) {
    return (x, x);
}

pub fn test_main() -> i64 {
    let record = foo(0x9);

    record.0 * record.1
}

pub fn main() {
    test_main();
}
