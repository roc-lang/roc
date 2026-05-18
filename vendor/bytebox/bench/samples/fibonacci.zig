export fn run(n: i32) i32 {
    if (n < 2) {
        return 1;
    } else {
        const a = run(n - 1);
        const b = run(n - 2);
        return a + b;
    }
}
