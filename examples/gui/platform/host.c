extern int rust_main();

/*
We need one file to be in C rather than Rust!

We need to be able to build a host.o and app.o on separate machines and link them together later,
because we want to be able to store host.o on a package manager server.

However the Rust/Cargo build system doesn't normally use .o files, preferring archive formats like .a and .rlib.
The best way we have found to generate a host.o for Rust platforms is to have a single .c file containing `main`.

https://users.rust-lang.org/t/error-when-compiling-linking-with-o-files/49635/5?u=rtfeldman
*/
int main() { return rust_main(); }