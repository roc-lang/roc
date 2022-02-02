use roc_parse::ast::Expr;

pub mod app_memory;
pub mod eval;
pub mod gen;

pub trait ReplApp {
    fn deref_bool(&self, addr: usize) -> bool;

    fn deref_u8(&self, addr: usize) -> u8;
    fn deref_u16(&self, addr: usize) -> u16;
    fn deref_u32(&self, addr: usize) -> u32;
    fn deref_u64(&self, addr: usize) -> u64;
    fn deref_u128(&self, addr: usize) -> u128;
    fn deref_usize(&self, addr: usize) -> usize;

    fn deref_i8(&self, addr: usize) -> i8;
    fn deref_i16(&self, addr: usize) -> i16;
    fn deref_i32(&self, addr: usize) -> i32;
    fn deref_i64(&self, addr: usize) -> i64;
    fn deref_i128(&self, addr: usize) -> i128;
    fn deref_isize(&self, addr: usize) -> isize;

    fn deref_f32(&self, addr: usize) -> f32;
    fn deref_f64(&self, addr: usize) -> f64;

    fn deref_str(&self, addr: usize) -> &str;

    fn call_function<'a, Return: Sized, F: Fn(Return) -> Expr<'a>>(
        &self,
        main_fn_name: &str,
        transform: F,
    ) -> Expr<'a>;

    fn call_function_dynamic_size<'a, T: Sized, F: Fn(usize) -> T>(
        &self,
        main_fn_name: &str,
        ret_bytes: usize,
        transform: F,
    ) -> T;
}
