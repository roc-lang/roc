use std::mem::size_of;

use roc_parse::ast::Expr;
use roc_repl_eval::ReplApp;

pub struct WasmReplApp<'a> {
    module: &'a [u8],
    copied_memory: &'a [u8],
}

macro_rules! deref_number {
    ($name: ident, $t: ty) => {
        fn $name(&self, address: usize) -> $t {
            const N: usize = size_of::<$t>();
            let mut array = [0; N];
            array.copy_from_slice(&self.copied_memory[address..][..N]);
            <$t>::from_le_bytes(array)
        }
    };
}

impl<'a> ReplApp for WasmReplApp<'a> {
    fn deref_bool(&self, address: usize) -> bool {
        self.copied_memory[address] != 0
    }

    deref_number!(deref_u8, u8);
    deref_number!(deref_u16, u16);
    deref_number!(deref_u32, u32);
    deref_number!(deref_u64, u64);
    deref_number!(deref_u128, u128);
    deref_number!(deref_usize, usize);

    deref_number!(deref_i8, i8);
    deref_number!(deref_i16, i16);
    deref_number!(deref_i32, i32);
    deref_number!(deref_i64, i64);
    deref_number!(deref_i128, i128);
    deref_number!(deref_isize, isize);

    deref_number!(deref_f32, f32);
    deref_number!(deref_f64, f64);

    fn deref_str(&self, addr: usize) -> &str {
        let elems_addr = self.deref_usize(addr);
        let len = self.deref_usize(addr + size_of::<usize>());
        let bytes = &self.copied_memory[elems_addr..][..len];
        std::str::from_utf8(bytes).unwrap()
    }

    fn call_function<'e, Return: Sized, F: Fn(Return) -> Expr<'e>>(
        &self,
        _main_fn_name: &str,
        _transform: F,
    ) -> Expr<'e> {
        todo!()
    }

    fn call_function_dynamic_size<T: Sized, F: Fn(usize) -> T>(
        &self,
        _main_fn_name: &str,
        _ret_bytes: usize,
        _transform: F,
    ) -> T {
        todo!()
    }
}
