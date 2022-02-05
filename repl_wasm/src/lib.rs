use std::mem::size_of;

use roc_parse::ast::Expr;
use roc_repl_eval::{ReplApp, ReplAppMemory};

pub struct WasmReplApp<'a> {
    _module: &'a [u8],
}

pub struct WasmMemory<'a> {
    copied_bytes: &'a [u8],
}

impl<'a> ReplApp<'a> for WasmReplApp<'a> {
    type Memory = WasmMemory<'a>;

    /// Run user code that returns a type with a `Builtin` layout
    /// Size of the return value is statically determined from its Rust type
    /// The `transform` callback takes the app's memory and the returned value
    fn call_function<Return, F>(&self, _main_fn_name: &str, _transform: F) -> Expr<'a>
    where
        F: Fn(&'a Self::Memory, Return) -> Expr<'a>,
        Self::Memory: 'a,
    {
        todo!()
    }

    /// Run user code that returns a struct or union, whose size is provided as an argument
    /// The `transform` callback takes the app's memory and the address of the returned value
    fn call_function_dynamic_size<T, F>(
        &self,
        _main_fn_name: &str,
        _ret_bytes: usize,
        _transform: F,
    ) -> T
    where
        F: Fn(&'a Self::Memory, usize) -> T,
        Self::Memory: 'a,
    {
        todo!()
    }
}

macro_rules! deref_number {
    ($name: ident, $t: ty) => {
        fn $name(&self, address: usize) -> $t {
            const N: usize = size_of::<$t>();
            let mut array = [0; N];
            array.copy_from_slice(&self.copied_bytes[address..][..N]);
            <$t>::from_le_bytes(array)
        }
    };
}

impl<'a> ReplAppMemory for WasmMemory<'a> {
    fn deref_bool(&self, address: usize) -> bool {
        self.copied_bytes[address] != 0
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
        let bytes = &self.copied_bytes[elems_addr..][..len];
        std::str::from_utf8(bytes).unwrap()
    }
}
