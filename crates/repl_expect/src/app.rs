use roc_parse::ast::Expr;
use roc_repl_eval::{ReplApp, ReplAppMemory};
use roc_std::RocStr;
use roc_target::Target;

pub(crate) struct ExpectMemory {
    pub(crate) start: *const u8,
}

macro_rules! deref_number {
    ($name: ident, $t: ty) => {
        fn $name(&self, addr: usize) -> $t {
            let ptr = unsafe { self.start.add(addr) } as *const _;
            unsafe { std::ptr::read_unaligned(ptr) }
        }
    };
}

impl ReplAppMemory for ExpectMemory {
    fn deref_bool(&self, addr: usize) -> bool {
        let ptr = unsafe { self.start.add(addr) };
        let value = unsafe { std::ptr::read_unaligned(ptr) };

        assert!(value == 0 || value == 1, "Memory shenanigans detected: bool values should only ever be 0 or 1. This may not be fixed in the old compiler, see also <https://roc.zulipchat.com/#narrow/channel/231634-beginners/topic/Segmentation.20fault.20during.20exercism.20tests.20for.20phone-number/near/515715647>.");

        value != 0
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
        const WIDTH: usize = 3 * std::mem::size_of::<usize>();

        let last_byte_addr = addr + WIDTH - 1;
        let last_byte = self.deref_i8(last_byte_addr);

        let is_small = last_byte < 0;

        if is_small {
            let ptr = unsafe { self.start.add(addr) };
            let roc_str: &RocStr = unsafe { &*ptr.cast() };

            roc_str.as_str()
        } else {
            let offset = self.deref_usize(addr);
            let seamless_slice_mask = usize::MAX >> 1;
            let length =
                self.deref_usize(addr + std::mem::size_of::<usize>()) & seamless_slice_mask;
            let _capacity = self.deref_usize(addr + 2 * std::mem::size_of::<usize>());

            unsafe {
                let ptr = self.start.add(offset);
                let slice = std::slice::from_raw_parts(ptr, length);

                std::str::from_utf8_unchecked(slice)
            }
        }
    }

    fn deref_pointer_with_tag_id(&self, addr: usize) -> (u16, u64) {
        // because addr is an index/offset, we cannot use the low bits
        let tag_id = self.deref_u32(addr);
        let offset = self.deref_u32(addr + 4);

        (tag_id as _, offset as _)
    }
}

pub(crate) struct ExpectReplApp<'a> {
    pub(crate) memory: &'a ExpectMemory,
    pub(crate) offset: usize,
}

impl<'a> ReplApp<'a> for ExpectReplApp<'a> {
    type Memory = ExpectMemory;

    /// Run user code that returns a type with a `Builtin` layout
    /// Size of the return value is statically determined from its Rust type
    /// The `transform` callback takes the app's memory and the returned value
    /// _main_fn_name is always the same and we don't use it here
    fn call_function<Return, F>(&mut self, _main_fn_name: &str, mut transform: F) -> Expr<'a>
    where
        F: FnMut(&'a Self::Memory, Return) -> Expr<'a>,
        Self::Memory: 'a,
    {
        let result: Return = unsafe {
            let ptr = self.memory.start.add(self.offset);
            let ptr: *const Return = std::mem::transmute(ptr);
            ptr.read()
        };

        transform(self.memory, result)
    }

    fn call_function_returns_roc_list<F>(&mut self, main_fn_name: &str, transform: F) -> Expr<'a>
    where
        F: FnMut(&'a Self::Memory, (usize, usize, usize)) -> Expr<'a>,
        Self::Memory: 'a,
    {
        self.call_function(main_fn_name, transform)
    }

    fn call_function_returns_roc_str<T, F>(
        &mut self,
        _target: Target,
        main_fn_name: &str,
        transform: F,
    ) -> Option<T>
    where
        F: Fn(&'a Self::Memory, usize) -> T,
        Self::Memory: 'a,
    {
        self.call_function_dynamic_size(main_fn_name, 24, transform)
    }

    /// Run user code that returns a struct or union, whose size is provided as an argument
    /// The `transform` callback takes the app's memory and the address of the returned value
    /// _main_fn_name and _ret_bytes are only used for the CLI REPL. For Wasm they are compiled-in
    /// to the test_wrapper function of the app itself
    fn call_function_dynamic_size<T, F>(
        &mut self,
        _main_fn_name: &str,
        _ret_bytes: usize,
        mut transform: F,
    ) -> Option<T>
    where
        F: FnMut(&'a Self::Memory, usize) -> T,
        Self::Memory: 'a,
    {
        Some(transform(self.memory, self.offset))
    }
}
