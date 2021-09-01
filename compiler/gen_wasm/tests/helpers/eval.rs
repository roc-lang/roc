use roc_can::builtins::builtin_defs_map;
use roc_collections::all::{MutMap, MutSet};
use roc_std::{RocDec, RocList, RocOrder, RocStr};

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app \"test\" provides [ main ] to \"./platform\"\n\nmain =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

#[allow(dead_code)]
pub fn helper_wasm<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    _is_gen_test: bool,
    _ignore_problems: bool,
) -> wasmer::Instance {
    use std::path::{Path, PathBuf};

    let filename = PathBuf::from("Test.roc");
    let src_dir = Path::new("fake/test/path");

    let module_src;
    let temp;
    if src.starts_with("app") {
        // this is already a module
        module_src = src;
    } else {
        // this is an expression, promote it to a module
        temp = promote_expr_to_module(src);
        module_src = &temp;
    }

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        stdlib,
        src_dir,
        exposed_types,
        8,
        builtin_defs_map,
    );

    let loaded = loaded.expect("failed to load module");

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        procedures: top_procedures,
        interns,
        exposed_to_host,
        ..
    } = loaded;

    let mut procedures = MutMap::default();

    for (key, proc) in top_procedures {
        procedures.insert(key, proc);
    }

    let exposed_to_host = exposed_to_host.keys().copied().collect::<MutSet<_>>();

    let env = gen_wasm::Env {
        arena,
        interns,
        exposed_to_host,
    };

    let module_bytes = gen_wasm::build_module(&env, procedures).unwrap();

    // for debugging (e.g. with wasm2wat)
    if false {
        use std::io::Write;
        let mut file = std::fs::File::create("/home/folkertdev/roc/wasm/manual.wasm").unwrap();
        file.write_all(&module_bytes).unwrap();
    }

    // now, do wasmer stuff

    use wasmer::{Function, Instance, Module, Store};

    let store = Store::default();
    // let module = Module::from_file(&store, &test_wasm_path).unwrap();
    let module = Module::new(&store, &module_bytes).unwrap();

    // First, we create the `WasiEnv`
    use wasmer_wasi::WasiState;
    let mut wasi_env = WasiState::new("hello")
        // .args(&["world"])
        // .env("KEY", "Value")
        .finalize()
        .unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let mut import_object = wasi_env
        .import_object(&module)
        .unwrap_or_else(|_| wasmer::imports!());

    {
        let mut exts = wasmer::Exports::new();

        let main_function = Function::new_native(&store, fake_wasm_main_function);
        let ext = wasmer::Extern::Function(main_function);
        exts.insert("main", ext);

        let main_function = Function::new_native(&store, wasm_roc_panic);
        let ext = wasmer::Extern::Function(main_function);
        exts.insert("roc_panic", ext);

        import_object.register("env", exts);
    }

    Instance::new(&module, &import_object).unwrap()
}

#[allow(dead_code)]
fn wasm_roc_panic(address: u32, tag_id: u32) {
    match tag_id {
        0 => {
            let mut string = "";

            MEMORY.with(|f| {
                let memory = f.borrow().unwrap();

                let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(address);
                let width = 100;
                let c_ptr = (ptr.deref(memory, 0, width)).unwrap();

                use libc::c_char;
                use std::ffi::CStr;
                let slice = unsafe { CStr::from_ptr(c_ptr as *const _ as *const c_char) };
                string = slice.to_str().unwrap();
            });

            panic!("Roc failed with message: {:?}", string)
        }
        _ => todo!(),
    }
}

use std::cell::RefCell;

thread_local! {
    pub static MEMORY: RefCell<Option<&'static wasmer::Memory>> = RefCell::new(None);
}

#[allow(dead_code)]
fn fake_wasm_main_function(_: u32, _: u32) -> u32 {
    panic!("wasm entered the main function; this should never happen!")
}

#[allow(dead_code)]
pub fn assert_wasm_evals_to_help<T>(src: &str, ignore_problems: bool) -> Result<T, String>
where
    T: FromWasmMemory,
{
    let arena = bumpalo::Bump::new();

    // NOTE the stdlib must be in the arena; just taking a reference will segfault
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let is_gen_test = true;
    let instance =
        crate::helpers::eval::helper_wasm(&arena, src, stdlib, is_gen_test, ignore_problems);

    let memory = instance.exports.get_memory("memory").unwrap();

    crate::helpers::eval::MEMORY.with(|f| {
        *f.borrow_mut() = Some(unsafe { std::mem::transmute(memory) });
    });

    let test_wrapper = instance.exports.get_function("test_wrapper").unwrap();

    match test_wrapper.call(&[]) {
        Err(e) => Err(format!("{:?}", e)),
        Ok(result) => {
            let address = match result[0] {
                wasmer::Value::I32(a) => a,
                _ => panic!(),
            };

            let output = <T as crate::helpers::eval::FromWasmMemory>::decode(
                memory,
                // skip the RocCallResult tag id
                address as u32 + 8,
            );

            Ok(output)
        }
    }
}

#[macro_export]
macro_rules! assert_wasm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems:expr) => {
        match $crate::helpers::eval::assert_wasm_evals_to_help::<$ty>($src, $ignore_problems) {
            Err(msg) => println!("{:?}", msg),
            Ok(actual) => {
                #[allow(clippy::bool_assert_comparison)]
                assert_eq!($transform(actual), $expected)
            }
        }
    };

    ($src:expr, $expected:expr, $ty:ty) => {
        $crate::assert_wasm_evals_to!($src, $expected, $ty, $crate::helpers::eval::identity, false);
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        $crate::assert_wasm_evals_to!($src, $expected, $ty, $transform, false);
    };
}

#[macro_export]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_evals_to!($src, $expected, $ty, $crate::helpers::eval::identity);
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            $crate::assert_wasm_evals_to!($src, $expected, $ty, $transform, false);
        }
    };
}

#[allow(dead_code)]
pub fn identity<T>(value: T) -> T {
    value
}

pub trait FromWasmMemory: Sized {
    const SIZE_OF_WASM: usize;
    const ALIGN_OF_WASM: usize;
    const ACTUAL_WIDTH: usize = if (Self::SIZE_OF_WASM % Self::ALIGN_OF_WASM) == 0 {
        Self::SIZE_OF_WASM
    } else {
        Self::SIZE_OF_WASM + (Self::ALIGN_OF_WASM - (Self::SIZE_OF_WASM % Self::ALIGN_OF_WASM))
    };

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self;
}

macro_rules! from_wasm_memory_primitive_decode {
    ($type_name:ident) => {
        const SIZE_OF_WASM: usize = core::mem::size_of::<$type_name>();
        const ALIGN_OF_WASM: usize = core::mem::align_of::<$type_name>();

        fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
            use core::mem::MaybeUninit;

            let mut output: MaybeUninit<Self> = MaybeUninit::uninit();
            let width = std::mem::size_of::<Self>();

            let ptr = output.as_mut_ptr();
            let raw_ptr = ptr as *mut u8;
            let slice = unsafe { std::slice::from_raw_parts_mut(raw_ptr, width) };

            let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(offset as u32);
            let foobar = (ptr.deref(memory, 0, width as u32)).unwrap();
            let wasm_slice = unsafe { std::mem::transmute(foobar) };

            slice.copy_from_slice(wasm_slice);

            unsafe { output.assume_init() }
        }
    };
}

macro_rules! from_wasm_memory_primitive {
    ($($type_name:ident ,)+) => {
        $(
            impl FromWasmMemory for $type_name {
                from_wasm_memory_primitive_decode!($type_name);
            }
        )*
    }
}

from_wasm_memory_primitive!(
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, f32, f64, bool, RocDec, RocOrder,
);

impl FromWasmMemory for () {
    const SIZE_OF_WASM: usize = 0;
    const ALIGN_OF_WASM: usize = 0;

    fn decode(_: &wasmer::Memory, _: u32) -> Self {}
}

impl FromWasmMemory for RocStr {
    const SIZE_OF_WASM: usize = 8;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let bytes = <u64 as FromWasmMemory>::decode(memory, offset);

        let length = (bytes >> 32) as u32;
        let elements = bytes as u32;

        if length == 0 {
            RocStr::default()
        } else if (length as i32) < 0 {
            // this is a small string
            let last_byte = bytes.to_ne_bytes()[7];
            let actual_length = (last_byte ^ 0b1000_0000) as usize;

            let slice = &bytes.to_ne_bytes()[..actual_length as usize];
            RocStr::from_slice(slice)
        } else {
            // this is a big string
            let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(elements);
            let foobar = (ptr.deref(memory, 0, length)).unwrap();
            let wasm_slice = unsafe { std::mem::transmute(foobar) };

            RocStr::from_slice(wasm_slice)
        }
    }
}

impl<T: FromWasmMemory + Clone> FromWasmMemory for RocList<T> {
    const SIZE_OF_WASM: usize = 8;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let bytes = <u64 as FromWasmMemory>::decode(memory, offset);

        let length = (bytes >> 32) as u32;
        let elements = bytes as u32;

        let mut items = Vec::with_capacity(length as usize);

        for i in 0..length {
            let item = <T as FromWasmMemory>::decode(
                memory,
                elements + i * <T as FromWasmMemory>::SIZE_OF_WASM as u32,
            );
            items.push(item);
        }

        RocList::from_slice(&items)
    }
}

impl<T: FromWasmMemory + Clone> FromWasmMemory for &'_ [T] {
    const SIZE_OF_WASM: usize = 8;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let bytes = <u64 as FromWasmMemory>::decode(memory, offset);

        let length = (bytes >> 32) as u32;
        let elements = bytes as u32;

        let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(elements);
        let width = <T as FromWasmMemory>::SIZE_OF_WASM as u32 * length;
        let foobar = (ptr.deref(memory, 0, width)).unwrap();
        let wasm_slice =
            unsafe { std::slice::from_raw_parts(foobar as *const _ as *const _, length as usize) };

        wasm_slice
    }
}

impl<T: FromWasmMemory> FromWasmMemory for &'_ T {
    const SIZE_OF_WASM: usize = 4;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let elements = <u32 as FromWasmMemory>::decode(memory, offset);

        let actual = <T as FromWasmMemory>::decode(memory, elements);

        let b = Box::new(actual);

        std::boxed::Box::<T>::leak(b)
    }
}

impl<T: FromWasmMemory + Clone, const N: usize> FromWasmMemory for [T; N] {
    const SIZE_OF_WASM: usize = N * T::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = T::ALIGN_OF_WASM;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(offset);
        let width = <T as FromWasmMemory>::SIZE_OF_WASM as u32 * N as u32;
        let foobar = (ptr.deref(memory, 0, width)).unwrap();
        let wasm_slice: &[T; N] = unsafe { &*(foobar as *const _ as *const [T; N]) };

        wasm_slice.clone()
    }
}

impl FromWasmMemory for usize {
    const SIZE_OF_WASM: usize = 4;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        <u32 as FromWasmMemory>::decode(memory, offset) as usize
    }
}

impl<T: FromWasmMemory, U: FromWasmMemory> FromWasmMemory for (T, U) {
    const SIZE_OF_WASM: usize = T::SIZE_OF_WASM + U::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = max2(T::SIZE_OF_WASM, U::SIZE_OF_WASM);

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasmMemory>::decode(memory, offset);

        let u = <U as FromWasmMemory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        (t, u)
    }
}

const fn max2(a: usize, b: usize) -> usize {
    if a > b {
        a
    } else {
        b
    }
}

const fn max3(a: usize, b: usize, c: usize) -> usize {
    max2(max2(a, b), c)
}

impl<T: FromWasmMemory, U: FromWasmMemory, V: FromWasmMemory> FromWasmMemory for (T, U, V) {
    const SIZE_OF_WASM: usize = T::SIZE_OF_WASM + U::SIZE_OF_WASM + V::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = max3(T::SIZE_OF_WASM, U::SIZE_OF_WASM, V::SIZE_OF_WASM);

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        assert!(
            U::ALIGN_OF_WASM >= V::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasmMemory>::decode(memory, offset);

        let u = <U as FromWasmMemory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        let v = <V as FromWasmMemory>::decode(
            memory,
            offset + T::ACTUAL_WIDTH as u32 + U::ACTUAL_WIDTH as u32,
        );

        (t, u, v)
    }
}
