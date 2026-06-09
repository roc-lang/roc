#![crate_type = "lib"]
use std::mem::MaybeUninit;

#[repr(C)]
pub struct Plant { pub x: i32, pub r#type: u32 }

type HostedFn = unsafe extern "C" fn(*mut RocOps, *mut Plant, *const u8);

#[repr(C)]
pub struct HostedFunctions {
    pub count: u64,
    pub fns: *const HostedFn,
}

#[repr(C)]
pub struct RocOps {
    _pad: [u8; 0x40],
    pub hosted_fns: *const HostedFn,   // table base pointer at offset 0x40
}

#[inline(never)]
unsafe fn random_plant(ops: *mut RocOps, seed: i32) -> Plant {
    let table = (*ops).hosted_fns;        // load table base from ops+0x40
    let f = *table;                        // fns[0]
    let mut args = [0u8; 16];
    args[0..4].copy_from_slice(&seed.to_le_bytes());  // marshal arg into buffer
    let mut ret = MaybeUninit::<Plant>::uninit();
    f(ops, ret.as_mut_ptr(), args.as_ptr());           // indirect call, result via out-ptr
    ret.assume_init()
}

#[no_mangle]
pub extern "C" fn starting_plants(ops: *mut RocOps) -> Vec<Plant> {
    (0..=14)
        .into_iter()
        .map(|i| unsafe { random_plant(ops, i * 12) })
        .collect()
}
