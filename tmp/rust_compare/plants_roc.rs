#![crate_type = "lib"]

use std::os::raw::c_void;
use std::ptr;

#[repr(C)]
pub struct Plant {
    pub x: i32,
    pub r#type: u32,
}

// Same memory order as the C PlantVec / Rust's RawVec: { cap, ptr, len }.
#[repr(C)]
pub struct PlantVec {
    pub cap: usize,
    pub ptr: *mut Plant,
    pub len: usize,
}

// Matches Roc's RocOps.roc_crashed ABI: (const RocCrashed *, void *env), noreturn.
#[repr(C)]
pub struct RocCrashed {
    pub utf8_bytes: *mut u8,
    pub len: usize,
}

extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn random_plant(seed: i32) -> Plant;
    fn handle_oom(crashed: *const RocCrashed, env: *mut c_void) -> !;
}

const PLANT_COUNT: usize = 15;

#[no_mangle]
pub extern "C" fn starting_plants() -> PlantVec {
    unsafe {
        let ptr = malloc(PLANT_COUNT * core::mem::size_of::<Plant>()) as *mut Plant;
        if ptr.is_null() {
            static MSG: &[u8] = b"Ran out of memory!";
            let crashed = RocCrashed {
                utf8_bytes: MSG.as_ptr() as *mut u8,
                len: MSG.len(),
            };
            handle_oom(&crashed, ptr::null_mut());
        }
        let mut i: i32 = 0;
        while (i as usize) < PLANT_COUNT {
            ptr.add(i as usize).write(random_plant(i * 12));
            i += 1;
        }
        PlantVec {
            cap: PLANT_COUNT,
            ptr,
            len: PLANT_COUNT,
        }
    }
}
