#![crate_type = "lib"]

#[repr(C)]
pub struct Plant {
    pub x: i32,
    pub r#type: u32,
}

extern "C" {
    fn random_plant(seed: i32) -> Plant;
}

#[no_mangle]
pub extern "C" fn starting_plants() -> Vec<Plant> {
    (0..=14)
        .into_iter()
        .map(|i| unsafe { random_plant(i * 12) })
        .collect()
}
