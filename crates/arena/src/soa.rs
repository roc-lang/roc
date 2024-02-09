/// Builds a struct-of-arrays (SoA) type backed by indices into arenas. Basic example:
///
///     soa!(u16 Headers<'a> {
///         header_name: Vec<'a, &'a str>,
///         num_exports: Vec<'a, u32>,
///     });
///
/// Each field must be labeled as Vec<...> - non-Vec fields are not allowed. This expands to:
///
///     pub struct<'a> Headers<'a> {
///         header_name: ArenaRef<'a, &'a str>,
///         num_exports: ArenaRef<'a, u32>,
///         len: u16,
///         capacity: u16,
///     }
///
/// The length and capacity are u16 because that was the first argument to the soa! macro.
/// Each field shares the same len and capacity. Methods like Headers::push require being
/// passed every single field, and the push operation gets applied to each of the internal
///

#[macro_export]
macro_rules! soa {
    ($len_type:tt $name:ident<$($lifetime:tt),*> { $($field_name:ident : Vec<$field_lifetime:tt, $field_type:ty>),* $(,)? }) => {
        pub struct $name<$($lifetime,)*> {
            $($field_name: $crate::ArenaRefMut<$field_lifetime, $field_type>,)*
            len: $len_type,
            capacity: $len_type,
        }

        impl<$($lifetime,)*> $name<$($lifetime,)*> {
            pub fn push(&mut self, $($field_name:(&mut $crate::Arena<$field_lifetime>, $field_type)),*) {
                let current_len = self.len;
                let current_capacity = self.capacity;

                // If we're out of capacity, reserve more space in the arenas.
                if current_len >= current_capacity {
                    let additional_capacity = (current_capacity / 2).max(4);

                    self.reserve(additional_capacity, $($field_name.0,)*);
                }

                let index = current_len as usize;

                // Now that we have enough capacity, write the data in there.
                #[allow(clippy::unnecessary_cast)]
                unsafe {$(
                   *(self.$field_name.as_mut($field_name.0) as *mut $field_type).add(index) = $field_name.1;
                )*}

                // Now that we've written everything, increment the length
                self.len = current_len + 1;
            }

            pub fn reserve(&mut self, additional_capacity: $len_type, $($field_name: &mut $crate::Arena<$field_lifetime>,)*) {
                // Reserve space in all the arenas
                #[allow(clippy::unnecessary_cast)]
                {
                    let elems_to_reserve = additional_capacity as u32;

                    $(
                        let bytes_per_elem = core::mem::size_of::<$field_type>() as u32;

                        if let Some(bytes_to_reserve) = elems_to_reserve.checked_mul(bytes_per_elem) {
                            $field_name.reserve(bytes_to_reserve);
                        } else {
                            let todo = todo!("handle reserve overflow");
                        }
                    )*
                }

                match self.capacity.checked_add(additional_capacity) {
                    Some(new_capacity) => {
                        self.capacity = new_capacity;
                    }
                    None => {
                        todo!("capacity overflow; bail out because we can no longer safely write");
                    }
                }
            }

            pub fn len(&self) -> $len_type {
                self.len
            }

            pub fn is_empty(&self) -> bool {
                self.len == 0
            }

            pub fn capacity(&self) -> $len_type {
                self.capacity
            }

            pub fn with_capacity_in(_capacity: $len_type, $($field_name:&mut $crate::Arena<$field_lifetime>),*) -> Self{
                todo!("for with_capacity_in, need to multiply capacity elems by size_of::<$field_type> for each field type, sum them all up, and make a separate ArenaRefMut for each.");
            }

            pub fn as_mut_slices(&mut self, $($field_name:&mut $crate::Arena<$field_lifetime>),*) -> ($(&$field_lifetime mut [$field_type],)*) {
                #[allow(clippy::unnecessary_cast)]
                unsafe {
                    (
                        $(
                            core::slice::from_raw_parts_mut(
                                self.$field_name.as_mut($field_name) as *mut $field_type,
                                self.len as usize,
                            ),
                        )*
                    )
                }
            }

            pub fn as_slices(&mut self, $($field_name:&$crate::Arena<$field_lifetime>),*) -> ($(&$field_lifetime [$field_type],)*) {
                #[allow(clippy::unnecessary_cast)]
                unsafe {
                    (
                        $(
                            core::slice::from_raw_parts(
                                self.$field_name.as_ref($field_name) as *const $field_type,
                                self.len as usize,
                            ),
                        )*
                    )
                }
            }
        }
    };
}

soa!(u16 Ordered<'a> {
    start0: Vec<'a, &'a str>,
    start1: Vec<'a, u32>,
});
