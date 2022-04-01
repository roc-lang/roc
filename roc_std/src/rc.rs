/// A type which uses reference counting for it's heap allocated memory.
///
/// Note that if a type doesn't allocate any heap memory (eg. `i32`), the
/// `increment` and `decrement` methods don't need to do anything.
///
/// # Safety
///
/// It must be safe to memcpy this type to a new location after the reference count has been increased.
pub unsafe trait ReferenceCount {
    /// Increment the reference count.
    fn increment(&self);
    /// Decrement the reference count.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `ptr` points to a value with a non-zero
    /// reference count.
    unsafe fn decrement(ptr: *const Self);
}

macro_rules! impl_reference_count_for_primitive {
    ($ty:ty) => {
        unsafe impl ReferenceCount for $ty {
            fn increment(&self) {
                // Do nothing.
            }

            unsafe fn decrement(_ptr: *const Self) {
                // Do nothing.
            }
        }
    };
}

impl_reference_count_for_primitive!(bool);
impl_reference_count_for_primitive!(char);
impl_reference_count_for_primitive!(u8);
impl_reference_count_for_primitive!(i8);
impl_reference_count_for_primitive!(u16);
impl_reference_count_for_primitive!(i16);
impl_reference_count_for_primitive!(u32);
impl_reference_count_for_primitive!(i32);
impl_reference_count_for_primitive!(u64);
impl_reference_count_for_primitive!(i64);
impl_reference_count_for_primitive!(u128);
impl_reference_count_for_primitive!(i128);
impl_reference_count_for_primitive!(f32);
impl_reference_count_for_primitive!(f64);

macro_rules! impl_reference_count_for_tuple {
    ($($ty:ident: $field:tt,)*) => {
        unsafe impl<$($ty),*> ReferenceCount for ($($ty,)*)
        where
            $($ty: ReferenceCount,)*
        {
            fn increment(&self) {
                $(self.$field.increment();)*
            }

            #[allow(unused_variables, clippy::unused_unit)]
            unsafe fn decrement(ptr: *const Self) {
                let ptrs = {
                    let this = &*ptr;
                    ($(core::ptr::addr_of!(this.$field),)*)
                };

                $($ty::decrement(ptrs.$field);)*
            }
        }
    };
}

impl_reference_count_for_tuple!();
impl_reference_count_for_tuple!(A: 0,);
impl_reference_count_for_tuple!(A: 0, B: 1,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10,);
impl_reference_count_for_tuple!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11,);

unsafe impl<T, const N: usize> ReferenceCount for [T; N]
where
    T: ReferenceCount,
{
    fn increment(&self) {
        self.iter().for_each(T::increment)
    }

    unsafe fn decrement(ptr: *const Self) {
        for i in 0..N {
            T::decrement(ptr.cast::<T>().add(i));
        }
    }
}
