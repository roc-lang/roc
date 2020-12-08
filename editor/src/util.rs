// Taken from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license
pub fn size_of_slice<T: Sized>(slice: &[T]) -> usize {
    std::mem::size_of::<T>() * slice.len()
}
