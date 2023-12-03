use std::cmp::Ordering;

// inspired by https://docs.rs/generational-arena/0.2.8/generational_arena/struct.Arena.html#method.get2_mut
pub fn get2_mut<T>(slice: &mut [T], i: usize, j: usize) -> Option<(&mut T, &mut T)> {
    match i.cmp(&j) {
        Ordering::Less => {
            let (l, r) = slice.split_at_mut(j);
            Some((&mut l[i], &mut r[0]))
        }
        Ordering::Greater => {
            let (l, r) = slice.split_at_mut(i);
            Some((&mut r[0], &mut l[j]))
        }
        Ordering::Equal => None,
    }
}
