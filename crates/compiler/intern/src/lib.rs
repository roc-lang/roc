//! Intern strings
#![warn(clippy::dbg_macro)]

pub mod sized_strings;

use sized_strings::{Str4, UsizeModulo};

pub struct Interns<Id> {
    str4s: Vec<Option<Str4>>,
    all_ids: Vec<Id>,
}

impl<Id> Interns<Id> {
    pub fn add(&mut self, string: &str) {
        match string.len() {
            1..=4 => {
                store_in_hashmap(&mut self.str4s, Str4::new(string));
            }
            5..=8 => {
                // TODO Str8
            }
            9..=16 => {
                // TODO Str16
            }
            _ => {
                debug_assert!(!string.is_empty());

                // TODO BigStr
            }
        }
    }
}

fn store_in_hashmap<'a, T: Copy + UsizeModulo>(vec: &'a mut Vec<Option<T>>, entry: T) {
    const MIN_CAPACITY: usize = 24;

    if vec.len() == vec.capacity() {
        vec.reserve(vec.len().min(MIN_CAPACITY) / 2);
    }

    let mut index = entry.usize_modulo(vec.len());

    loop {
        // Safety: index begins as a modulo of the length, so this must be in-bounds.
        // See safety note later for when index is modified.
        let slot = unsafe { vec.get_unchecked_mut(index) };

        if slot.is_none() {
            // It was empty, so we can store this string in it.
            *slot = Some(entry);
            break;
        } else {
            // It was taken. Increment index (linear probing), and wrap
            // around at the length.
            index += 1;

            if index >= vec.len() {
                // Safety: we know there must be an index 0, because the first thing
                // we did was to check for length == capacity, and if so, grow the Vec.
                index = 0;
            }
        }
    }
}
