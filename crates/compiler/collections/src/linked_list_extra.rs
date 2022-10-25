// This is just to get the nightly drain_iter API for LinkedList.
// TODO delete this module and use normal drain_iter once it has stabilized!

use std::{collections::LinkedList, fmt, marker::PhantomData, ptr::NonNull};

/// The internal representation of std::collections::linked_list::LinkedList
struct InternalLinkedList<T> {
    head: Option<NonNull<Node<T>>>,
    tail: Option<NonNull<Node<T>>>,
    len: usize,
    marker: PhantomData<Box<Node<T>>>,
}

impl<T> InternalLinkedList<T> {
    unsafe fn unlink_node(&mut self, mut node: NonNull<Node<T>>) {
        let node = node.as_mut(); // this one is ours now, we can create an &mut.

        // Not creating new mutable (unique!) references overlapping `element`.
        match node.prev {
            Some(prev) => (*prev.as_ptr()).next = node.next,
            // this node is the head node
            None => self.head = node.next,
        };

        match node.next {
            Some(next) => (*next.as_ptr()).prev = node.prev,
            // this node is the tail node
            None => self.tail = node.prev,
        };

        self.len -= 1;
    }
}

struct Node<T> {
    next: Option<NonNull<Node<T>>>,
    prev: Option<NonNull<Node<T>>>,
    element: T,
}

// Implementation of the nightly DrainFilter API, Apache2 licensed:
// https://doc.rust-lang.org/std/collections/struct.LinkedList.html#method.drain_filter
// TODO replace this with normal drain_iter once it's stabilized.
pub fn drain_filter<'a, T, F>(list: &'a mut LinkedList<T>, filter: F) -> DrainFilter<'a, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    // This is the internal representation of std::collections::linked_list::LinkedList;
    let list = unsafe {
        std::mem::transmute::<&'a mut LinkedList<T>, &'a mut InternalLinkedList<T>>(list)
    };

    // avoid borrow issues.
    let it = list.head;
    let old_len = list.len;

    DrainFilter {
        list,
        it,
        pred: filter,
        idx: 0,
        old_len,
    }
}

pub struct DrainFilter<'a, T: 'a, F: 'a>
where
    F: FnMut(&mut T) -> bool,
{
    list: &'a mut InternalLinkedList<T>,
    it: Option<NonNull<Node<T>>>,
    pred: F,
    idx: usize,
    old_len: usize,
}

impl<T, F> Iterator for DrainFilter<'_, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        while let Some(mut node) = self.it {
            unsafe {
                self.it = node.as_ref().next;
                self.idx += 1;

                if (self.pred)(&mut node.as_mut().element) {
                    // `unlink_node` is okay with aliasing `element` references.
                    self.list.unlink_node(node);
                    return Some(Box::from_raw(node.as_ptr()).element);
                }
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.old_len - self.idx))
    }
}

impl<T, F> Drop for DrainFilter<'_, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    fn drop(&mut self) {
        struct DropGuard<'r, 'a, T, F>(&'r mut DrainFilter<'a, T, F>)
        where
            F: FnMut(&mut T) -> bool;

        impl<'r, 'a, T, F> Drop for DropGuard<'r, 'a, T, F>
        where
            F: FnMut(&mut T) -> bool,
        {
            fn drop(&mut self) {
                self.0.for_each(drop);
            }
        }

        while let Some(item) = self.next() {
            let guard = DropGuard(self);
            drop(item);
            std::mem::forget(guard);
        }
    }
}

impl<T: fmt::Debug, F> fmt::Debug for DrainFilter<'_, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // This is the internal representation of std::collections::linked_list::LinkedList;
        let list =
            unsafe { std::mem::transmute::<&InternalLinkedList<T>, &LinkedList<T>>(&*self.list) };

        f.debug_tuple("DrainFilter").field(list).finish()
    }
}
