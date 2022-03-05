use crate::roc::{ElemId, RocElem, RocElemTag};

#[derive(Debug, PartialEq, Eq)]
pub struct Focus {
    focused: Option<ElemId>,
    focused_ancestors: Vec<(ElemId, usize)>,
}

impl Default for Focus {
    fn default() -> Self {
        Self {
            focused: None,
            focused_ancestors: Vec::new(),
        }
    }
}

impl Focus {
    pub fn focused_elem(&self) -> Option<ElemId> {
        self.focused
    }

    /// e.g. the user pressed Tab.
    ///
    /// This is in contrast to next_local, which advances within a button group.
    /// For example, if I have three radio buttons in a group, pressing the
    /// arrow keys will cycle through them over and over without exiting the group -
    /// whereas pressing Tab will cycle through them once and then exit the group.
    pub fn next_global(&mut self, root: &RocElem) {
        match self.focused {
            Some(focused) => {
                // while let Some((ancestor_id, index)) = self.focused_ancestors.pop() {
                //     let ancestor = ancestor_id.elem();

                //     // TODO FIXME - right now this will re-traverse a lot of ground! To prevent this,
                //     // we should remember past indices searched, and tell the ancestors "hey stop searching when"
                //     // you reach these indices, because they were already covered previously.
                //     // One potentially easy way to do this: pass a min_index and max_index, and only look between those!
                //     //
                //     // Related idea: instead of doing .pop() here, iterate normally so we can `break;` after storing
                //     // `new_ancestors = Some(next_ancestors);` - this way, we still have access to the full ancestry, and
                //     // can maybe even pass it in to make it clear what work has already been done!
                //     if let Some((new_id, new_ancestors)) =
                //         Self::next_focusable_sibling(focused, Some(ancestor), Some(index))
                //     {
                //         // We found the next element to focus, so record that.
                //         self.focused = Some(new_id);

                //         // We got a path to the new focusable's ancestor(s), so add them to the path.
                //         // (This may restore some of the ancestors we've been .pop()-ing as we iterated.)
                //         self.focused_ancestors.extend(new_ancestors);

                //         return;
                //     }

                //     // Need to write a bunch of tests for this, especially tests of focus wrapping around - e.g.
                //     // what happens if it wraps around to a sibling? What happens if it wraps around to something
                //     // higher up the tree? Lower down the tree? What if nothing is focusable?
                //     // A separate question: what if we should have a separate text-to-speech concept separate from focus?
                // }
            }
            None => {
                // Nothing was focused in the first place, so try to focus the root.
                if root.is_focusable() {
                    self.focused = Some(root.id());
                    self.focused_ancestors = Vec::new();
                } else if let Some((new_id, new_ancestors)) =
                    Self::next_focusable_sibling(root, None, None)
                {
                    // If the root itself is not focusable, use its next focusable sibling.
                    self.focused = Some(new_id);
                    self.focused_ancestors = new_ancestors;
                }

                // Regardless of whether we found a focusable Elem, we're done.
                return;
            }
        }
    }

    /// Return the next focusable sibling element after this one.
    /// If this element has no siblings, or no *next* sibling after the given index
    /// (e.g. the given index refers to the last element in a Row element), return None.
    fn next_focusable_sibling(
        elem: &RocElem,
        ancestor: Option<&RocElem>,
        opt_index: Option<usize>,
    ) -> Option<(ElemId, Vec<(ElemId, usize)>)> {
        use RocElemTag::*;

        match elem.tag() {
            Button | Text => None,
            Row | Col => {
                let children = unsafe { &elem.entry().row_or_col.children.as_slice() };
                let iter = match opt_index {
                    Some(focus_index) => children[0..focus_index].iter(),
                    None => children.iter(),
                };

                for child in iter {
                    if let Some(focused) = Self::next_focusable_sibling(child, ancestor, None) {
                        return Some(focused);
                    }
                }

                None
            }
        }
    }
}

#[test]
fn next_focus_button_root() {
    use crate::roc::{ButtonStyles, RocElem};

    let child = RocElem::text("");
    let root = RocElem::button(ButtonStyles::default(), child);
    let mut focus = Focus::default();

    // At first, nothing should be focused.
    assert_eq!(focus.focused_elem(), std::ptr::null());

    focus.advance(&root);

    // Buttons should be focusable, so advancing focus should give the button focus.
    assert_eq!(focus.focused_elem(), &root as *const RocElem);

    // Since the button is at the root, advancing again should maintain focus on it.
    focus.advance(&root);
    assert_eq!(focus.focused_elem(), &root as *const RocElem);
}

#[test]
fn next_focus_text_root() {
    let root = RocElem::text("");
    let mut focus = Focus::default();

    // At first, nothing should be focused.
    assert_eq!(focus.focused_elem(), std::ptr::null());

    focus.advance(&root);

    // Text should not be focusable, so advancing focus should have no effect here.
    assert_eq!(focus.focused_elem(), std::ptr::null());

    // Just to double-check, advancing a second time should not change this.
    focus.advance(&root);
    assert_eq!(focus.focused_elem(), std::ptr::null());
}
