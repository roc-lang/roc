use roc_collections::soa::Slice;

use crate::layout::Layout;

/// A look-aside buffer to keep layouts in.
#[derive(Default, Debug)]
pub struct LayoutBuffer<'a>(Vec<Layout<'a>>);

impl<'a> LayoutBuffer<'a> {
    pub fn reserve(&mut self, size: usize) -> Slice<Layout<'a>> {
        Slice::extend_new(&mut self.0, std::iter::repeat(Layout::VOID).take(size))
    }

    pub fn set_reserved(
        &mut self,
        slice: Slice<Layout<'a>>,
        layouts: impl ExactSizeIterator<Item = Layout<'a>>,
    ) {
        debug_assert_eq!(layouts.len(), slice.len());
        for (index, layout) in slice.indices().zip(layouts) {
            self.0[index] = layout;
        }
    }
}
