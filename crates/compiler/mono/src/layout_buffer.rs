use roc_collections::soa::Slice;

use crate::layout::Layout;

/// A look-aside buffer to keep layouts in.
#[derive(Default, Debug)]
pub struct LayoutBuffer<'a>(Vec<Layout<'a>>);

impl<'a> LayoutBuffer<'a> {
    pub fn reserve(&mut self, size: usize) -> Slice<Layout<'a>> {
        Slice::extend_new(&mut self.0, std::iter::repeat(Layout::VOID).take(size))
    }

    pub fn set_reserved<I>(&mut self, slice: Slice<Layout<'a>>, layouts: I)
    where
        I: IntoIterator<Item = Layout<'a>>,
        I::IntoIter: ExactSizeIterator,
    {
        let layouts = layouts.into_iter();
        debug_assert_eq!(layouts.len(), slice.len());
        for (index, layout) in slice.indices().zip(layouts) {
            self.0[index] = layout;
        }
    }

    pub fn iter_slice(&self, slice: Slice<Layout<'a>>) -> impl Iterator<Item = Layout<'a>> + '_ {
        self.0[slice.indices()].iter().copied()
    }
}
