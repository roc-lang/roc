struct KnownSizeIteratorImpl<T, I>(I, usize)
where
    I: Iterator<Item = T>;

impl<T, I> Iterator for KnownSizeIteratorImpl<T, I>
where
    I: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<T, I> ExactSizeIterator for KnownSizeIteratorImpl<T, I>
where
    I: Iterator<Item = T>,
{
    fn len(&self) -> usize {
        self.1
    }
}

pub struct KnownSizeIterator {}

macro_rules! impl_from {
    ($name:ident, $($i:ident: $tv:ident),*) => {
        impl KnownSizeIterator {
            pub fn $name<T, $($tv,)*>($($i: $tv,)*) -> impl ExactSizeIterator<Item = T>
            where
                $($tv: IntoIterator<Item = T>, $tv::IntoIter: ExactSizeIterator,)*
            {
                $(let $i = $i.into_iter();)*
                let len = 0 $(+ $i.len())*;
                KnownSizeIteratorImpl(std::iter::empty()$(.chain($i))*, len)
            }
        }
    }
}

impl_from!(from_2, i0: I0, i1: I1);
impl_from!(from_3, i0: I0, i1: I1, i2: I2);
