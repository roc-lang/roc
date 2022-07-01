use roc_can::expected::Expected;
use roc_can::expected::PExpected;

/// Clones the outer node, but does not clone any nodeids
pub trait ShallowClone {
    fn shallow_clone(&self) -> Self;
}

impl<T> ShallowClone for Expected<T>
where
    T: ShallowClone,
{
    fn shallow_clone(&self) -> Self {
        use Expected::*;

        match self {
            NoExpectation(t) => NoExpectation(t.shallow_clone()),
            ForReason(reason, t, region) => ForReason(reason.clone(), t.shallow_clone(), *region),
            FromAnnotation(loc_pat, n, source, t) => {
                FromAnnotation(loc_pat.clone(), *n, *source, t.shallow_clone())
            }
        }
    }
}

impl<T: ShallowClone> ShallowClone for PExpected<T> {
    fn shallow_clone(&self) -> Self {
        use PExpected::*;

        match self {
            NoExpectation(t) => NoExpectation(t.shallow_clone()),
            ForReason(reason, t, region) => ForReason(reason.clone(), t.shallow_clone(), *region),
        }
    }
}
