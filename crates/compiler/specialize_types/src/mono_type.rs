use roc_module::symbol::Symbol;
use soa::{Id, Slice, Slice2};

/// For now, a heap-allocated string. In the future, this will be something else.
struct RecordFieldName(String);

pub struct MonoTypes {
    // TODO
}

impl MonoTypes {
    pub fn add_apply(
        &mut self,
        symbol: Symbol,
        args: impl IntoIterator<Item = Id<MonoType>>,
    ) -> Id<MonoType> {
        todo!("extend");
    }

    pub(crate) fn add_function(
        &self,
        new_closure: Id<MonoType>,
        new_ret: Id<MonoType>,
        new_args: impl IntoIterator<Item = Id<MonoType>>,
    ) -> Id<MonoType> {
        todo!("extend")
    }

    /// We only use the field labels for sorting later; we don't care what format they're in otherwise.
    pub(crate) fn add_record<Label: Ord>(
        &self,
        fields: impl Iterator<Item = (Label, Id<MonoType>)>,
    ) -> Id<MonoType> {
        todo!("extend")
    }
}

// TODO: we can make all of this take up a minimal amount of memory as follows:
// 1. Arrange it so that for each MonoType variant we need at most one length and one start index.
// 2. Store all MonoType discriminants in one array (there are only 5 of them, so u3 is plenty;
//    if we discard record field names, can unify record and tuple and use u2 for the 4 variants)
// 3. Store all the MonoType variant slice lengths in a separate array (u8 should be plenty)
// 4. Store all the MonoType start indices in a separate array (u32 should be plenty)

#[derive(Clone, Copy, Debug)]
pub enum MonoType {
    Apply {
        // TODO: the symbol can be stored adjacency style immediately before
        // the first type arg. (This means the args slice must always
        // have a valid start index even if there are no args.) This will
        // work regardless of whether the Symbl is 32 or 64 bits, because
        // its alignment will never be smaller than the alignment of an Index.
        symbol: Symbol,
        args: Slice<Id<MonoType>>,
    },

    Func {
        /// The first element in this slice is the capture type,
        /// followed by the return type, and then the rest are the
        /// function's arg types. These are all stored in one slice
        /// because having them adjacent is best for cache locality,
        /// and storing separate Ids for each would make this variant bigger.
        capture_ret_args: Slice<Id<MonoType>>,
    },

    /// Slice of (field_name, field_type) pairs.
    Record(
        // TODO: since both tuple elements are the same size and alignment,
        // we can store them as all of the one followed by all of the other,
        // and therefore store only length and start index.
        Slice2<Interned<String>, Id<MonoType>>,
    ),

    /// Each element in the slice represents a different tuple element.
    /// The index in the slice corresponds to the index in the tuple.
    Tuple(Slice<Id<MonoType>>),

    /// Slice of (tag_name, tag_payload_types) pairs
    TagUnion(Slice2<Interned<String>, Slice<Id<MonoType>>>),
}
