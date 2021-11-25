use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_types::subs::{Content, FlatType, Subs, Variable};
use roc_types::types::RecordField;
use std::collections::hash_map::Entry;

#[cfg(debug_assertions)]
use crate::layout::{ext_var_is_empty_record, ext_var_is_empty_tag_union};

#[derive(Clone, Copy)]
struct Index<T> {
    index: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Index<T> {
    pub const fn new(index: u32) -> Self {
        Self {
            index,
            _marker: std::marker::PhantomData,
        }
    }
}

#[derive(Clone, Copy)]
struct Slice<T> {
    start: u32,
    length: u16,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Slice<T> {
    pub const fn new(start: u32, length: u16) -> Self {
        Self {
            start,
            length,
            _marker: std::marker::PhantomData,
        }
    }

    pub const fn len(&self) -> usize {
        self.length as _
    }

    pub const fn is_empty(&self) -> bool {
        self.length == 0
    }

    pub const fn indices(&self) -> std::ops::Range<usize> {
        self.start as usize..(self.start as usize + self.length as usize)
    }
}

trait Reserve {
    fn reserve(layouts: &mut Layouts, length: usize) -> Self;
}

impl Index<Layout> {
    fn reserve(layouts: &mut Layouts) -> Self {
        let index = layouts.layouts.len() as u32;

        layouts.layouts.push(Layout::Reserved);

        Self {
            index,
            _marker: Default::default(),
        }
    }
}

impl Reserve for Slice<Layout> {
    fn reserve(layouts: &mut Layouts, length: usize) -> Self {
        let start = layouts.layouts.len() as u32;

        let it = std::iter::repeat(Layout::Reserved).take(length);
        layouts.layouts.extend(it);

        Self {
            start,
            length: length as u16,
            _marker: Default::default(),
        }
    }
}

impl Reserve for Slice<Slice<Layout>> {
    fn reserve(layouts: &mut Layouts, length: usize) -> Self {
        let start = layouts.layout_slices.len() as u32;

        let empty: Slice<Layout> = Slice::new(0, 0);
        let it = std::iter::repeat(empty).take(length);
        layouts.layout_slices.extend(it);

        Self {
            start,
            length: length as u16,
            _marker: Default::default(),
        }
    }
}

type SliceSlice = Slice<Slice<Layout>>;
type LayoutIndex = Index<Layout>;
type LambdaSetIndex = u32;
type NullableUnionIndex = u32;
type LayoutTuple = u32;
type SymbolSlice = (u32, u16);

static_assertions::assert_eq_size!([u8; 12], Layout);

pub struct Layouts {
    layouts: Vec<Layout>,
    layout_slices: Vec<Slice<Layout>>,
    function_layouts: Vec<(Slice<Layout>, LambdaSetIndex)>,
    lambda_sets: Vec<LambdaSet>,
    symbols: Vec<Symbol>,
    nullable_unions: Vec<SliceSlice>,
    recursion_variable_to_structure_variable_map: MutMap<Variable, Index<Layout>>,
    usize_int_width: IntWidth,
}

struct FunctionLayout {
    /// An index into the Layouts.function_layouts array:
    ///
    /// last element is the result, prior elements the arguments
    /// arguments_and_result: Slice<Layout>,
    /// lambda_set: LambdaSetIndex,
    index: u32,
}
struct LambdaSet {
    representation: LayoutIndex,
    set_symbols: SymbolSlice,
    set_layouts: SliceSlice,
}

#[derive(Clone, Copy)]
enum Layout {
    // theory: we can zero out memory to reserve space for many layouts
    Reserved,

    // Question: where to store signedness information?
    Int(IntWidth),
    Float(FloatWidth),
    Decimal,

    Str,
    Dict(LayoutTuple),
    Set(LayoutIndex),
    List(LayoutIndex),

    Struct(Slice<Layout>),

    Boxed(Index<Layout>),
    UnionNonRecursive(SliceSlice),
    UnionRecursive(SliceSlice),
    UnionNonNullableUnwrapped(Slice<Layout>),
    UnionNullableWrapper {
        data: NullableUnionIndex,
        tag_id: u16,
    },

    UnionNullableUnwrappedTrue(Slice<Layout>),
    UnionNullableUnwrappedFalse(Slice<Layout>),

    RecursivePointer,
}

impl Layouts {
    const VOID_INDEX: Index<Layout> = Index::new(0);
    const UNIT_INDEX: Index<Layout> = Index::new(1);

    /// sort a slice according to elements' alignment
    fn sort_slice(&mut self, slice: Slice<Layout>) {
        todo!()
    }

    fn usize(&self) -> Layout {
        Layout::Int(self.usize_int_width)
    }
}

enum LayoutError {
    UnresolvedVariable(Variable),
    TypeError(()),
}

impl Layout {
    pub const UNIT: Self = Self::Struct(Slice::new(0, 0));
    pub const VOID: Self = Self::UnionNonRecursive(Slice::new(0, 0));

    pub const EMPTY_LIST: Self = Self::List(Layouts::VOID_INDEX);

    pub fn from_var(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
    ) -> Result<Layout, LayoutError> {
        // so we can set some things/clean up
        Self::from_var_help(layouts, subs, var)
    }

    fn from_var_help(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
    ) -> Result<Layout, LayoutError> {
        let content = &subs.get_ref(var).content;
        Self::from_content(layouts, subs, var, content)
    }

    fn from_content(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
        content: &Content,
    ) -> Result<Layout, LayoutError> {
        use LayoutError::*;

        match content {
            Content::FlexVar(_) => Err(UnresolvedVariable(var)),
            Content::RigidVar(_) => Err(UnresolvedVariable(var)),
            Content::RecursionVar {
                structure,
                opt_name: _,
            } => {
                let structure = subs.get_root_key_without_compacting(*structure);

                let entry = layouts
                    .recursion_variable_to_structure_variable_map
                    .entry(structure);

                match entry {
                    Entry::Vacant(vacant) => {
                        let reserved = Index::new(layouts.layouts.len() as _);
                        layouts.layouts.push(Layout::Reserved);

                        vacant.insert(reserved);

                        let layout = Layout::from_var(layouts, subs, structure)?;

                        layouts.layouts[reserved.index as usize] = layout;

                        Ok(Layout::Boxed(reserved))
                    }
                    Entry::Occupied(occupied) => {
                        let index = occupied.get();

                        Ok(Layout::Boxed(*index))
                    }
                }
            }
            Content::Structure(flat_type) => Self::from_flat_type(layouts, subs, flat_type),
            Content::Alias(symbol, _, actual) => {
                let symbol = *symbol;

                if let Some(int_width) = IntWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Int(int_width));
                }

                if let Some(float_width) = FloatWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Float(float_width));
                }

                match symbol {
                    Symbol::NUM_DECIMAL | Symbol::NUM_AT_DECIMAL => Ok(Layout::Decimal),

                    Symbol::NUM_NAT | Symbol::NUM_NATURAL | Symbol::NUM_AT_NATURAL => {
                        Ok(layouts.usize())
                    }

                    _ => {
                        // at this point we throw away alias information
                        Self::from_var_help(layouts, subs, *actual)
                    }
                }
            }
            Content::Error => Err(TypeError(())),
        }
    }

    fn from_flat_type(
        layouts: &mut Layouts,
        subs: &Subs,
        flat_type: &FlatType,
    ) -> Result<Layout, LayoutError> {
        use LayoutError::*;

        match flat_type {
            FlatType::Apply(Symbol::LIST_LIST, arguments) => {
                debug_assert_eq!(arguments.len(), 1);

                let element_var = subs.variables[arguments.slice.start as usize];
                match Self::from_var_help(layouts, subs, element_var) {
                    Ok(element_layout) => {
                        let element_index = Index::new(layouts.layouts.len() as _);
                        layouts.layouts.push(element_layout);

                        Ok(Layout::List(element_index))
                    }
                    Err(UnresolvedVariable(_)) => Ok(Layout::EMPTY_LIST),
                    Err(other) => Err(other),
                }
            }

            FlatType::Apply(symbol, _) => {
                unreachable!("Symbol {:?} does not have a layout", symbol)
            }

            FlatType::Func(_arguments, lambda_set, _result) => {
                // in this case, a function (pointer) is represented by the environment it
                // captures: the lambda set

                Self::from_var_help(layouts, subs, *lambda_set)
            }
            FlatType::Record(fields, ext) => {
                debug_assert!(ext_var_is_empty_record(subs, *ext));

                let mut slice = Slice::reserve(layouts, fields.len());

                let mut non_optional_fields = 0;
                let it = slice.indices().zip(fields.iter_all());
                for (target_index, (_, field_index, var_index)) in it {
                    match subs.record_fields[field_index.start as usize] {
                        RecordField::Optional(_) => {
                            // do nothing
                        }
                        RecordField::Required(_) | RecordField::Demanded(_) => {
                            let var = subs.variables[var_index.start as usize];
                            let layout = Layout::from_var_help(layouts, subs, var)?;

                            layouts.layouts[target_index] = layout;

                            non_optional_fields += 1;
                        }
                    }
                }

                // we have some wasted space in the case of optional fields; so be it
                slice.length = non_optional_fields;

                layouts.sort_slice(slice);

                Ok(Layout::Struct(slice))
            }
            FlatType::TagUnion(union_tags, ext) => {
                debug_assert!(ext_var_is_empty_tag_union(subs, *ext));

                let slices =
                    Self::from_slice_variable_slice(layouts, subs, union_tags.variables())?;

                Ok(Layout::UnionNonRecursive(slices))
            }

            FlatType::FunctionOrTagUnion(_, _, ext) => {
                debug_assert!(ext_var_is_empty_tag_union(subs, *ext));

                // at this point we know this is a tag
                Ok(Layout::UNIT)
            }
            FlatType::RecursiveTagUnion(rec_var, union_tags, ext) => {
                debug_assert!(ext_var_is_empty_tag_union(subs, *ext));

                let rec_var = subs.get_root_key_without_compacting(*rec_var);

                let cached = layouts
                    .recursion_variable_to_structure_variable_map
                    .get(&rec_var);

                if let Some(layout_index) = cached {
                    match layouts.layouts[layout_index.index as usize] {
                        Layout::Reserved => {
                            // we have to do the work here to fill this reserved variable in
                        }
                        other => {
                            return Ok(other);
                        }
                    }
                }

                let slices =
                    Self::from_slice_variable_slice(layouts, subs, union_tags.variables())?;

                Ok(Layout::UnionRecursive(slices))
            }
            FlatType::Erroneous(_) => Err(TypeError(())),
            FlatType::EmptyRecord => Ok(Layout::UNIT),
            FlatType::EmptyTagUnion => Ok(Layout::VOID),
        }
    }

    fn from_slice_variable_slice(
        layouts: &mut Layouts,
        subs: &Subs,
        slice_variable_slice: roc_types::subs::SubsSlice<roc_types::subs::VariableSubsSlice>,
    ) -> Result<Slice<Slice<Layout>>, LayoutError> {
        let slice = Slice::reserve(layouts, slice_variable_slice.len());

        let variable_slices = &subs.variable_slices[slice_variable_slice.indices()];
        let it = slice.indices().zip(variable_slices);
        for (target_index, variable_slice) in it {
            let layout_slice = Layout::from_variable_slice(layouts, subs, *variable_slice)?;
            layouts.layout_slices[target_index] = layout_slice;
        }

        Ok(slice)
    }

    fn from_variable_slice(
        layouts: &mut Layouts,
        subs: &Subs,
        variable_subs_slice: roc_types::subs::VariableSubsSlice,
    ) -> Result<Slice<Layout>, LayoutError> {
        let slice = Slice::reserve(layouts, variable_subs_slice.len());

        let variable_slice = &subs.variables[variable_subs_slice.indices()];
        let it = slice.indices().zip(variable_slice);
        for (target_index, var) in it {
            let layout = Layout::from_var_help(layouts, subs, *var)?;
            layouts.layouts[target_index] = layout;
        }

        layouts.sort_slice(slice);

        Ok(slice)
    }
}
