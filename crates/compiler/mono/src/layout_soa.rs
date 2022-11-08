use crate::layout::{ext_var_is_empty_record, ext_var_is_empty_tag_union};
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_target::TargetInfo;
use roc_types::subs::{self, Content, FlatType, Subs, Variable};
use roc_types::types::RecordField;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Index<T> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Slice<T> {
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

    pub fn into_iter(&self) -> impl Iterator<Item = Index<T>> {
        self.indices().map(|i| Index::new(i as _))
    }
}

trait Reserve {
    fn reserve(layouts: &mut Layouts, length: usize) -> Self;
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

static_assertions::assert_eq_size!([u8; 12], Layout);

pub struct Layouts {
    layouts: Vec<Layout>,
    layout_slices: Vec<Slice<Layout>>,
    // function_layouts: Vec<(Slice<Layout>, Index<LambdaSet>)>,
    lambda_sets: Vec<LambdaSet>,
    symbols: Vec<Symbol>,
    recursion_variable_to_structure_variable_map: MutMap<Variable, Index<Layout>>,
    target_info: TargetInfo,
}

pub struct FunctionLayout {
    /// last element is the result, prior elements the arguments
    arguments_and_result: Slice<Layout>,
    pub lambda_set: Index<LambdaSet>,
}

impl FunctionLayout {
    pub fn from_var(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
    ) -> Result<Self, LayoutError> {
        // so we can set some things/clean up
        Self::from_var_help(layouts, subs, var)
    }

    fn from_var_help(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
    ) -> Result<Self, LayoutError> {
        let content = &subs.get_content_without_compacting(var);
        Self::from_content(layouts, subs, var, content)
    }

    fn from_content(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
        content: &Content,
    ) -> Result<Self, LayoutError> {
        use LayoutError::*;

        match content {
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => Err(UnresolvedVariable(var)),
            Content::RecursionVar { .. } => Err(TypeError(())),
            Content::LambdaSet(lset) => Self::from_lambda_set(layouts, subs, *lset),
            Content::Structure(flat_type) => Self::from_flat_type(layouts, subs, flat_type),
            Content::Alias(_, _, actual, _) => Self::from_var_help(layouts, subs, *actual),
            Content::RangedNumber(_) => todo!(),
            Content::Error => Err(TypeError(())),
        }
    }

    fn from_lambda_set(
        _layouts: &mut Layouts,
        _subs: &Subs,
        _lset: subs::LambdaSet,
    ) -> Result<Self, LayoutError> {
        todo!();
    }

    fn from_flat_type(
        layouts: &mut Layouts,
        subs: &Subs,
        flat_type: &FlatType,
    ) -> Result<Self, LayoutError> {
        match flat_type {
            FlatType::Func(arguments, lambda_set, result) => {
                let slice = Slice::reserve(layouts, arguments.len() + 1);

                let variable_slice = &subs.variables[arguments.indices()];
                let it = slice.indices().zip(variable_slice);
                for (target_index, var) in it {
                    let layout = Layout::from_var_help(layouts, subs, *var)?;
                    layouts.layouts[target_index] = layout;
                }

                let result_layout = Layout::from_var_help(layouts, subs, *result)?;
                let result_index: Index<Layout> = Index::new(slice.start + slice.len() as u32 - 1);
                layouts.layouts[result_index.index as usize] = result_layout;

                let lambda_set = LambdaSet::from_var(layouts, subs, *lambda_set)?;
                let lambda_set_index = Index::new(layouts.lambda_sets.len() as u32);
                layouts.lambda_sets.push(lambda_set);

                Ok(Self {
                    arguments_and_result: slice,
                    lambda_set: lambda_set_index,
                })
            }

            _ => todo!(),
        }
    }

    pub fn argument_slice(&self) -> Slice<Layout> {
        let mut result = self.arguments_and_result;
        result.length -= 1;

        result
    }
    pub fn result_index(&self) -> Index<Layout> {
        Index::new(self.arguments_and_result.start + self.arguments_and_result.length as u32 - 1)
    }
}

/// Idea: don't include the symbols for the first 3 cases in --optimize mode
pub enum LambdaSet {
    Empty {
        symbol: Index<Symbol>,
    },
    Single {
        symbol: Index<Symbol>,
        layout: Index<Layout>,
    },
    Struct {
        symbol: Index<Symbol>,
        layouts: Slice<Layout>,
    },
    Union {
        symbols: Slice<Symbol>,
        layouts: Slice<Slice<Layout>>,
    },
}

impl LambdaSet {
    pub fn from_var(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
    ) -> Result<Self, LayoutError> {
        // so we can set some things/clean up
        Self::from_var_help(layouts, subs, var)
    }

    fn from_var_help(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
    ) -> Result<Self, LayoutError> {
        let content = &subs.get_content_without_compacting(var);
        Self::from_content(layouts, subs, var, content)
    }

    fn from_content(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
        content: &Content,
    ) -> Result<Self, LayoutError> {
        use LayoutError::*;

        match content {
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => Err(UnresolvedVariable(var)),
            Content::RecursionVar { .. } => {
                unreachable!("lambda sets cannot currently be recursive")
            }
            Content::LambdaSet(lset) => Self::from_lambda_set(layouts, subs, *lset),
            Content::Structure(_flat_type) => unreachable!(),
            Content::Alias(_, _, actual, _) => Self::from_var_help(layouts, subs, *actual),
            Content::RangedNumber(_) => todo!(),
            Content::Error => Err(TypeError(())),
        }
    }

    fn from_lambda_set(
        layouts: &mut Layouts,
        subs: &Subs,
        lset: subs::LambdaSet,
    ) -> Result<Self, LayoutError> {
        let subs::LambdaSet {
            solved,
            recursion_var: _,
            unspecialized: _,
            ambient_function: _,
        } = lset;

        // TODO: handle unspecialized

        debug_assert!(
            !solved.is_empty(),
            "lambda set must contain atleast the function itself"
        );

        let lambda_names = solved.labels();
        let closure_names = Self::get_closure_names(layouts, subs, lambda_names);

        let variables = solved.variables();
        if variables.len() == 1 {
            let symbol = subs.symbol_names[lambda_names.start as usize];
            let symbol_index = Index::new(layouts.symbols.len() as u32);
            layouts.symbols.push(symbol);
            let variable_slice = subs.variable_slices[variables.start as usize];

            match variable_slice.len() {
                0 => Ok(LambdaSet::Empty {
                    symbol: symbol_index,
                }),
                1 => {
                    let var = subs.variables[variable_slice.start as usize];
                    let layout = Layout::from_var(layouts, subs, var)?;

                    let index = Index::new(layouts.layouts.len() as u32);
                    layouts.layouts.push(layout);

                    Ok(LambdaSet::Single {
                        symbol: symbol_index,
                        layout: index,
                    })
                }
                _ => {
                    let slice = Layout::from_variable_slice(layouts, subs, variable_slice)?;

                    Ok(LambdaSet::Struct {
                        symbol: symbol_index,
                        layouts: slice,
                    })
                }
            }
        } else {
            let layouts = Layout::from_slice_variable_slice(layouts, subs, solved.variables())?;

            Ok(LambdaSet::Union {
                symbols: closure_names,
                layouts,
            })
        }
    }

    fn get_closure_names(
        layouts: &mut Layouts,
        subs: &Subs,
        subs_slice: roc_types::subs::SubsSlice<Symbol>,
    ) -> Slice<Symbol> {
        let slice = Slice::new(layouts.symbols.len() as u32, subs_slice.len() as u16);

        let symbols = &subs.symbol_names[subs_slice.indices()];

        for symbol in symbols {
            layouts.symbols.push(*symbol);
        }

        slice
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Layout {
    // theory: we can zero out memory to reserve space for many layouts
    Reserved,

    // Question: where to store signedness information?
    Int(IntWidth),
    Float(FloatWidth),
    Decimal,

    Str,
    Dict(Index<(Layout, Layout)>),
    Set(Index<Layout>),
    List(Index<Layout>),

    Struct(Slice<Layout>),

    UnionNonRecursive(Slice<Slice<Layout>>),

    Boxed(Index<Layout>),
    UnionRecursive(Slice<Slice<Layout>>),
    //    UnionNonNullableUnwrapped(Slice<Layout>),
    //    UnionNullableWrapper {
    //        data: NullableUnionIndex,
    //        tag_id: u16,
    //    },
    //
    //    UnionNullableUnwrappedTrue(Slice<Layout>),
    //    UnionNullableUnwrappedFalse(Slice<Layout>),

    // RecursivePointer,
}

fn round_up_to_alignment(unaligned: u16, alignment_bytes: u16) -> u16 {
    let unaligned = unaligned as i32;
    let alignment_bytes = alignment_bytes as i32;
    if alignment_bytes <= 1 {
        return unaligned as u16;
    }
    if alignment_bytes.count_ones() != 1 {
        panic!(
            "Cannot align to {} bytes. Not a power of 2.",
            alignment_bytes
        );
    }
    let mut aligned = unaligned;
    aligned += alignment_bytes - 1; // if lower bits are non-zero, push it over the next boundary
    aligned &= -alignment_bytes; // mask with a flag that has upper bits 1, lower bits 0

    aligned as u16
}

impl Layouts {
    const VOID_INDEX: Index<Layout> = Index::new(0);
    const VOID_TUPLE: Index<(Layout, Layout)> = Index::new(0);
    const UNIT_INDEX: Index<Layout> = Index::new(2);

    pub fn new(target_info: TargetInfo) -> Self {
        let mut layouts = Vec::with_capacity(64);

        layouts.push(Layout::VOID);
        layouts.push(Layout::VOID);
        layouts.push(Layout::UNIT);

        // sanity check
        debug_assert_eq!(layouts[Self::VOID_INDEX.index as usize], Layout::VOID);
        debug_assert_eq!(layouts[Self::VOID_TUPLE.index as usize + 1], Layout::VOID);
        debug_assert_eq!(layouts[Self::UNIT_INDEX.index as usize], Layout::UNIT);

        Layouts {
            layouts: Vec::default(),
            layout_slices: Vec::default(),
            lambda_sets: Vec::default(),
            symbols: Vec::default(),
            recursion_variable_to_structure_variable_map: MutMap::default(),
            target_info,
        }
    }

    /// sort a slice according to elements' alignment
    fn sort_slice_by_alignment(&mut self, layout_slice: Slice<Layout>) {
        let slice = &mut self.layouts[layout_slice.indices()];

        // SAFETY: the align_of function does not mutate the layouts vector
        // this unsafety is required to circumvent the borrow checker
        let sneaky_slice =
            unsafe { std::slice::from_raw_parts_mut(slice.as_mut_ptr(), slice.len()) };

        sneaky_slice.sort_by(|layout1, layout2| {
            let align1 = self.align_of_layout(*layout1);
            let align2 = self.align_of_layout(*layout2);

            // we want the biggest alignment first
            align2.cmp(&align1)
        });
    }

    fn usize(&self) -> Layout {
        let usize_int_width = match self.target_info.ptr_width() {
            roc_target::PtrWidth::Bytes4 => IntWidth::U32,
            roc_target::PtrWidth::Bytes8 => IntWidth::U64,
        };

        Layout::Int(usize_int_width)
    }

    fn align_of_layout_index(&self, index: Index<Layout>) -> u16 {
        let layout = self.layouts[index.index as usize];

        self.align_of_layout(layout)
    }

    fn align_of_layout(&self, layout: Layout) -> u16 {
        let usize_int_width = match self.target_info.ptr_width() {
            roc_target::PtrWidth::Bytes4 => IntWidth::U32,
            roc_target::PtrWidth::Bytes8 => IntWidth::U64,
        };

        let ptr_alignment = usize_int_width.alignment_bytes(self.target_info) as u16;

        match layout {
            Layout::Reserved => unreachable!(),
            Layout::Int(int_width) => int_width.alignment_bytes(self.target_info) as u16,
            Layout::Float(float_width) => float_width.alignment_bytes(self.target_info) as u16,
            Layout::Decimal => IntWidth::U128.alignment_bytes(self.target_info) as u16,
            Layout::Str | Layout::Dict(_) | Layout::Set(_) | Layout::List(_) => ptr_alignment,
            Layout::Struct(slice) => self.align_of_layout_slice(slice),
            Layout::Boxed(_) | Layout::UnionRecursive(_) => ptr_alignment,
            Layout::UnionNonRecursive(slices) => {
                let tag_id_align = IntWidth::I64.alignment_bytes(self.target_info) as u16;

                self.align_of_layout_slices(slices).max(tag_id_align)
            }
//            Layout::UnionNonNullableUnwrapped(_) => todo!(),
//            Layout::UnionNullableWrapper { data, tag_id } => todo!(),
//            Layout::UnionNullableUnwrappedTrue(_) => todo!(),
//            Layout::UnionNullableUnwrappedFalse(_) => todo!(),
//            Layout::RecursivePointer => todo!(),
        }
    }

    /// Invariant: the layouts are sorted from biggest to smallest alignment
    fn align_of_layout_slice(&self, slice: Slice<Layout>) -> u16 {
        match slice.into_iter().next() {
            None => 0,
            Some(first_index) => self.align_of_layout_index(first_index),
        }
    }

    fn align_of_layout_slices(&self, slice: Slice<Slice<Layout>>) -> u16 {
        slice
            .into_iter()
            .map(|index| self.layout_slices[index.index as usize])
            .map(|slice| self.align_of_layout_slice(slice))
            .max()
            .unwrap_or_default()
    }

    /// Invariant: the layouts are sorted from biggest to smallest alignment
    fn size_of_layout_slice(&self, slice: Slice<Layout>) -> u16 {
        match slice.into_iter().next() {
            None => 0,
            Some(first_index) => {
                let alignment = self.align_of_layout_index(first_index);

                let mut sum = 0;

                for index in slice.into_iter() {
                    sum += self.size_of_layout_index(index);
                }

                round_up_to_alignment(sum, alignment)
            }
        }
    }

    pub fn size_of_layout_index(&self, index: Index<Layout>) -> u16 {
        let layout = self.layouts[index.index as usize];

        self.size_of_layout(layout)
    }

    pub fn size_of_layout(&self, layout: Layout) -> u16 {
        let usize_int_width = match self.target_info.ptr_width() {
            roc_target::PtrWidth::Bytes4 => IntWidth::U32,
            roc_target::PtrWidth::Bytes8 => IntWidth::U64,
        };

        let ptr_width = usize_int_width.stack_size() as u16;

        match layout {
            Layout::Reserved => unreachable!(),
            Layout::Int(int_width) => int_width.stack_size() as _,
            Layout::Float(float_width) => float_width as _,
            Layout::Decimal => (std::mem::size_of::<roc_std::RocDec>()) as _,
            Layout::Str | Layout::Dict(_) | Layout::Set(_) | Layout::List(_) => 2 * ptr_width,
            Layout::Struct(slice) => self.size_of_layout_slice(slice),
            Layout::Boxed(_) | Layout::UnionRecursive(_) => ptr_width,
            Layout::UnionNonRecursive(slices) if slices.is_empty() => 0,
            Layout::UnionNonRecursive(slices) => {
                let tag_id = IntWidth::I64;

                let max_slice_size = slices
                    .into_iter()
                    .map(|index| self.layout_slices[index.index as usize])
                    .map(|slice| self.align_of_layout_slice(slice))
                    .max()
                    .unwrap_or_default();

                tag_id.stack_size() as u16 + max_slice_size
            }
//            Layout::UnionNonNullableUnwrapped(_) => todo!(),
//            Layout::UnionNullableWrapper { data, tag_id } => todo!(),
//            Layout::UnionNullableUnwrappedTrue(_) => todo!(),
//            Layout::UnionNullableUnwrappedFalse(_) => todo!(),
//            Layout::RecursivePointer => todo!(),
        }
    }
}

pub enum LayoutError {
    UnresolvedVariable(Variable),
    TypeError(()),
}

impl Layout {
    pub const UNIT: Self = Self::Struct(Slice::new(0, 0));
    pub const VOID: Self = Self::UnionNonRecursive(Slice::new(0, 0));

    pub const EMPTY_LIST: Self = Self::List(Layouts::VOID_INDEX);
    pub const EMPTY_DICT: Self = Self::Dict(Layouts::VOID_TUPLE);
    pub const EMPTY_SET: Self = Self::Set(Layouts::VOID_INDEX);

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
        let content = &subs.get_content_without_compacting(var);
        Self::from_content(layouts, subs, var, content)
    }

    /// Used in situations where an unspecialized variable is not a problem,
    /// and we can substitute with `[]`, the empty tag union.
    /// e.g. an empty list literal has type `List *`. We can still generate code
    /// in those cases by just picking any concrete type for the list element,
    /// and we pick the empty tag union in practice.
    fn from_var_help_or_void(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
    ) -> Result<Layout, LayoutError> {
        let content = &subs.get_content_without_compacting(var);

        match content {
            Content::FlexVar(_) | Content::RigidVar(_) => Ok(Layout::VOID),

            _ => Self::from_content(layouts, subs, var, content),
        }
    }

    fn from_content(
        layouts: &mut Layouts,
        subs: &Subs,
        var: Variable,
        content: &Content,
    ) -> Result<Layout, LayoutError> {
        use LayoutError::*;

        match content {
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _) => Err(UnresolvedVariable(var)),
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
            // Lambda set layout is same as tag union
            Content::LambdaSet(lset) => Self::from_lambda_set(layouts, subs, *lset),
            Content::Structure(flat_type) => Self::from_flat_type(layouts, subs, flat_type),
            Content::Alias(symbol, _, actual, _) => {
                let symbol = *symbol;

                if let Some(int_width) = IntWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Int(int_width));
                }

                if let Some(float_width) = FloatWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Float(float_width));
                }

                match symbol {
                    Symbol::NUM_DECIMAL => Ok(Layout::Decimal),

                    Symbol::NUM_NAT | Symbol::NUM_NATURAL => Ok(layouts.usize()),

                    _ => {
                        // at this point we throw away alias information
                        Self::from_var_help(layouts, subs, *actual)
                    }
                }
            }
            Content::RangedNumber(_) => todo!(),
            Content::Error => Err(TypeError(())),
        }
    }

    fn from_lambda_set(
        layouts: &mut Layouts,
        subs: &Subs,
        lset: subs::LambdaSet,
    ) -> Result<Layout, LayoutError> {
        let subs::LambdaSet {
            solved,
            recursion_var,
            unspecialized: _,
            ambient_function: _,
        } = lset;

        // TODO: handle unspecialized lambda set

        match recursion_var.into_variable() {
            Some(rec_var) => {
                let rec_var = subs.get_root_key_without_compacting(rec_var);

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

                let slices = Self::from_slice_variable_slice(layouts, subs, solved.variables())?;

                Ok(Layout::UnionRecursive(slices))
            }
            None => {
                let slices = Self::from_slice_variable_slice(layouts, subs, solved.variables())?;

                Ok(Layout::UnionNonRecursive(slices))
            }
        }
    }

    fn from_flat_type(
        layouts: &mut Layouts,
        subs: &Subs,
        flat_type: &FlatType,
    ) -> Result<Layout, LayoutError> {
        match flat_type {
            FlatType::Apply(Symbol::LIST_LIST, arguments) => {
                debug_assert_eq!(arguments.len(), 1);

                let element_var = subs.variables[arguments.start as usize];
                let element_layout = Self::from_var_help_or_void(layouts, subs, element_var)?;

                let element_index = Index::new(layouts.layouts.len() as _);
                layouts.layouts.push(element_layout);

                Ok(Layout::List(element_index))
            }

            FlatType::Apply(Symbol::DICT_DICT, arguments) => {
                debug_assert_eq!(arguments.len(), 2);

                let key_var = subs.variables[arguments.start as usize];
                let value_var = subs.variables[arguments.start as usize + 1];

                let key_layout = Self::from_var_help_or_void(layouts, subs, key_var)?;
                let value_layout = Self::from_var_help_or_void(layouts, subs, value_var)?;

                let index = Index::new(layouts.layouts.len() as _);
                layouts.layouts.push(key_layout);
                layouts.layouts.push(value_layout);

                Ok(Layout::Dict(index))
            }

            FlatType::Apply(Symbol::SET_SET, arguments) => {
                debug_assert_eq!(arguments.len(), 1);

                let element_var = subs.variables[arguments.start as usize];
                let element_layout = Self::from_var_help_or_void(layouts, subs, element_var)?;

                let element_index = Index::new(layouts.layouts.len() as _);
                layouts.layouts.push(element_layout);

                Ok(Layout::Set(element_index))
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
                    match subs.record_fields[field_index.index as usize] {
                        RecordField::Optional(_) | RecordField::RigidOptional(_) => {
                            // do nothing
                        }
                        RecordField::Required(_)
                        | RecordField::Demanded(_)
                        | RecordField::RigidRequired(_) => {
                            let var = subs.variables[var_index.index as usize];
                            let layout = Layout::from_var_help(layouts, subs, var)?;

                            layouts.layouts[target_index] = layout;

                            non_optional_fields += 1;
                        }
                    }
                }

                // we have some wasted space in the case of optional fields; so be it
                slice.length = non_optional_fields;

                layouts.sort_slice_by_alignment(slice);

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

        layouts.sort_slice_by_alignment(slice);

        Ok(slice)
    }
}
