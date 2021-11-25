use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_module::symbol::Symbol;
use roc_types::subs::{Content, FlatType, Subs, Variable};

#[derive(Clone, Copy)]
struct Index<T> {
    index: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Index<T> {
    pub const fn new(index: u32) -> Self {
        Self {
            index,
            _marker: Default::default(),
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
            _marker: Default::default(),
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

impl Slice<Layout> {
    pub fn reserve(layouts: &mut Layouts, length: usize) -> Self {
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

impl Slice<Slice<Layout>> {
    pub fn reserve(layouts: &mut Layouts, length: usize) -> Self {
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
}

enum LayoutError {
    UnresolvedVariable(Variable),
    TypeError(()),
}

impl Layout {
    pub const UNIT: Self = Self::Struct(Slice::new(0, 0));
    pub const VOID: Self = Self::UnionNonRecursive(Slice::new(0, 0));

    pub const EMPTY_LIST: Self = Self::List(Layouts::VOID_INDEX);

    fn from_var(layouts: &mut Layouts, subs: &Subs, var: Variable) -> Result<Layout, LayoutError> {
        let content = &subs.get_ref(var).content;
        Self::from_content(layouts, subs, content)
    }

    fn from_content(
        layouts: &mut Layouts,
        subs: &Subs,
        content: &Content,
    ) -> Result<Layout, LayoutError> {
        match content {
            Content::FlexVar(_) => todo!(),
            Content::RigidVar(_) => todo!(),
            Content::RecursionVar {
                structure,
                opt_name,
            } => todo!(),
            Content::Structure(flat_type) => Self::from_flat_type(layouts, subs, flat_type),
            Content::Alias(_, _, actual) => {
                // at this point we throw away alias information
                Self::from_var(layouts, subs, *actual)
            }
            Content::Error => todo!(),
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
                match Self::from_var(layouts, subs, element_var) {
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

                Self::from_var(layouts, subs, *lambda_set)
            }
            FlatType::Record(_, _) => todo!(),
            FlatType::TagUnion(_, _) => todo!(),
            FlatType::FunctionOrTagUnion(_, _, _) => todo!(),
            FlatType::RecursiveTagUnion(_, _, _) => todo!(),
            FlatType::Erroneous(_) => todo!(),
            FlatType::EmptyRecord => Ok(Layout::UNIT),
            FlatType::EmptyTagUnion => Ok(Layout::VOID),
        }
    }
}
