use crate::expected::{Expected, PExpected};
use roc_collections::soa::{Index, Slice};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;
use roc_types::types::{Category, PatternCategory, Type};

#[derive(Debug)]
pub struct Constraints {
    pub constraints: Vec<Constraint>,
    pub types: Vec<Type>,
    pub variables: Vec<Variable>,
    pub def_types: Vec<(Symbol, Loc<Index<Type>>)>,
    pub let_constraints: Vec<LetConstraint>,
    pub categories: Vec<Category>,
    pub pattern_categories: Vec<PatternCategory>,
    pub expectations: Vec<Expected<Type>>,
    pub pattern_expectations: Vec<PExpected<Type>>,
    pub includes_tags: Vec<IncludesTag>,
    pub strings: Vec<&'static str>,
}

impl Default for Constraints {
    fn default() -> Self {
        Self::new()
    }
}

impl Constraints {
    pub fn new() -> Self {
        let constraints = Vec::new();
        let mut types = Vec::new();
        let variables = Vec::new();
        let def_types = Vec::new();
        let let_constraints = Vec::new();
        let mut categories = Vec::with_capacity(16);
        let mut pattern_categories = Vec::with_capacity(16);
        let expectations = Vec::new();
        let pattern_expectations = Vec::new();
        let includes_tags = Vec::new();
        let strings = Vec::new();

        types.extend([Type::EmptyRec, Type::EmptyTagUnion]);

        categories.extend([
            Category::Record,
            Category::ForeignCall,
            Category::OpaqueArg,
            Category::Lambda,
            Category::ClosureSize,
            Category::StrInterpolation,
            Category::If,
            Category::When,
            Category::Float,
            Category::Int,
            Category::Num,
            Category::List,
            Category::Str,
            Category::Character,
        ]);

        pattern_categories.extend([
            PatternCategory::Record,
            PatternCategory::EmptyRecord,
            PatternCategory::PatternGuard,
            PatternCategory::PatternDefault,
            PatternCategory::Set,
            PatternCategory::Map,
            PatternCategory::Str,
            PatternCategory::Num,
            PatternCategory::Int,
            PatternCategory::Float,
            PatternCategory::Character,
        ]);

        Self {
            constraints,
            types,
            variables,
            def_types,
            let_constraints,
            categories,
            pattern_categories,
            expectations,
            pattern_expectations,
            includes_tags,
            strings,
        }
    }

    pub const EMPTY_RECORD: Index<Type> = Index::new(0);
    pub const EMPTY_TAG_UNION: Index<Type> = Index::new(1);

    pub const CATEGORY_RECORD: Index<Category> = Index::new(0);
    pub const CATEGORY_FOREIGNCALL: Index<Category> = Index::new(1);
    pub const CATEGORY_OPAQUEARG: Index<Category> = Index::new(2);
    pub const CATEGORY_LAMBDA: Index<Category> = Index::new(3);
    pub const CATEGORY_CLOSURESIZE: Index<Category> = Index::new(4);
    pub const CATEGORY_STRINTERPOLATION: Index<Category> = Index::new(5);
    pub const CATEGORY_IF: Index<Category> = Index::new(6);
    pub const CATEGORY_WHEN: Index<Category> = Index::new(7);
    pub const CATEGORY_FLOAT: Index<Category> = Index::new(8);
    pub const CATEGORY_INT: Index<Category> = Index::new(9);
    pub const CATEGORY_NUM: Index<Category> = Index::new(10);
    pub const CATEGORY_LIST: Index<Category> = Index::new(11);
    pub const CATEGORY_STR: Index<Category> = Index::new(12);
    pub const CATEGORY_CHARACTER: Index<Category> = Index::new(13);

    pub const PCATEGORY_RECORD: Index<PatternCategory> = Index::new(0);
    pub const PCATEGORY_EMPTYRECORD: Index<PatternCategory> = Index::new(1);
    pub const PCATEGORY_PATTERNGUARD: Index<PatternCategory> = Index::new(2);
    pub const PCATEGORY_PATTERNDEFAULT: Index<PatternCategory> = Index::new(3);
    pub const PCATEGORY_SET: Index<PatternCategory> = Index::new(4);
    pub const PCATEGORY_MAP: Index<PatternCategory> = Index::new(5);
    pub const PCATEGORY_STR: Index<PatternCategory> = Index::new(6);
    pub const PCATEGORY_NUM: Index<PatternCategory> = Index::new(7);
    pub const PCATEGORY_INT: Index<PatternCategory> = Index::new(8);
    pub const PCATEGORY_FLOAT: Index<PatternCategory> = Index::new(9);
    pub const PCATEGORY_CHARACTER: Index<PatternCategory> = Index::new(10);

    #[inline(always)]
    pub fn push_type(&mut self, typ: Type) -> Index<Type> {
        match typ {
            Type::EmptyRec => Self::EMPTY_RECORD,
            Type::EmptyTagUnion => Self::EMPTY_TAG_UNION,
            other => Index::push_new(&mut self.types, other),
        }
    }

    #[inline(always)]
    pub fn push_expected_type(&mut self, expected: Expected<Type>) -> Index<Expected<Type>> {
        Index::push_new(&mut self.expectations, expected)
    }

    #[inline(always)]
    pub fn push_category(&mut self, category: Category) -> Index<Category> {
        match category {
            Category::Record => Self::CATEGORY_RECORD,
            Category::ForeignCall => Self::CATEGORY_FOREIGNCALL,
            Category::OpaqueArg => Self::CATEGORY_OPAQUEARG,
            Category::Lambda => Self::CATEGORY_LAMBDA,
            Category::ClosureSize => Self::CATEGORY_CLOSURESIZE,
            Category::StrInterpolation => Self::CATEGORY_STRINTERPOLATION,
            Category::If => Self::CATEGORY_IF,
            Category::When => Self::CATEGORY_WHEN,
            Category::Float => Self::CATEGORY_FLOAT,
            Category::Int => Self::CATEGORY_INT,
            Category::Num => Self::CATEGORY_NUM,
            Category::List => Self::CATEGORY_LIST,
            Category::Str => Self::CATEGORY_STR,
            Category::Character => Self::CATEGORY_CHARACTER,
            other => Index::push_new(&mut self.categories, other),
        }
    }

    #[inline(always)]
    pub fn push_pattern_category(&mut self, category: PatternCategory) -> Index<PatternCategory> {
        match category {
            PatternCategory::Record => Self::PCATEGORY_RECORD,
            PatternCategory::EmptyRecord => Self::PCATEGORY_EMPTYRECORD,
            PatternCategory::PatternGuard => Self::PCATEGORY_PATTERNGUARD,
            PatternCategory::PatternDefault => Self::PCATEGORY_PATTERNDEFAULT,
            PatternCategory::Set => Self::PCATEGORY_SET,
            PatternCategory::Map => Self::PCATEGORY_MAP,
            PatternCategory::Str => Self::PCATEGORY_STR,
            PatternCategory::Num => Self::PCATEGORY_NUM,
            PatternCategory::Int => Self::PCATEGORY_INT,
            PatternCategory::Float => Self::PCATEGORY_FLOAT,
            PatternCategory::Character => Self::PCATEGORY_CHARACTER,
            other => Index::push_new(&mut self.pattern_categories, other),
        }
    }

    #[inline(always)]
    pub fn equal_types(
        &mut self,
        typ: Type,
        expected: Expected<Type>,
        category: Category,
        region: Region,
    ) -> Constraint {
        let type_index = Index::push_new(&mut self.types, typ);
        let expected_index = Index::push_new(&mut self.expectations, expected);
        let category_index = Self::push_category(self, category);

        Constraint::Eq(type_index, expected_index, category_index, region)
    }

    pub fn equal_pattern_types(
        &mut self,
        typ: Type,
        expected: PExpected<Type>,
        category: PatternCategory,
        region: Region,
    ) -> Constraint {
        let type_index = Index::push_new(&mut self.types, typ);
        let expected_index = Index::push_new(&mut self.pattern_expectations, expected);
        let category_index = Self::push_pattern_category(self, category);

        Constraint::Pattern(type_index, expected_index, category_index, region)
    }

    pub fn pattern_presence(
        &mut self,
        typ: Type,
        expected: PExpected<Type>,
        category: PatternCategory,
        region: Region,
    ) -> Constraint {
        let type_index = Index::push_new(&mut self.types, typ);
        let expected_index = Index::push_new(&mut self.pattern_expectations, expected);
        let category_index = Index::push_new(&mut self.pattern_categories, category);

        Constraint::PatternPresence(type_index, expected_index, category_index, region)
    }

    pub fn is_open_type(&mut self, typ: Type) -> Constraint {
        let type_index = Index::push_new(&mut self.types, typ);

        Constraint::IsOpenType(type_index)
    }

    pub fn includes_tag<I>(
        &mut self,
        typ: Type,
        tag_name: TagName,
        types: I,
        category: PatternCategory,
        region: Region,
    ) -> Constraint
    where
        I: IntoIterator<Item = Type>,
    {
        let type_index = Index::push_new(&mut self.types, typ);
        let category_index = Index::push_new(&mut self.pattern_categories, category);
        let types_slice = Slice::extend_new(&mut self.types, types);

        let includes_tag = IncludesTag {
            type_index,
            tag_name,
            types: types_slice,
            pattern_category: category_index,
            region,
        };

        let includes_tag_index = Index::push_new(&mut self.includes_tags, includes_tag);

        Constraint::IncludesTag(includes_tag_index)
    }

    fn variable_slice<I>(&mut self, it: I) -> Slice<Variable>
    where
        I: IntoIterator<Item = Variable>,
    {
        let start = self.variables.len();
        self.variables.extend(it);
        let length = self.variables.len() - start;

        Slice::new(start as _, length as _)
    }

    fn def_types_slice<I>(&mut self, it: I) -> Slice<(Symbol, Loc<Index<Type>>)>
    where
        I: IntoIterator<Item = (Symbol, Loc<Type>)>,
        I::IntoIter: ExactSizeIterator,
    {
        let it = it.into_iter();

        let start = self.def_types.len();

        // because we have an ExactSizeIterator, we can reserve space here
        self.def_types.reserve(it.len());

        for (symbol, loc_type) in it {
            let Loc { region, value } = loc_type;
            let type_index = Index::push_new(&mut self.types, value);

            self.def_types.push((symbol, Loc::at(region, type_index)));
        }

        let length = self.def_types.len() - start;

        Slice::new(start as _, length as _)
    }

    #[inline(always)]
    pub fn exists<I>(&mut self, flex_vars: I, defs_constraint: Constraint) -> Constraint
    where
        I: IntoIterator<Item = Variable>,
    {
        let defs_and_ret_constraint = Index::new(self.constraints.len() as _);

        self.constraints.push(defs_constraint);
        self.constraints.push(Constraint::True);

        let let_contraint = LetConstraint {
            rigid_vars: Slice::default(),
            flex_vars: self.variable_slice(flex_vars),
            def_types: Slice::default(),
            defs_and_ret_constraint,
        };

        let let_index = Index::new(self.let_constraints.len() as _);
        self.let_constraints.push(let_contraint);

        Constraint::Let(let_index)
    }

    #[inline(always)]
    pub fn exists_many<I, C>(&mut self, flex_vars: I, defs_constraint: C) -> Constraint
    where
        I: IntoIterator<Item = Variable>,
        C: IntoIterator<Item = Constraint>,
        C::IntoIter: ExactSizeIterator,
    {
        let defs_constraint = self.and_constraint(defs_constraint);

        let defs_and_ret_constraint = Index::new(self.constraints.len() as _);
        self.constraints.push(defs_constraint);
        self.constraints.push(Constraint::True);

        let let_contraint = LetConstraint {
            rigid_vars: Slice::default(),
            flex_vars: self.variable_slice(flex_vars),
            def_types: Slice::default(),
            defs_and_ret_constraint,
        };

        let let_index = Index::new(self.let_constraints.len() as _);
        self.let_constraints.push(let_contraint);

        Constraint::Let(let_index)
    }

    #[inline(always)]
    pub fn let_constraint<I1, I2, I3>(
        &mut self,
        rigid_vars: I1,
        flex_vars: I2,
        def_types: I3,
        defs_constraint: Constraint,
        ret_constraint: Constraint,
    ) -> Constraint
    where
        I1: IntoIterator<Item = Variable>,
        I2: IntoIterator<Item = Variable>,
        I3: IntoIterator<Item = (Symbol, Loc<Type>)>,
        I3::IntoIter: ExactSizeIterator,
    {
        let defs_and_ret_constraint = Index::new(self.constraints.len() as _);

        self.constraints.push(defs_constraint);
        self.constraints.push(ret_constraint);

        let let_contraint = LetConstraint {
            rigid_vars: self.variable_slice(rigid_vars),
            flex_vars: self.variable_slice(flex_vars),
            def_types: self.def_types_slice(def_types),
            defs_and_ret_constraint,
        };

        let let_index = Index::new(self.let_constraints.len() as _);
        self.let_constraints.push(let_contraint);

        Constraint::Let(let_index)
    }

    #[inline(always)]
    pub fn and_constraint<I>(&mut self, constraints: I) -> Constraint
    where
        I: IntoIterator<Item = Constraint>,
        I::IntoIter: ExactSizeIterator,
    {
        let mut it = constraints.into_iter();

        match it.len() {
            0 => Constraint::True,
            1 => it.next().unwrap(),
            _ => {
                let start = self.constraints.len() as u32;

                self.constraints.extend(it);

                let end = self.constraints.len() as u32;

                let slice = Slice::new(start, (end - start) as u16);

                Constraint::And(slice)
            }
        }
    }

    pub fn lookup(
        &mut self,
        symbol: Symbol,
        expected: Expected<Type>,
        region: Region,
    ) -> Constraint {
        Constraint::Lookup(
            symbol,
            Index::push_new(&mut self.expectations, expected),
            region,
        )
    }
    pub fn contains_save_the_environment(&self, constraint: &Constraint) -> bool {
        match constraint {
            Constraint::Eq(..) => false,
            Constraint::Store(..) => false,
            Constraint::Lookup(..) => false,
            Constraint::Pattern(..) => false,
            Constraint::True => false,
            Constraint::SaveTheEnvironment => true,
            Constraint::Let(index) => {
                let let_constraint = &self.let_constraints[index.index()];

                let offset = let_constraint.defs_and_ret_constraint.index();
                let defs_constraint = &self.constraints[offset];
                let ret_constraint = &self.constraints[offset + 1];

                self.contains_save_the_environment(defs_constraint)
                    || self.contains_save_the_environment(ret_constraint)
            }
            Constraint::And(slice) => {
                let constraints = &self.constraints[slice.indices()];

                constraints
                    .iter()
                    .any(|c| self.contains_save_the_environment(c))
            }
            Constraint::IsOpenType(_) => false,
            Constraint::IncludesTag(_) => false,
            Constraint::PatternPresence(_, _, _, _) => false,
        }
    }

    pub fn store(
        &mut self,
        typ: Type,
        variable: Variable,
        filename: &'static str,
        line_number: u32,
    ) -> Constraint {
        let type_index = Index::push_new(&mut self.types, typ);
        let string_index = Index::push_new(&mut self.strings, filename);

        Constraint::Store(type_index, variable, string_index, line_number)
    }
}

static_assertions::assert_eq_size!([u8; 3 * 8], Constraint);

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Index<Type>, Index<Expected<Type>>, Index<Category>, Region),
    Store(Index<Type>, Variable, Index<&'static str>, u32),
    Lookup(Symbol, Index<Expected<Type>>, Region),
    Pattern(
        Index<Type>,
        Index<PExpected<Type>>,
        Index<PatternCategory>,
        Region,
    ),
    True, // Used for things that always unify, e.g. blanks and runtime errors
    SaveTheEnvironment,
    Let(Index<LetConstraint>),
    And(Slice<Constraint>),
    /// Presence constraints
    IsOpenType(Index<Type>), // Theory; always applied to a variable? if yes the use that
    IncludesTag(Index<IncludesTag>),
    PatternPresence(
        Index<Type>,
        Index<PExpected<Type>>,
        Index<PatternCategory>,
        Region,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetConstraint {
    pub rigid_vars: Slice<Variable>,
    pub flex_vars: Slice<Variable>,
    pub def_types: Slice<(Symbol, Loc<Index<Type>>)>,
    pub defs_and_ret_constraint: Index<(Constraint, Constraint)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncludesTag {
    pub type_index: Index<Type>,
    pub tag_name: TagName,
    pub types: Slice<Type>,
    pub pattern_category: Index<PatternCategory>,
    pub region: Region,
}
