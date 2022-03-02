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
}

impl Constraints {
    pub fn new() -> Self {
        let constraints = Vec::new();
        let mut types = Vec::new();
        let variables = Vec::new();
        let def_types = Vec::new();
        let let_constraints = Vec::new();
        let mut categories = Vec::new();
        let pattern_categories = Vec::new();
        let expectations = Vec::new();
        let pattern_expectations = Vec::new();
        let includes_tags = Vec::new();

        types.push(Type::EmptyRec);
        types.push(Type::EmptyTagUnion);

        categories.push(Category::Record);

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
        }
    }

    pub const EMPTY_RECORD: Index<Type> = Index::new(0);
    pub const EMPTY_TAG_UNION: Index<Type> = Index::new(1);

    pub const CATEGORY_RECORD: Index<Category> = Index::new(0);

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
            other => Index::push_new(&mut self.categories, other),
        }
    }

    pub fn equal_types(
        &mut self,
        typ: Type,
        expected: Expected<Type>,
        category: Category,
        region: Region,
    ) -> Constraint {
        let type_index = Index::push_new(&mut self.types, typ);
        let expected_index = Index::push_new(&mut self.expectations, expected);
        let category_index = Index::push_new(&mut self.categories, category);

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
        let category_index = Index::push_new(&mut self.pattern_categories, category);

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

    fn def_types_slice<I>(&mut self, it: I) -> Slice<(Symbol, Loc<Type>)>
    where
        I: IntoIterator<Item = (Symbol, Loc<Type>)>,
    {
        let start = self.def_types.len();

        for (symbol, loc_type) in it {
            let type_index = Index::new(self.types.len() as _);
            let Loc { region, value } = loc_type;
            self.types.push(value);

            self.def_types.push((symbol, Loc::at(region, type_index)));
        }

        let length = self.def_types.len() - start;

        Slice::new(start as _, length as _)
    }

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

    pub fn exists_many<I, C>(&mut self, flex_vars: I, defs_constraint: C) -> Constraint
    where
        I: IntoIterator<Item = Variable>,
        C: IntoIterator<Item = Constraint>,
    {
        let defs_and_ret_constraint = Index::new(self.constraints.len() as _);

        self.and_constraint(defs_constraint);
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

    pub fn and_constraint<I>(&mut self, constraints: I) -> Constraint
    where
        I: IntoIterator<Item = Constraint>,
    {
        let start = self.constraints.len() as u32;

        self.constraints.extend(constraints);

        let end = self.constraints.len() as u32;

        let slice = Slice::new(start, (end - start) as u16);

        Constraint::And(slice)
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
        let type_index = Index::new(self.types.len() as _);

        self.types.push(typ);

        Constraint::Store(type_index, variable, filename, line_number)
    }
}

static_assertions::assert_eq_size!([u8; 4 * 8], Constraint);
static_assertions::assert_eq_size!([u8; 3 * 8 + 4], LetConstraint);

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Index<Type>, Index<Expected<Type>>, Index<Category>, Region),
    Store(Index<Type>, Variable, &'static str, u32),
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
    pub def_types: Slice<(Symbol, Loc<Type>)>,
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
