use crate::expected::{Expected, PExpected};
use roc_collections::all::{MutSet, SendMap};
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::types::{Category, PatternCategory, Type};
use roc_types::{subs::Variable, types::VariableDetail};

pub struct Constraints {
    constraints: Vec<Constraint>,
    types: Vec<Type>,
    variables: Vec<Variable>,
    def_types: Vec<(Symbol, Located<Index<Type>>)>,
    let_constraints: Vec<LetConstraint>,
    categories: Vec<Category>,
    pattern_categories: Vec<PatternCategory>,
    expectations: Vec<Expected<Type>>,
    pattern_expectations: Vec<PExpected<Type>>,
}

impl Constraints {
    pub fn equal_types(
        &mut self,
        typ: Type,
        expected: Expected<Type>,
        category: Category,
        region: Region,
    ) -> Constraint {
        let type_index = Index::new(self.types.len() as _);
        let expected_index = Index::new(self.expectations.len() as _);
        let category_index = Index::new(self.categories.len() as _);

        self.types.push(typ);
        self.expectations.push(expected);
        self.categories.push(category);

        Constraint::Eq(type_index, expected_index, category_index, region)
    }

    pub fn equal_pattern_types(
        &mut self,
        typ: Type,
        expected: PExpected<Type>,
        category: PatternCategory,
        region: Region,
    ) -> Constraint {
        let type_index = Index::new(self.types.len() as _);
        let expected_index = Index::new(self.pattern_expectations.len() as _);
        let category_index = Index::new(self.pattern_categories.len() as _);

        self.types.push(typ);
        self.pattern_expectations.push(expected);
        self.pattern_categories.push(category);

        Constraint::Pattern(type_index, expected_index, category_index, region)
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

    fn def_types_slice<I>(&mut self, it: I) -> Slice<(Symbol, Located<Type>)>
    where
        I: IntoIterator<Item = (Symbol, Located<Type>)>,
    {
        let start = self.def_types.len();

        for (symbol, loc_type) in it {
            let type_index = Index::new(self.types.len() as _);
            let Located { region, value } = loc_type;
            self.types.push(value);

            self.def_types
                .push((symbol, Located::at(region, type_index)));
        }

        let length = self.def_types.len() - start;

        Slice::new(start as _, length as _)
    }

    pub fn let_contraint<I1, I2, I3>(
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
        I3: IntoIterator<Item = (Symbol, Located<Type>)>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetConstraint {
    pub rigid_vars: Slice<Variable>,
    pub flex_vars: Slice<Variable>,
    pub def_types: Slice<(Symbol, Located<Type>)>,
    pub defs_and_ret_constraint: Index<(Constraint, Constraint)>,
}

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
