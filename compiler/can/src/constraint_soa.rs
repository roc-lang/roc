use crate::expected::{Expected, PExpected};
use roc_collections::soa::{Index, Slice};
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::types::{Category, PatternCategory, Type};
use roc_types::{subs::Variable, types::VariableDetail};

pub struct Constraints {
    constraints: Vec<Constraint>,
    types: Vec<Type>,
    variables: Vec<Variable>,
    def_types: Vec<(Symbol, Loc<Index<Type>>)>,
    let_constraints: Vec<LetConstraint>,
    categories: Vec<Category>,
    pattern_categories: Vec<PatternCategory>,
    expectations: Vec<Expected<Type>>,
    pattern_expectations: Vec<PExpected<Type>>,
}

impl Constraints {
    pub const EMPTY_RECORD: Index<Type> = Index::new(0);
    pub const EMPTY_TAG_UNION: Index<Type> = Index::new(1);

    pub const CATEGORY_RECORD: Index<Category> = Index::new(0);

    pub fn push_type(&mut self, typ: Type) -> Index<Type> {
        match typ {
            Type::EmptyRec => Self::EMPTY_RECORD,
            Type::EmptyTagUnion => Self::EMPTY_TAG_UNION,
            other => Index::push_new(&mut self.types, other),
        }
    }

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
    pub def_types: Slice<(Symbol, Loc<Type>)>,
    pub defs_and_ret_constraint: Index<(Constraint, Constraint)>,
}
