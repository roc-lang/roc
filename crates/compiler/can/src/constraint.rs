use std::cell::Cell;
use std::path::PathBuf;
use std::sync::Arc;

use crate::abilities::SpecializationId;
use crate::exhaustive::{ExhaustiveContext, SketchedRows};
use crate::expected::{Expected, PExpected};
use crate::expr::TryKind;
use roc_collections::soa::{index_push_new, slice_extend_new};
use roc_module::ident::{IdentSuffix, TagName};
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::subs::{ExhaustiveMark, IllegalCycleMark, Variable};
use roc_types::types::{Category, PatternCategory, TypeTag, Types};
use soa::{EitherIndex, Index, Slice};

pub struct Constraints {
    pub constraints: Vec<Constraint>,
    pub type_slices: Vec<TypeOrVar>,
    pub variables: Vec<Variable>,
    loc_variables: Vec<Loc<Variable>>,
    pub loc_symbols: Vec<(Symbol, Region)>,
    pub let_constraints: Vec<LetConstraint>,
    pub categories: Vec<Category>,
    pub pattern_categories: Vec<PatternCategory>,
    pub expectations: Vec<Expected<TypeOrVar>>,
    pub pattern_expectations: Vec<PExpected<TypeOrVar>>,
    pub includes_tags: Vec<IncludesTag>,
    pub strings: Vec<&'static str>,
    pub sketched_rows: Vec<SketchedRows>,
    pub eq: Vec<Eq>,
    pub pattern_eq: Vec<PatternEq>,
    pub cycles: Vec<Cycle>,
    pub fx_call_constraints: Vec<FxCallConstraint>,
    pub fx_suffix_constraints: Vec<FxSuffixConstraint>,
    pub try_target_constraints: Vec<TryTargetConstraint>,
}

impl std::fmt::Debug for Constraints {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Constraints")
            .field("constraints", &self.constraints)
            .field("types", &"<types>")
            .field("type_slices", &self.type_slices)
            .field("variables", &self.variables)
            .field("loc_variables", &self.loc_variables)
            .field("loc_symbols", &self.loc_symbols)
            .field("let_constraints", &self.let_constraints)
            .field("categories", &self.categories)
            .field("pattern_categories", &self.pattern_categories)
            .field("expectations", &"<expectations>")
            .field("pattern_expectations", &"<pattern expectations>")
            .field("includes_tags", &self.includes_tags)
            .field("strings", &self.strings)
            .field("sketched_rows", &self.sketched_rows)
            .field("eq", &self.eq)
            .field("pattern_eq", &self.pattern_eq)
            .field("cycles", &self.cycles)
            .field("fx_call_constraints", &self.fx_call_constraints)
            .field("fx_suffix_constraints", &self.fx_suffix_constraints)
            .finish()
    }
}

impl Default for Constraints {
    fn default() -> Self {
        Self::new()
    }
}

pub type ExpectedTypeIndex = Index<Expected<TypeOrVar>>;
pub type PExpectedTypeIndex = Index<PExpected<TypeOrVar>>;
pub type TypeOrVar = EitherIndex<TypeTag, Variable>;

impl Constraints {
    pub fn new() -> Self {
        let constraints = Vec::new();
        let type_slices = Vec::with_capacity(16);
        let variables = Vec::new();
        let loc_variables = Vec::new();
        let loc_symbols = Vec::new();
        let let_constraints = Vec::new();
        let mut categories = Vec::with_capacity(16);
        let mut pattern_categories = Vec::with_capacity(16);
        let expectations = Vec::new();
        let pattern_expectations = Vec::new();
        let includes_tags = Vec::new();
        let strings = Vec::new();
        let sketched_rows = Vec::new();
        let eq = Vec::new();
        let pattern_eq = Vec::new();
        let cycles = Vec::new();
        let fx_call_constraints = Vec::with_capacity(16);
        let fx_suffix_constraints = Vec::new();
        let result_type_constraints = Vec::new();

        categories.extend([
            Category::Record,
            Category::ForeignCall,
            Category::OpaqueArg,
            Category::Lambda,
            Category::ClosureSize,
            Category::StrInterpolation,
            Category::If,
            Category::When,
            Category::Frac,
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
            type_slices,
            variables,
            loc_variables,
            loc_symbols,
            let_constraints,
            categories,
            pattern_categories,
            expectations,
            pattern_expectations,
            includes_tags,
            strings,
            sketched_rows,
            eq,
            pattern_eq,
            cycles,
            fx_call_constraints,
            fx_suffix_constraints,
            try_target_constraints: result_type_constraints,
        }
    }

    pub const EMPTY_RECORD: Index<Cell<Index<TypeTag>>> = Index::new(0);
    pub const EMPTY_TAG_UNION: Index<Cell<Index<TypeTag>>> = Index::new(1);
    pub const STR: Index<Cell<Index<TypeTag>>> = Index::new(2);

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
    pub const CATEGORY_RETURN: Index<Category> = Index::new(14);

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
    pub fn push_type(&mut self, types: &Types, typ: Index<TypeTag>) -> TypeOrVar {
        if let TypeTag::Variable(var) = types[typ] {
            Self::push_type_variable(var)
        } else {
            EitherIndex::from_left(typ)
        }
    }

    pub fn statistics(&self, module_id: ModuleId) -> Result<String, std::fmt::Error> {
        use std::fmt::Write;

        let mut buf = String::new();

        writeln!(buf, "Constraints statistics for module {module_id:?}:")?;

        writeln!(buf, "   constraints length: {}:", self.constraints.len())?;
        writeln!(
            buf,
            "   let_constraints length: {}:",
            self.let_constraints.len()
        )?;
        writeln!(buf, "   expectations length: {}:", self.expectations.len())?;
        writeln!(buf, "   categories length: {}:", self.categories.len())?;

        Ok(buf)
    }

    #[inline(always)]
    pub const fn push_variable(&self, var: Variable) -> TypeOrVar {
        Self::push_type_variable(var)
    }

    #[inline(always)]
    const fn push_type_variable(var: Variable) -> TypeOrVar {
        // that's right, we use the variable's integer value as the index
        // that way, we don't need to push anything onto a vector
        let index: Index<Variable> = Index::new(var.index());

        EitherIndex::from_right(index)
    }

    pub fn push_expected_type(&mut self, expected: Expected<TypeOrVar>) -> ExpectedTypeIndex {
        index_push_new(&mut self.expectations, expected)
    }

    pub fn push_pat_expected_type(&mut self, expected: PExpected<TypeOrVar>) -> PExpectedTypeIndex {
        index_push_new(&mut self.pattern_expectations, expected)
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
            Category::Frac => Self::CATEGORY_FLOAT,
            Category::Int => Self::CATEGORY_INT,
            Category::Num => Self::CATEGORY_NUM,
            Category::List => Self::CATEGORY_LIST,
            Category::Str => Self::CATEGORY_STR,
            Category::Character => Self::CATEGORY_CHARACTER,
            other => index_push_new(&mut self.categories, other),
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
            other => index_push_new(&mut self.pattern_categories, other),
        }
    }

    pub fn equal_types(
        &mut self,
        type_index: TypeOrVar,
        expected_index: ExpectedTypeIndex,
        category: Category,
        region: Region,
    ) -> Constraint {
        let category_index = Self::push_category(self, category);

        Constraint::Eq(Eq(type_index, expected_index, category_index, region))
    }

    pub fn equal_types_var(
        &mut self,
        var: Variable,
        expected_index: ExpectedTypeIndex,
        category: Category,
        region: Region,
    ) -> Constraint {
        let type_index = Self::push_type_variable(var);
        let category_index = Self::push_category(self, category);

        Constraint::Eq(Eq(type_index, expected_index, category_index, region))
    }

    pub fn equal_types_with_storage(
        &mut self,
        type_index: TypeOrVar,
        expected_index: ExpectedTypeIndex,
        category: Category,
        region: Region,
        storage_var: Variable,
    ) -> Constraint {
        let category_index = Self::push_category(self, category);

        let equal = Constraint::Eq(Eq(type_index, expected_index, category_index, region));

        let storage_type_index = Self::push_type_variable(storage_var);
        let storage_category = Category::Storage(std::file!(), std::line!());
        let storage_category_index = Self::push_category(self, storage_category);
        let storage = Constraint::Eq(Eq(
            storage_type_index,
            expected_index,
            storage_category_index,
            region,
        ));

        self.and_constraint([equal, storage])
    }

    pub fn equal_pattern_types(
        &mut self,
        type_index: TypeOrVar,
        expected_index: PExpectedTypeIndex,
        category: PatternCategory,
        region: Region,
    ) -> Constraint {
        let category_index = Self::push_pattern_category(self, category);

        Constraint::Pattern(type_index, expected_index, category_index, region)
    }

    pub fn pattern_presence(
        &mut self,
        type_index: TypeOrVar,
        expected_index: PExpectedTypeIndex,
        category: PatternCategory,
        region: Region,
    ) -> Constraint {
        let category_index = index_push_new(&mut self.pattern_categories, category);

        Constraint::PatternPresence(type_index, expected_index, category_index, region)
    }

    pub fn is_open_type(&mut self, type_index: TypeOrVar) -> Constraint {
        Constraint::IsOpenType(type_index)
    }

    pub fn includes_tag(
        &mut self,
        type_index: TypeOrVar,
        tag_name: TagName,
        payloads: Slice<Variable>,
        category: PatternCategory,
        region: Region,
    ) -> Constraint {
        let category_index = index_push_new(&mut self.pattern_categories, category);

        let includes_tag = IncludesTag {
            type_index,
            tag_name,
            types: payloads,
            pattern_category: category_index,
            region,
        };

        let includes_tag_index = index_push_new(&mut self.includes_tags, includes_tag);

        Constraint::IncludesTag(includes_tag_index)
    }

    pub fn variable_slice<I>(&mut self, it: I) -> Slice<Variable>
    where
        I: IntoIterator<Item = Variable>,
    {
        let start = self.variables.len();
        self.variables.extend(it);
        let length = self.variables.len() - start;

        Slice::new(start as _, length as _)
    }

    pub fn loc_variable_slice<I>(&mut self, it: I) -> Slice<Loc<Variable>>
    where
        I: IntoIterator<Item = Loc<Variable>>,
    {
        let start = self.loc_variables.len();
        self.loc_variables.extend(it);
        let length = self.loc_variables.len() - start;

        Slice::new(start as _, length as _)
    }

    fn def_types_slice<I>(&mut self, it: I) -> DefTypes
    where
        I: IntoIterator<Item = (Symbol, Loc<TypeOrVar>)>,
        I::IntoIter: ExactSizeIterator,
    {
        let it = it.into_iter();

        let types_start = self.type_slices.len();
        let loc_symbols_start = self.loc_symbols.len();

        // because we have an ExactSizeIterator, we can reserve space here
        let length = it.len();

        self.type_slices.reserve(length);
        self.loc_symbols.reserve(length);

        for (symbol, loc_type) in it {
            let Loc { region, value } = loc_type;

            self.type_slices.push(value);
            self.loc_symbols.push((symbol, region));
        }

        DefTypes {
            types: Slice::new(types_start as _, length as _),
            loc_symbols: Slice::new(loc_symbols_start as _, length as _),
        }
    }

    /// A constraint that some type variables exist and should be introduced into the environment
    /// at a given rank.
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
            def_types: DefTypes::default(),
            defs_and_ret_constraint,

            // We're just marking that the variables exist, not that they should be generalized
            // (in fact there is no return constraint, so nothing to generalize at this level)
            generalizable: Generalizable(false),
        };

        let let_index = Index::new(self.let_constraints.len() as _);
        self.let_constraints.push(let_contraint);

        Constraint::Let(let_index, Slice::default())
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
            def_types: DefTypes::default(),
            defs_and_ret_constraint,
            generalizable: Generalizable(false),
        };

        let let_index = Index::new(self.let_constraints.len() as _);
        self.let_constraints.push(let_contraint);

        Constraint::Let(let_index, Slice::default())
    }

    #[inline(always)]
    pub fn let_constraint<I1, I2, I3>(
        &mut self,
        rigid_vars: I1,
        flex_vars: I2,
        def_types: I3,
        defs_constraint: Constraint,
        ret_constraint: Constraint,
        generalizable: Generalizable,
    ) -> Constraint
    where
        I1: IntoIterator<Item = Loc<Variable>>,
        I2: IntoIterator<Item = Loc<Variable>>,
        I3: IntoIterator<Item = (Symbol, Loc<TypeOrVar>)>,
        I3::IntoIter: ExactSizeIterator,
    {
        // defs and ret constraint are stored consequtively, so we only need to store one index
        let defs_and_ret_constraint = Index::new(self.constraints.len() as _);

        self.constraints.push(defs_constraint);
        self.constraints.push(ret_constraint);

        let let_constraint = LetConstraint {
            rigid_vars: self.loc_variable_slice(rigid_vars),
            flex_vars: self.variable_slice(flex_vars.into_iter().map(|v| v.value)),
            def_types: self.def_types_slice(def_types),
            defs_and_ret_constraint,
            generalizable,
        };

        let let_index = Index::new(self.let_constraints.len() as _);
        self.let_constraints.push(let_constraint);

        Constraint::Let(let_index, Slice::default())
    }

    /// A variant of `Let` used specifically for imports. When importing types from another module,
    /// we use a StorageSubs to store the data, and copy over the relevant
    /// variables/content/flattype/tagname etc.
    ///
    /// The general idea is to let-generalize the imorted types in the target module.
    /// More concretely, we need to simulate what `type_to_var` (solve.rs) does to a `Type`.
    /// While the copying puts all the data the right place, it misses that `type_to_var` puts
    /// the variables that it creates (to store the nodes of a Type in Subs) in the pool of the
    /// current rank (so they can be generalized).
    ///
    /// So, during copying of an import (`copy_import_to`, subs.rs) we track the variables that
    /// we need to put into the pool (simulating what `type_to_var` would do). Those variables
    /// then need to find their way to the pool, and a convenient approach turned out to be to
    /// tag them onto the `Let` that we used to add the imported values.
    #[inline(always)]
    pub fn let_import_constraint<I1, I2, I3>(
        &mut self,
        rigid_vars: I1,
        flex_vars: I2,
        def_types: I3,
        module_constraint: Constraint,
        pool_variables: &[Variable],
    ) -> Constraint
    where
        I1: IntoIterator<Item = Variable>,
        I2: IntoIterator<Item = Variable>,
        I3: IntoIterator<Item = (Symbol, Loc<TypeOrVar>)>,
        I3::IntoIter: ExactSizeIterator,
    {
        // defs and ret constraint are stored consequtively, so we only need to store one index
        let defs_and_ret_constraint = Index::new(self.constraints.len() as _);

        self.constraints.push(Constraint::True);
        self.constraints.push(module_constraint);

        let let_contraint = LetConstraint {
            rigid_vars: self.loc_variable_slice(rigid_vars.into_iter().map(Loc::at_zero)),
            flex_vars: self.variable_slice(flex_vars),
            def_types: self.def_types_slice(def_types),
            defs_and_ret_constraint,
            // If the module these variables were solved in solved them as generalized,
            // they should be generalized here too.
            generalizable: Generalizable(true),
        };

        let let_index = Index::new(self.let_constraints.len() as _);
        self.let_constraints.push(let_contraint);

        let pool_slice = self.variable_slice(pool_variables.iter().copied());

        Constraint::Let(let_index, pool_slice)
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
        expected_index: ExpectedTypeIndex,
        region: Region,
    ) -> Constraint {
        Constraint::Lookup(symbol, expected_index, region)
    }

    pub fn fx_call(
        &mut self,
        call_fx_var: Variable,
        call_kind: FxCallKind,
        call_region: Region,
        expectation: Option<FxExpectation>,
    ) -> Constraint {
        let constraint = FxCallConstraint {
            call_fx_var,
            call_kind,
            call_region,
            expectation,
        };

        let constraint_index = index_push_new(&mut self.fx_call_constraints, constraint);

        Constraint::FxCall(constraint_index)
    }

    pub fn fx_record_field_unsuffixed(&mut self, variable: Variable, region: Region) -> Constraint {
        let type_index = Self::push_type_variable(variable);
        let constraint = FxSuffixConstraint {
            kind: FxSuffixKind::UnsuffixedRecordField,
            type_index,
            region,
        };

        let constraint_index = index_push_new(&mut self.fx_suffix_constraints, constraint);

        Constraint::FxSuffix(constraint_index)
    }

    pub fn flex_to_pure(&mut self, fx_var: Variable) -> Constraint {
        Constraint::FlexToPure(fx_var)
    }

    pub fn try_target(
        &mut self,
        result_type_index: TypeOrVar,
        ok_payload_var: Variable,
        err_payload_var: Variable,
        region: Region,
        kind: TryKind,
    ) -> Constraint {
        let constraint = TryTargetConstraint {
            target_type_index: result_type_index,
            ok_payload_var,
            err_payload_var,
            region,
            kind,
        };

        let constraint_index = index_push_new(&mut self.try_target_constraints, constraint);

        Constraint::TryTarget(constraint_index)
    }

    pub fn contains_save_the_environment(&self, constraint: &Constraint) -> bool {
        match constraint {
            Constraint::SaveTheEnvironment => true,
            Constraint::Let(index, _) => {
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
            Constraint::Eq(..)
            | Constraint::Store(..)
            | Constraint::Lookup(..)
            | Constraint::Pattern(..)
            | Constraint::ExpectEffectful(..)
            | Constraint::TryTarget(_)
            | Constraint::FxCall(_)
            | Constraint::FxSuffix(_)
            | Constraint::FlexToPure(_)
            | Constraint::True
            | Constraint::IsOpenType(_)
            | Constraint::IncludesTag(_)
            | Constraint::PatternPresence(_, _, _, _)
            | Constraint::Exhaustive { .. }
            | Constraint::Resolve(..)
            | Constraint::IngestedFile(..)
            | Constraint::CheckCycle(..)
            | Constraint::ImportParams(..) => false,
        }
    }

    pub fn store(
        &mut self,
        type_index: TypeOrVar,
        variable: Variable,
        filename: &'static str,
        line_number: u32,
    ) -> Constraint {
        let string_index = index_push_new(&mut self.strings, filename);

        Constraint::Store(type_index, variable, string_index, line_number)
    }

    pub fn exhaustive(
        &mut self,
        real_var: Variable,
        real_region: Region,
        category_and_expectation: Result<
            (Category, ExpectedTypeIndex),
            (PatternCategory, PExpectedTypeIndex),
        >,
        sketched_rows: SketchedRows,
        context: ExhaustiveContext,
        exhaustive: ExhaustiveMark,
    ) -> Constraint {
        let real_var = Self::push_type_variable(real_var);
        let sketched_rows = index_push_new(&mut self.sketched_rows, sketched_rows);

        let equality = match category_and_expectation {
            Ok((category, expected)) => {
                let category = index_push_new(&mut self.categories, category);
                let equality = Eq(real_var, expected, category, real_region);
                let equality = index_push_new(&mut self.eq, equality);
                Ok(equality)
            }
            Err((category, expected)) => {
                let category = index_push_new(&mut self.pattern_categories, category);
                let equality = PatternEq(real_var, expected, category, real_region);
                let equality = index_push_new(&mut self.pattern_eq, equality);
                Err(equality)
            }
        };

        Constraint::Exhaustive(equality, sketched_rows, context, exhaustive)
    }

    pub fn check_cycle<I, I1>(
        &mut self,
        loc_symbols: I,
        expr_regions: I1,
        cycle_mark: IllegalCycleMark,
    ) -> Constraint
    where
        I: IntoIterator<Item = (Symbol, Region)>,
        I1: IntoIterator<Item = Region>,
    {
        let def_names = slice_extend_new(&mut self.loc_symbols, loc_symbols);

        // we add a dummy symbol to these regions, so we can store the data in the loc_symbols vec
        let it = expr_regions.into_iter().map(|r| (Symbol::ATTR_ATTR, r));
        let expr_regions = slice_extend_new(&mut self.loc_symbols, it);
        let expr_regions = Slice::new(expr_regions.start() as _, expr_regions.len() as _);

        let cycle = Cycle {
            def_names,
            expr_regions,
        };
        let cycle_index = index_push_new(&mut self.cycles, cycle);

        Constraint::CheckCycle(cycle_index, cycle_mark)
    }

    pub fn ingested_file(
        &mut self,
        type_index: TypeOrVar,
        file_path: Box<PathBuf>,
        bytes: Arc<Vec<u8>>,
    ) -> Constraint {
        Constraint::IngestedFile(type_index, file_path, bytes)
    }

    pub fn import_params(
        &mut self,
        opt_type_index: Option<TypeOrVar>,
        module_id: ModuleId,
        region: Region,
    ) -> Constraint {
        Constraint::ImportParams(opt_type_index, module_id, region)
    }
}

roc_error_macros::assert_sizeof_default!(Constraint, 3 * 8);
roc_error_macros::assert_sizeof_aarch64!(Constraint, 3 * 8);

impl std::ops::Index<ExpectedTypeIndex> for Constraints {
    type Output = Expected<TypeOrVar>;

    fn index(&self, index: ExpectedTypeIndex) -> &Self::Output {
        &self.expectations[index.index()]
    }
}

impl std::ops::Index<PExpectedTypeIndex> for Constraints {
    type Output = PExpected<TypeOrVar>;

    fn index(&self, index: PExpectedTypeIndex) -> &Self::Output {
        &self.pattern_expectations[index.index()]
    }
}

impl std::ops::Index<Slice<Loc<Variable>>> for Constraints {
    type Output = [Loc<Variable>];

    fn index(&self, slice: Slice<Loc<Variable>>) -> &Self::Output {
        &self.loc_variables[slice.indices()]
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Eq(
    pub TypeOrVar,
    pub ExpectedTypeIndex,
    pub Index<Category>,
    pub Region,
);

#[derive(Clone, Copy, Debug)]
pub struct PatternEq(
    pub TypeOrVar,
    pub PExpectedTypeIndex,
    pub Index<PatternCategory>,
    pub Region,
);

/// When we come across a lookup of an ability member, we'd like to try to specialize that
/// lookup during solving (knowing the specialization statically avoids re-solving during mono,
/// and always gives us a way to show what specialization was intended in the editor).
///
/// However, we attempting to resolve the specialization right at the lookup site is futile
/// (we may not have solved enough of the surrounding context to know the specialization).
/// So, we only collect what resolutions we'd like to make, and attempt to resolve them once
/// we pass through a let-binding (a def, or a normal `=` binding). At those positions, the
/// expression is generalized, so if there is a static specialization, we'd know it at that
/// point.
///
/// Note that this entirely opportunistic; if a lookup of an ability member uses it
/// polymorphically, we won't find its specialization(s) until monomorphization.
#[derive(Clone, Copy, Debug)]
pub struct OpportunisticResolve {
    /// The specialized type of this lookup, to try to resolve.
    pub specialization_variable: Variable,

    /// The ability member to try to resolve.
    pub member: Symbol,
    /// If we resolve a specialization, what specialization ID to store it on.
    pub specialization_id: SpecializationId,
}

#[derive(Clone)]
pub enum Constraint {
    Eq(Eq),
    Store(TypeOrVar, Variable, Index<&'static str>, u32),
    Lookup(Symbol, ExpectedTypeIndex, Region),
    Pattern(
        TypeOrVar,
        PExpectedTypeIndex,
        Index<PatternCategory>,
        Region,
    ),
    /// Check call fx against enclosing function fx
    FxCall(Index<FxCallConstraint>),
    /// Require idents to be accurately suffixed
    FxSuffix(Index<FxSuffixConstraint>),
    /// Set an fx var as pure if flex (no effectful functions were called)
    FlexToPure(Variable),
    /// Expect statement or ignored def to be effectful
    ExpectEffectful(Variable, ExpectEffectfulReason, Region),
    /// Expect value to be some kind of Result
    TryTarget(Index<TryTargetConstraint>),
    /// Used for things that always unify, e.g. blanks and runtime errors
    True,
    SaveTheEnvironment,
    /// A Let constraint introduces symbols and their annotation at a certain level of nesting
    ///
    /// The `Slice<Variable>` is used for imports where we manually put the Content into Subs
    /// by copying from another module, but have to make sure that any variables we use to store
    /// these contents are added to `Pool` at the correct rank
    Let(Index<LetConstraint>, Slice<Variable>),
    And(Slice<Constraint>),
    /// Presence constraints
    IsOpenType(TypeOrVar), // Theory; always applied to a variable? if yes the use that
    IncludesTag(Index<IncludesTag>),
    PatternPresence(
        TypeOrVar,
        PExpectedTypeIndex,
        Index<PatternCategory>,
        Region,
    ),
    Exhaustive(
        Result<Index<Eq>, Index<PatternEq>>,
        Index<SketchedRows>,
        ExhaustiveContext,
        ExhaustiveMark,
    ),
    /// Attempt to resolve a specialization.
    Resolve(OpportunisticResolve),
    CheckCycle(Index<Cycle>, IllegalCycleMark),

    IngestedFile(TypeOrVar, Box<PathBuf>, Arc<Vec<u8>>),
    ImportParams(Option<TypeOrVar>, ModuleId, Region),
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DefTypes {
    pub types: Slice<TypeOrVar>,
    pub loc_symbols: Slice<(Symbol, Region)>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Generalizable(pub bool);

#[derive(Debug, Clone)]
pub struct LetConstraint {
    pub rigid_vars: Slice<Loc<Variable>>,
    pub flex_vars: Slice<Variable>,
    pub def_types: DefTypes,
    pub defs_and_ret_constraint: Index<(Constraint, Constraint)>,

    /// Whether the defs introduces in the let-binding can be generalized.
    /// Only
    ///   - syntactic functions
    ///   - syntactic numbers
    /// may be eligible for generalization, though there are other restrictions too.
    pub generalizable: Generalizable,
}

#[derive(Debug, Clone)]
pub struct IncludesTag {
    pub type_index: TypeOrVar,
    pub tag_name: TagName,
    pub types: Slice<Variable>,
    pub pattern_category: Index<PatternCategory>,
    pub region: Region,
}

#[derive(Debug, Clone)]
pub struct TryTargetConstraint {
    pub target_type_index: TypeOrVar,
    pub ok_payload_var: Variable,
    pub err_payload_var: Variable,
    pub region: Region,
    pub kind: TryKind,
}

#[derive(Debug, Clone, Copy)]
pub struct Cycle {
    pub def_names: Slice<(Symbol, Region)>,
    pub expr_regions: Slice<Region>,
}

#[derive(Debug)]
pub struct FxCallConstraint {
    pub call_fx_var: Variable,
    pub call_kind: FxCallKind,
    pub call_region: Region,
    pub expectation: Option<FxExpectation>,
}

#[derive(Debug, Clone, Copy)]
pub struct FxExpectation {
    pub fx_var: Variable,
    pub ann_region: Option<Region>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FxCallKind {
    Call(Option<Symbol>),
    Stmt,
    Ignored,
}

#[derive(Debug, Clone, Copy)]
pub struct FxSuffixConstraint {
    pub type_index: TypeOrVar,
    pub kind: FxSuffixKind,
    pub region: Region,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FxSuffixKind {
    Let(Symbol),
    Pattern(Symbol),
    UnsuffixedRecordField,
}

impl FxSuffixKind {
    pub fn suffix(&self) -> IdentSuffix {
        match self {
            Self::Let(symbol) | Self::Pattern(symbol) => symbol.suffix(),
            Self::UnsuffixedRecordField => IdentSuffix::None,
        }
    }

    pub fn symbol(&self) -> Option<&Symbol> {
        match self {
            Self::Let(symbol) | Self::Pattern(symbol) => Some(symbol),
            Self::UnsuffixedRecordField => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpectEffectfulReason {
    Stmt,
    Ignored,
}

/// Custom impl to limit vertical space used by the debug output
impl std::fmt::Debug for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq(Eq(arg0, arg1, arg2, arg3)) => {
                write!(f, "Eq({arg0:?}, {arg1:?}, {arg2:?}, {arg3:?})")
            }
            Self::Store(arg0, arg1, arg2, arg3) => {
                write!(f, "Store({arg0:?}, {arg1:?}, {arg2:?}, {arg3:?})")
            }
            Self::Lookup(arg0, arg1, arg2) => {
                write!(f, "Lookup({arg0:?}, {arg1:?}, {arg2:?})")
            }
            Self::Pattern(arg0, arg1, arg2, arg3) => {
                write!(f, "Pattern({arg0:?}, {arg1:?}, {arg2:?}, {arg3:?})")
            }
            Self::FxCall(arg0) => {
                write!(f, "FxCall({arg0:?})")
            }
            Self::FxSuffix(arg0) => {
                write!(f, "FxSuffix({arg0:?})")
            }
            Self::ExpectEffectful(arg0, arg1, arg2) => {
                write!(f, "EffectfulStmt({arg0:?}, {arg1:?}, {arg2:?})")
            }
            Self::FlexToPure(arg0) => {
                write!(f, "FlexToPure({arg0:?})")
            }
            Self::TryTarget(arg0) => {
                write!(f, "ExpectResultType({arg0:?})")
            }
            Self::True => write!(f, "True"),
            Self::SaveTheEnvironment => write!(f, "SaveTheEnvironment"),
            Self::Let(arg0, arg1) => f.debug_tuple("Let").field(arg0).field(arg1).finish(),
            Self::And(arg0) => f.debug_tuple("And").field(arg0).finish(),
            Self::IsOpenType(arg0) => f.debug_tuple("IsOpenType").field(arg0).finish(),
            Self::IncludesTag(arg0) => f.debug_tuple("IncludesTag").field(arg0).finish(),
            Self::PatternPresence(arg0, arg1, arg2, arg3) => {
                write!(f, "PatternPresence({arg0:?}, {arg1:?}, {arg2:?}, {arg3:?})")
            }
            Self::Exhaustive(arg0, arg1, arg2, arg3) => {
                write!(f, "Exhaustive({arg0:?}, {arg1:?}, {arg2:?}, {arg3:?})")
            }
            Self::Resolve(arg0) => {
                write!(f, "Resolve({arg0:?})")
            }
            Self::CheckCycle(arg0, arg1) => {
                write!(f, "CheckCycle({arg0:?}, {arg1:?})")
            }
            Self::IngestedFile(arg0, arg1, arg2) => {
                write!(f, "IngestedFile({arg0:?}, {arg1:?}, {arg2:?})")
            }
            Self::ImportParams(arg0, arg1, arg2) => {
                write!(f, "ImportParams({arg0:?}, {arg1:?}, {arg2:?})")
            }
        }
    }
}
