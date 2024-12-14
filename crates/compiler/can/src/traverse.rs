//! Traversals over the can ast.

use roc_module::{ident::Lowercase, symbol::Symbol};
use roc_region::all::{Loc, Position, Region};
use roc_types::{subs::Variable, types::MemberImpl};

use crate::{
    abilities::AbilitiesStore,
    def::{Annotation, Def},
    expr::{
        self, AnnotatedMark, ClosureData, Declarations, Expr, Field, OpaqueWrapFunctionData,
        StructAccessorData,
    },
    pattern::{DestructType, Pattern, RecordDestruct, TupleDestruct},
};
#[derive(Clone)]
pub enum DeclarationInfo<'a> {
    Value {
        loc_symbol: Loc<Symbol>,
        loc_expr: &'a Loc<Expr>,
        expr_var: Variable,
        pattern: Pattern,
        annotation: Option<&'a Annotation>,
    },
    Expectation {
        loc_condition: &'a Loc<Expr>,
    },
    Function {
        loc_symbol: Loc<Symbol>,
        loc_body: &'a Loc<Expr>,
        expr_var: Variable,
        pattern: Pattern,
        function: &'a Loc<expr::FunctionDef>,
        annotation: Option<&'a Annotation>,
    },
    Destructure {
        loc_pattern: &'a Loc<Pattern>,
        opt_pattern_var: Option<Variable>,
        loc_expr: &'a Loc<Expr>,
        expr_var: Variable,
        annotation: Option<&'a Annotation>,
    },
}

impl<'a> DeclarationInfo<'a> {
    pub fn region(&self) -> Region {
        use DeclarationInfo::*;
        match self {
            Value {
                loc_symbol,
                loc_expr,
                ..
            } => Region::span_across(&loc_symbol.region, &loc_expr.region),
            Expectation { loc_condition } => loc_condition.region,
            Function {
                loc_symbol,
                function,
                ..
            } => Region::span_across(&loc_symbol.region, &function.region),
            Destructure {
                loc_pattern,
                loc_expr,
                ..
            } => Region::span_across(&loc_pattern.region, &loc_expr.region),
        }
    }

    fn var(&self) -> Variable {
        match self {
            DeclarationInfo::Value { expr_var, .. } => *expr_var,
            DeclarationInfo::Expectation { .. } => Variable::BOOL,
            DeclarationInfo::Function { expr_var, .. } => *expr_var,
            DeclarationInfo::Destructure { expr_var, .. } => *expr_var,
        }
    }
}

pub fn walk_decls<V: Visitor>(visitor: &mut V, decls: &Declarations) {
    use crate::expr::DeclarationTag::*;

    for (index, tag) in decls.declarations.iter().enumerate() {
        let info = match tag {
            Value => {
                let loc_expr = &decls.expressions[index];

                let loc_symbol = decls.symbols[index];
                let expr_var = decls.variables[index];

                let pattern = match decls.specializes.get(&index).copied() {
                    Some(specializes) => Pattern::AbilityMemberSpecialization {
                        ident: loc_symbol.value,
                        specializes,
                    },
                    None => Pattern::Identifier(loc_symbol.value),
                };

                DeclarationInfo::Value {
                    loc_symbol,
                    loc_expr,
                    expr_var,
                    pattern,
                    annotation: decls.annotations[index].as_ref(),
                }
            }
            Expectation => {
                let loc_condition = &decls.expressions[index];

                DeclarationInfo::Expectation { loc_condition }
            }
            Function(function_index)
            | Recursive(function_index)
            | TailRecursive(function_index) => {
                let loc_body = &decls.expressions[index];

                let loc_symbol = decls.symbols[index];
                let expr_var = decls.variables[index];
                let annotation = decls.annotations[index].as_ref();

                let pattern = match decls.specializes.get(&index).copied() {
                    Some(specializes) => Pattern::AbilityMemberSpecialization {
                        ident: loc_symbol.value,
                        specializes,
                    },
                    None => Pattern::Identifier(loc_symbol.value),
                };

                let function_def = &decls.function_bodies[function_index.index()];

                DeclarationInfo::Function {
                    loc_symbol,
                    loc_body,
                    expr_var,
                    pattern,
                    function: function_def,
                    annotation,
                }
            }
            Destructure(destructure_index) => {
                let destructure = &decls.destructs[destructure_index.index()];
                let loc_pattern = &destructure.loc_pattern;

                let loc_expr = &decls.expressions[index];
                let expr_var = decls.variables[index];

                let opt_pattern_var = match loc_pattern.value {
                    Pattern::Identifier(..) | Pattern::AbilityMemberSpecialization { .. } => {
                        Some(expr_var)
                    }
                    _ => loc_pattern.value.opt_var(),
                };

                DeclarationInfo::Destructure {
                    loc_pattern,
                    opt_pattern_var,
                    loc_expr,
                    expr_var,
                    annotation: decls.annotations[index].as_ref(),
                }
            }
            MutualRecursion { .. } => {
                // The actual declarations involved in the mutual recursion will come next.
                continue;
            }
        };

        visitor.visit_decl(info);
    }
}

pub fn walk_decl<V: Visitor>(visitor: &mut V, decl: DeclarationInfo<'_>) {
    use DeclarationInfo::*;

    match decl {
        Value {
            loc_symbol,
            loc_expr,
            expr_var,
            pattern,
            annotation,
        } => {
            visitor.visit_pattern(&pattern, loc_symbol.region, Some(expr_var));

            visitor.visit_expr(&loc_expr.value, loc_expr.region, expr_var);
            if let Some(annot) = annotation {
                visitor.visit_annotation(annot);
            }
        }
        Expectation { loc_condition } => {
            visitor.visit_expr(&loc_condition.value, loc_condition.region, Variable::BOOL);
        }
        Function {
            loc_symbol,
            loc_body,
            expr_var,
            pattern,
            function,
            annotation,
        } => {
            visitor.visit_pattern(&pattern, loc_symbol.region, Some(expr_var));

            walk_closure_help(
                visitor,
                &function.value.arguments,
                loc_body,
                function.value.return_type,
            );

            if let Some(annot) = annotation {
                visitor.visit_annotation(annot);
            }
        }
        Destructure {
            loc_pattern,
            opt_pattern_var,
            loc_expr,
            expr_var,
            annotation,
        } => {
            visitor.visit_pattern(&loc_pattern.value, loc_pattern.region, opt_pattern_var);
            visitor.visit_expr(&loc_expr.value, loc_expr.region, expr_var);

            if let Some(annot) = annotation {
                visitor.visit_annotation(annot);
            }
        }
    };
}

pub fn walk_def<V: Visitor>(visitor: &mut V, def: &Def) {
    let Def {
        loc_pattern,
        loc_expr,
        annotation,
        expr_var,
        ..
    } = def;

    let opt_var = match loc_pattern.value {
        Pattern::Identifier(..) | Pattern::AbilityMemberSpecialization { .. } => Some(*expr_var),
        _ => loc_pattern.value.opt_var(),
    };

    visitor.visit_pattern(&loc_pattern.value, loc_pattern.region, opt_var);
    visitor.visit_expr(&loc_expr.value, loc_expr.region, *expr_var);
    if let Some(annot) = &annotation {
        visitor.visit_annotation(annot);
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr, var: Variable) {
    match expr {
        Expr::Closure(closure_data) => walk_closure(visitor, closure_data),
        Expr::When {
            cond_var,
            expr_var,
            loc_cond,
            branches,
            region: _,
            branches_cond_var: _,
            exhaustive: _,
        } => {
            walk_when(visitor, *cond_var, *expr_var, loc_cond, branches);
        }
        Expr::Num(..) => { /* terminal */ }
        Expr::Int(..) => { /* terminal */ }
        Expr::Float(..) => { /* terminal */ }
        Expr::Str(..) => { /* terminal */ }
        Expr::IngestedFile(..) => { /* terminal */ }
        Expr::SingleQuote(..) => { /* terminal */ }
        Expr::List {
            elem_var,
            loc_elems,
        } => {
            walk_list(visitor, *elem_var, loc_elems);
        }
        Expr::Var(..) => { /* terminal */ }
        Expr::ParamsVar { .. } => { /* terminal */ }
        Expr::AbilityMember(..) => { /* terminal */ }
        Expr::If {
            cond_var,
            branches,
            branch_var,
            final_else,
        } => walk_if(visitor, *cond_var, branches, *branch_var, final_else),
        Expr::LetRec(defs, body, _cycle_mark) => {
            defs.iter().for_each(|def| visitor.visit_def(def));
            visitor.visit_expr(&body.value, body.region, var);
        }
        Expr::LetNonRec(def, body) => {
            visitor.visit_def(def);
            visitor.visit_expr(&body.value, body.region, var);
        }
        Expr::Call(f, args, _called_via) => {
            let (fn_var, loc_fn, _closure_var, _ret_var, _fx_var) = &**f;
            walk_call(visitor, *fn_var, loc_fn, args);
        }
        Expr::Crash { msg, .. } => {
            visitor.visit_expr(&msg.value, msg.region, Variable::STR);
        }
        Expr::RunLowLevel {
            op: _,
            args,
            ret_var: _,
        } => {
            args.iter()
                .for_each(|(v, e)| visitor.visit_expr(e, Region::zero(), *v));
        }
        Expr::ForeignCall {
            foreign_symbol: _,
            args,
            ret_var: _,
        } => {
            args.iter()
                .for_each(|(v, e)| visitor.visit_expr(e, Region::zero(), *v));
        }
        Expr::Record {
            record_var: _,
            fields,
        } => {
            walk_record_fields(visitor, fields.iter());
        }
        Expr::Tuple {
            tuple_var: _,
            elems,
        } => elems
            .iter()
            .for_each(|(var, elem)| visitor.visit_expr(&elem.value, elem.region, *var)),
        Expr::EmptyRecord => { /* terminal */ }
        Expr::ImportParams(_, region, Some((_, expr))) => visitor.visit_expr(expr, *region, var),
        Expr::ImportParams(_, _, None) => { /* terminal */ }
        Expr::RecordAccess {
            field_var,
            loc_expr,
            field: _,
            record_var: _,
            ext_var: _,
        } => visitor.visit_expr(&loc_expr.value, loc_expr.region, *field_var),
        Expr::RecordAccessor(StructAccessorData { .. }) => { /* terminal */ }
        Expr::TupleAccess {
            elem_var,
            loc_expr,
            index: _,
            tuple_var: _,
            ext_var: _,
        } => visitor.visit_expr(&loc_expr.value, loc_expr.region, *elem_var),
        Expr::OpaqueWrapFunction(OpaqueWrapFunctionData { .. }) => { /* terminal */ }
        Expr::RecordUpdate {
            record_var: _,
            ext_var: _,
            symbol: _,
            updates,
        } => {
            walk_record_fields(visitor, updates.iter());
        }
        Expr::Tag {
            tag_union_var: _,
            ext_var: _,
            name: _,
            arguments,
        } => arguments
            .iter()
            .for_each(|(v, le)| visitor.visit_expr(&le.value, le.region, *v)),
        Expr::ZeroArgumentTag { .. } => { /* terminal */ }
        Expr::OpaqueRef {
            opaque_var: _,
            name: _,
            argument,
            specialized_def_type: _,
            type_arguments: _,
            lambda_set_variables: _,
        } => {
            let (var, le) = &**argument;
            visitor.visit_expr(&le.value, le.region, *var);
        }
        Expr::Expect {
            loc_condition,
            loc_continuation,
            lookups_in_cond: _,
        } => {
            visitor.visit_expr(&loc_condition.value, loc_condition.region, Variable::BOOL);
            visitor.visit_expr(
                &loc_continuation.value,
                loc_continuation.region,
                Variable::NULL,
            );
        }
        Expr::Dbg {
            variable,
            source: _,
            source_location: _,
            loc_message,
            loc_continuation,
            symbol: _,
        } => {
            visitor.visit_expr(&loc_message.value, loc_message.region, *variable);
            visitor.visit_expr(
                &loc_continuation.value,
                loc_continuation.region,
                Variable::NULL,
            );
        }
        Expr::Try {
            result_expr,
            result_var,
            return_var: _,
            ok_payload_var: _,
            err_payload_var: _,
            err_ext_var: _,
            kind: _,
        } => {
            visitor.visit_expr(&result_expr.value, result_expr.region, *result_var);
        }
        Expr::Return {
            return_value,
            return_var,
        } => {
            visitor.visit_expr(&return_value.value, return_value.region, *return_var);
        }
        Expr::RuntimeError(..) => { /* terminal */ }
    }
}

#[inline(always)]
pub fn walk_closure<V: Visitor>(visitor: &mut V, clos: &ClosureData) {
    let ClosureData {
        arguments,
        loc_body,
        return_type,
        ..
    } = clos;

    walk_closure_help(visitor, arguments, loc_body, *return_type)
}

fn walk_closure_help<V: Visitor>(
    visitor: &mut V,
    arguments: &[(Variable, AnnotatedMark, Loc<Pattern>)],
    loc_body: &Loc<Expr>,
    return_type: Variable,
) {
    arguments.iter().for_each(|(var, _exhaustive_mark, arg)| {
        visitor.visit_pattern(&arg.value, arg.region, Some(*var))
    });

    visitor.visit_expr(&loc_body.value, loc_body.region, return_type);
}

#[inline(always)]
pub fn walk_when<V: Visitor>(
    visitor: &mut V,
    cond_var: Variable,
    expr_var: Variable,
    loc_cond: &Loc<Expr>,
    branches: &[expr::WhenBranch],
) {
    visitor.visit_expr(&loc_cond.value, loc_cond.region, cond_var);

    branches
        .iter()
        .for_each(|branch| walk_when_branch(visitor, branch, expr_var));
}

#[inline(always)]
pub fn walk_when_branch<V: Visitor>(
    visitor: &mut V,
    branch: &expr::WhenBranch,
    expr_var: Variable,
) {
    let expr::WhenBranch {
        patterns,
        value,
        guard,
        redundant: _,
    } = branch;

    patterns.iter().for_each(|pat| {
        visitor.visit_pattern(
            &pat.pattern.value,
            pat.pattern.region,
            pat.pattern.value.opt_var(),
        )
    });
    visitor.visit_expr(&value.value, value.region, expr_var);
    if let Some(guard) = guard {
        visitor.visit_expr(&guard.value, guard.region, Variable::BOOL);
    }
}

#[inline(always)]
pub fn walk_list<V: Visitor>(visitor: &mut V, elem_var: Variable, loc_elems: &[Loc<Expr>]) {
    loc_elems
        .iter()
        .for_each(|le| visitor.visit_expr(&le.value, le.region, elem_var));
}

#[inline(always)]
pub fn walk_if<V: Visitor>(
    visitor: &mut V,
    cond_var: Variable,
    branches: &[(Loc<Expr>, Loc<Expr>)],
    branch_var: Variable,
    final_else: &Loc<Expr>,
) {
    branches.iter().for_each(|(cond, body)| {
        visitor.visit_expr(&cond.value, cond.region, cond_var);
        visitor.visit_expr(&body.value, body.region, branch_var);
    });
    visitor.visit_expr(&final_else.value, final_else.region, branch_var);
}

#[inline(always)]
pub fn walk_call<V: Visitor>(
    visitor: &mut V,
    fn_var: Variable,
    fn_expr: &Loc<Expr>,
    args: &[(Variable, Loc<Expr>)],
) {
    visitor.visit_expr(&fn_expr.value, fn_expr.region, fn_var);
    args.iter()
        .for_each(|(v, le)| visitor.visit_expr(&le.value, le.region, *v));
}

#[inline(always)]
pub fn walk_record_fields<'a, V: Visitor>(
    visitor: &mut V,
    fields: impl Iterator<Item = (&'a Lowercase, &'a Field)>,
) {
    fields.for_each(
        |(
            _name,
            Field {
                var,
                loc_expr,
                region: _,
            },
        )| { visitor.visit_expr(&loc_expr.value, loc_expr.region, *var) },
    )
}

pub trait Visitor: Sized {
    /// Most default implementations will call [Visitor::should_visit] to decide whether they
    /// should descend into a node. Return `false` to skip visiting.
    fn should_visit(&mut self, _region: Region) -> bool {
        true
    }

    fn visit_decls(&mut self, decls: &Declarations) {
        walk_decls(self, decls);
    }

    fn visit_decl(&mut self, decl: DeclarationInfo<'_>) {
        if self.should_visit(decl.region()) {
            walk_decl(self, decl);
        }
    }

    fn visit_def(&mut self, def: &Def) {
        if self.should_visit(def.region()) {
            walk_def(self, def);
        }
    }

    fn visit_annotation(&mut self, _pat: &Annotation) {
        // ignore by default
    }

    fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
        if self.should_visit(region) {
            walk_expr(self, expr, var);
        }
    }

    fn visit_pattern(&mut self, pattern: &Pattern, region: Region, _opt_var: Option<Variable>) {
        if self.should_visit(region) {
            walk_pattern(self, pattern);
        }
    }

    fn visit_record_destruct(&mut self, destruct: &RecordDestruct, region: Region) {
        if self.should_visit(region) {
            walk_record_destruct(self, destruct);
        }
    }

    fn visit_tuple_destruct(&mut self, destruct: &TupleDestruct, region: Region) {
        if self.should_visit(region) {
            self.visit_pattern(
                &destruct.typ.1.value,
                destruct.typ.1.region,
                Some(destruct.typ.0),
            )
        }
    }
}

pub fn walk_pattern<V: Visitor>(visitor: &mut V, pattern: &Pattern) {
    use Pattern::*;

    match pattern {
        Identifier(..) => { /* terminal */ }
        As(subpattern, _symbol) => {
            visitor.visit_pattern(&subpattern.value, subpattern.region, None)
        }
        AppliedTag { arguments, .. } => arguments
            .iter()
            .for_each(|(v, lp)| visitor.visit_pattern(&lp.value, lp.region, Some(*v))),
        UnwrappedOpaque { argument, .. } => {
            let (v, lp) = &**argument;
            visitor.visit_pattern(&lp.value, lp.region, Some(*v));
        }
        RecordDestructure { destructs, .. } => destructs
            .iter()
            .for_each(|d| visitor.visit_record_destruct(&d.value, d.region)),
        TupleDestructure { destructs, .. } => destructs
            .iter()
            .for_each(|d| visitor.visit_tuple_destruct(&d.value, d.region)),
        List {
            patterns, elem_var, ..
        } => patterns
            .patterns
            .iter()
            .for_each(|p| visitor.visit_pattern(&p.value, p.region, Some(*elem_var))),
        NumLiteral(..) => { /* terminal */ }
        IntLiteral(..) => { /* terminal */ }
        FloatLiteral(..) => { /* terminal */ }
        StrLiteral(..) => { /* terminal */ }
        SingleQuote(..) => { /* terminal */ }
        Underscore => { /* terminal */ }
        AbilityMemberSpecialization { .. } => { /* terminal */ }
        Shadowed(..) => { /* terminal */ }
        OpaqueNotInScope(..) => { /* terminal */ }
        UnsupportedPattern(..) => { /* terminal */ }
        MalformedPattern(..) => { /* terminal */ }
    }
}

pub fn walk_record_destruct<V: Visitor>(visitor: &mut V, destruct: &RecordDestruct) {
    use DestructType::*;
    match &destruct.typ {
        Required => { /* terminal */ }
        Optional(var, expr) => visitor.visit_expr(&expr.value, expr.region, *var),
        Guard(var, pat) => visitor.visit_pattern(&pat.value, pat.region, Some(*var)),
    }
}

struct TypeAtVisitor {
    region: Region,
    typ: Option<Variable>,
}

impl Visitor for TypeAtVisitor {
    fn should_visit(&mut self, region: Region) -> bool {
        region.contains(&self.region)
    }

    fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
        if region == self.region {
            debug_assert!(self.typ.is_none());
            self.typ = Some(var);
            return;
        }

        walk_expr(self, expr, var);
    }

    fn visit_pattern(&mut self, pat: &Pattern, region: Region, opt_var: Option<Variable>) {
        if region == self.region {
            debug_assert!(self.typ.is_none());
            self.typ = opt_var;
            return;
        }

        walk_pattern(self, pat)
    }
}

struct TypeAtPositionVisitor {
    position: Position,
    region_typ: Option<(Region, Variable)>,
}

impl Visitor for TypeAtPositionVisitor {
    fn should_visit(&mut self, region: Region) -> bool {
        region.contains_pos(self.position)
    }

    fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
        if region.contains_pos(self.position) {
            self.region_typ = Some((region, var));

            walk_expr(self, expr, var);
        }
    }

    fn visit_pattern(&mut self, pat: &Pattern, region: Region, opt_var: Option<Variable>) {
        if region.contains_pos(self.position) {
            if let Some(var) = opt_var {
                self.region_typ = Some((region, var));
            }

            walk_pattern(self, pat);
        }
    }
}

/// Attempts to find the type of an expression at `region`, if it exists.
pub fn find_type_at(region: Region, decls: &Declarations) -> Option<Variable> {
    let mut visitor = TypeAtVisitor { region, typ: None };
    visitor.visit_decls(decls);
    visitor.typ
}

#[derive(Debug)]
pub enum FoundSymbol {
    /// Specialization(T, foo1) is the specialization of foo for T.
    Specialization(Symbol, Symbol),
    /// AbilityMember(Foo, foo) is the ability member foo of Foo.
    AbilityMember(Symbol, Symbol),
    /// Raw symbol, not specialized to anything.
    Symbol(Symbol),
}

impl FoundSymbol {
    pub fn implementation_symbol(&self) -> Symbol {
        match self {
            FoundSymbol::Specialization(_, sym)
            | FoundSymbol::AbilityMember(_, sym)
            | FoundSymbol::Symbol(sym) => *sym,
        }
    }
}

/// Given an ability Foo implements foo : ..., returns (T, foo1) if the symbol at the given region is a
/// Like [find_type_at], but descends into the narrowest node containing [position].
pub fn find_closest_type_at(
    position: Position,
    decls: &Declarations,
) -> Option<(Region, Variable)> {
    let mut visitor = TypeAtPositionVisitor {
        position,
        region_typ: None,
    };
    visitor.visit_decls(decls);
    visitor.region_typ
}

/// Given an ability Foo has foo : ..., returns (T, foo1) if the symbol at the given region is a
/// symbol foo1 that specializes foo for T. Otherwise if the symbol is foo but the specialization
/// is unknown, (Foo, foo) is returned. Otherwise [None] is returned.
pub fn find_closest_symbol_at(
    position: Position,
    decls: &Declarations,
    abilities_store: &AbilitiesStore,
) -> Option<FoundSymbol> {
    find_symbol_at_impl(Region::from_pos(position), decls, abilities_store, true)
}

/// Given an ability Foo has foo : ..., returns (T, foo1) if the symbol at the given region is a
/// symbol foo1 that specializes foo for T. Otherwise if the symbol is foo but the specialization
/// is unknown, (Foo, foo) is returned. Otherwise [None] is returned.
pub fn find_symbol_at(
    region: Region,
    decls: &Declarations,
    abilities_store: &AbilitiesStore,
) -> Option<FoundSymbol> {
    find_symbol_at_impl(region, decls, abilities_store, false)
}

pub fn find_symbol_at_impl(
    region: Region,
    decls: &Declarations,
    abilities_store: &AbilitiesStore,
    allow_subregion: bool,
) -> Option<FoundSymbol> {
    let mut visitor = Finder {
        region,
        found: None,
        abilities_store,
        allow_subregion,
    };
    visitor.visit_decls(decls);
    return visitor.found;

    struct Finder<'a> {
        region: Region,
        abilities_store: &'a AbilitiesStore,
        found: Option<FoundSymbol>,
        allow_subregion: bool,
    }

    impl<'a> Finder<'a> {
        fn is_at_wanted_region(&self, region: Region) -> bool {
            if self.allow_subregion {
                region.contains(&self.region)
            } else {
                region == self.region
            }
        }
    }

    impl Visitor for Finder<'_> {
        fn should_visit(&mut self, region: Region) -> bool {
            region.contains(&self.region)
        }

        fn visit_pattern(&mut self, pattern: &Pattern, region: Region, _opt_var: Option<Variable>) {
            if self.is_at_wanted_region(region) {
                match pattern {
                    Pattern::AbilityMemberSpecialization {
                        ident: spec_symbol,
                        specializes: _,
                    } => {
                        debug_assert!(self.found.is_none());
                        let spec_type =
                            find_specialization_type_of_symbol(*spec_symbol, self.abilities_store)
                                .unwrap();
                        self.found = Some(FoundSymbol::Specialization(spec_type, *spec_symbol))
                    }
                    Pattern::Identifier(symbol) => self.found = Some(FoundSymbol::Symbol(*symbol)),
                    _ => {}
                }
            }

            walk_pattern(self, pattern);
        }

        fn visit_expr(&mut self, expr: &Expr, region: Region, var: Variable) {
            if self.is_at_wanted_region(region) {
                match expr {
                    &Expr::AbilityMember(member_symbol, specialization_id, _var) => {
                        debug_assert!(self.found.is_none());
                        self.found = match specialization_id
                            .and_then(|id| self.abilities_store.get_resolved(id))
                        {
                            Some(spec_symbol) => {
                                let spec_type = find_specialization_type_of_symbol(
                                    spec_symbol,
                                    self.abilities_store,
                                )
                                .unwrap();
                                Some(FoundSymbol::Specialization(spec_type, spec_symbol))
                            }
                            None => {
                                let parent_ability = self
                                    .abilities_store
                                    .member_def(member_symbol)
                                    .unwrap()
                                    .parent_ability;
                                Some(FoundSymbol::AbilityMember(parent_ability, member_symbol))
                            }
                        };
                        return;
                    }
                    Expr::Var(symbol, _var) => self.found = Some(FoundSymbol::Symbol(*symbol)),
                    _ => {}
                }
            }

            walk_expr(self, expr, var);
        }
    }

    fn find_specialization_type_of_symbol(
        symbol: Symbol,
        abilities_store: &AbilitiesStore,
    ) -> Option<Symbol> {
        abilities_store
            .iter_declared_implementations()
            .find(|(_, member_impl)| matches!(member_impl, MemberImpl::Impl(sym) if *sym == symbol))
            .map(|(impl_key, _)| impl_key.opaque)
    }
}

pub fn symbols_introduced_from_pattern(
    pattern: &Loc<Pattern>,
) -> impl Iterator<Item = Loc<Symbol>> {
    let mut visitor = Collector {
        symbols: Vec::new(),
    };
    visitor.visit_pattern(&pattern.value, pattern.region, None);
    return visitor.symbols.into_iter();

    struct Collector {
        symbols: Vec<Loc<Symbol>>,
    }
    impl Visitor for Collector {
        fn visit_pattern(&mut self, pattern: &Pattern, region: Region, _opt_var: Option<Variable>) {
            use Pattern::*;
            match pattern {
                Identifier(symbol)
                | Shadowed(_, _, symbol)
                | AbilityMemberSpecialization { ident: symbol, .. } => {
                    self.symbols.push(Loc::at(region, *symbol));
                }
                _ => walk_pattern(self, pattern),
            }
        }

        fn visit_record_destruct(&mut self, destruct: &RecordDestruct, region: Region) {
            // when a record field has a pattern guard, only symbols in the guard are introduced
            if let DestructType::Guard(_, subpattern) = &destruct.typ {
                self.visit_pattern(&subpattern.value, subpattern.region, None);
            } else {
                self.symbols.push(Loc::at(region, destruct.symbol));
            }
        }
    }
}

pub enum FoundDeclaration<'a> {
    Decl(DeclarationInfo<'a>),
    Def(&'a Def),
}

impl<'a> FoundDeclaration<'a> {
    pub fn region(&self) -> Region {
        match self {
            FoundDeclaration::Decl(decl) => decl.region(),
            FoundDeclaration::Def(def) => def.region(),
        }
    }

    pub fn var(&self) -> Variable {
        match self {
            FoundDeclaration::Decl(decl) => decl.var(),
            FoundDeclaration::Def(def) => def.expr_var,
        }
    }
}

/// Finds the declaration of `symbol`.
pub fn find_declaration(symbol: Symbol, decls: &'_ Declarations) -> Option<FoundDeclaration<'_>> {
    let mut visitor = Finder {
        symbol,
        found: None,
    };
    visitor.visit_decls(decls);
    return visitor.found;

    struct Finder<'a> {
        symbol: Symbol,
        found: Option<FoundDeclaration<'a>>,
    }

    impl Visitor for Finder<'_> {
        fn should_visit(&mut self, _region: Region) -> bool {
            true
        }

        fn visit_decl(&mut self, decl: DeclarationInfo<'_>) {
            match decl {
                DeclarationInfo::Value { loc_symbol, .. }
                | DeclarationInfo::Function { loc_symbol, .. }
                    if loc_symbol.value == self.symbol =>
                {
                    self.found = Some(FoundDeclaration::Decl(unsafe { std::mem::transmute(decl) }));
                }
                DeclarationInfo::Destructure { .. } => {
                    // TODO destructures
                    walk_decl(self, decl);
                }
                _ => {
                    walk_decl(self, decl);
                }
            }
        }

        fn visit_def(&mut self, def: &Def) {
            if matches!(def.loc_pattern.value, Pattern::Identifier(s) if s == self.symbol) {
                debug_assert!(self.found.is_none());
                // Safety: the def can't escape the passed in `decls`, and the visitor does not
                // synthesize defs.
                self.found = Some(FoundDeclaration::Def(unsafe { std::mem::transmute(def) }));
                return;
            }

            walk_def(self, def)
        }
    }
}
