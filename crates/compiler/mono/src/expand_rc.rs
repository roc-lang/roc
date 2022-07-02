/// UNUSED
///
/// but kept for future reference
///
///
//    pub fn optimize_refcount_operations<'i, T>(
//        arena: &'a Bump,
//        home: ModuleId,
//        ident_ids: &'i mut IdentIds,
//        procs: &mut MutMap<T, Proc<'a>>,
//    ) {
//        use crate::expand_rc;
//
//        let deferred = expand_rc::Deferred {
//            inc_dec_map: Default::default(),
//            assignments: Vec::new_in(arena),
//            decrefs: Vec::new_in(arena),
//        };
//
//        let mut env = expand_rc::Env {
//            home,
//            arena,
//            ident_ids,
//            layout_map: Default::default(),
//            alias_map: Default::default(),
//            constructor_map: Default::default(),
//            deferred,
//        };
//
//        for (_, proc) in procs.iter_mut() {
//            let b = expand_rc::expand_and_cancel_proc(
//                &mut env,
//                arena.alloc(proc.body.clone()),
//                proc.args,
//            );
//            proc.body = b.clone();
//        }
//    }
use crate::ir::{BranchInfo, Expr, ModifyRc, Stmt};
use crate::layout::{Layout, UnionLayout};
use bumpalo::collections::Vec;
use bumpalo::Bump;
// use linked_hash_map::LinkedHashMap;
use roc_collections::all::MutMap;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

// This file is heavily inspired by the Perceus paper
//
// https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf
//
// With how we insert RC instructions, this pattern is very common:
//
//  when xs is
//      Cons x xx ->
//          inc x;
//          inc xx;
//          dec xs;
//          ...
//
// This pattern is very inefficient, because it will first increment the tail (recursively),
// and then decrement it again. We can see this more clearly if we inline/specialize the `dec xs`
//
//  when xs is
//      Cons x xx ->
//          inc x;
//          inc xx;
//          dec x;
//          dec xx;
//          decref xs
//          ...
//
// Here `decref` non-recursively decrements (and possibly frees) `xs`. Now the idea is that we can
// fuse `inc x; dec x` by just doing nothing: they cancel out
//
// We can do slightly more, in the `Nil` case
//
//  when xs is
//      ...
//      Nil ->
//          dec xs;
//          accum
//
// Here we know that `Nil` is represented by NULL (a linked list has a NullableUnwrapped layout),
// so we  can just drop the `dec xs`
//
// # complications
//
// Let's work through the `Cons x xx` example
//
// First we need to know the constructor of `xs` in the particular block. This information would
// normally be lost when we compile pattern matches, but we keep it in the `BranchInfo` field of
// switch branches. here we also store the symbol that was switched on, and the layout of that
// symbol.
//
// Next, we need to know that `x` and `xx` alias the head and tail of `xs`. We store that
// information when encountering a `AccessAtIndex` into `xs`.
//
// In most cases these two pieces of information are enough. We keep track of a
// `LinkedHashMap<Symbol, i64>`: `LinkedHashMap` remembers insertion order, which is crucial here.
// The `i64` value represents the increment (positive value) or decrement (negative value). When
// the value is 0, increments and decrements have cancelled out and we just emit nothing.
//
// We need to do slightly more work in the case of
//
//  when xs is
//      Cons _ xx ->
//          recurse xx (1 + accum)
//
// In this case, the head is not bound. That's OK when the list elements are not refcounted (or
// contain anything refcounted). But when they do, we can't expand the `dec xs` because there is no
// way to reference the head element.
//
// Our refcounting mechanism can't deal well with unused variables (it'll leak their memory). But
// we can insert the access after RC instructions have been inserted. So in the above case we
// actually get
//
//  when xs is
//      Cons _ xx ->
//          let v1 = AccessAtIndex 1 xs
//          inc v1;
//          let xx = AccessAtIndex 2 xs
//          inc xx;
//          dec v1;
//          dec xx;
//          decref xs;
//          recurse xx (1 + accum)
//
// Here we see another problem: the increments and decrements cannot be fused immediately.
// Therefore we add a rule that we can "push down" increments and decrements past
//
//  - `Let`s binding a `AccessAtIndex`
//  - refcount operations
//
// This allows the aforementioned `LinkedHashMap` to accumulate all changes, and then emit
// all (uncancelled) modifications at once before any "non-push-downable-stmt", hence:
//
//  when xs is
//      Cons _ xx ->
//          let v1 = AccessAtIndex 1 xs
//          let xx = AccessAtIndex 2 xs
//          dec v1;
//          decref xs;
//          recurse xx (1 + accum)

pub struct Env<'a, 'i> {
    /// bump allocator
    pub arena: &'a Bump,

    /// required for creating new `Symbol`s
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,

    /// layout of the symbol
    pub layout_map: MutMap<Symbol, Layout<'a>>,

    /// record for each symbol, the aliases of its fields
    pub alias_map: MutMap<Symbol, MutMap<u64, Symbol>>,

    /// for a symbol (found in a `when x is`), record in which branch we are
    pub constructor_map: MutMap<Symbol, u64>,

    /// increments and decrements deferred until later
    pub deferred: Deferred<'a>,
}

#[derive(Debug)]
pub struct Deferred<'a> {
    pub inc_dec_map: LinkedHashMap<Symbol, i64>,
    pub assignments: Vec<'a, (Symbol, Expr<'a>, Layout<'a>)>,
    pub decrefs: Vec<'a, Symbol>,
}

impl<'a, 'i> Env<'a, 'i> {
    fn insert_branch_info(&mut self, info: &BranchInfo<'a>) {
        match info {
            BranchInfo::Constructor {
                layout,
                scrutinee,
                tag_id,
            } => {
                self.constructor_map.insert(*scrutinee, *tag_id as u64);
                self.layout_map.insert(*scrutinee, *layout);
            }
            BranchInfo::None => (),
        }
    }

    fn remove_branch_info(&mut self, info: &BranchInfo) {
        match info {
            BranchInfo::Constructor { scrutinee, .. } => {
                self.constructor_map.remove(scrutinee);
                self.layout_map.remove(scrutinee);
            }
            BranchInfo::None => (),
        }
    }

    fn try_insert_struct_info(&mut self, symbol: Symbol, layout: &Layout<'a>) {
        use Layout::*;

        if let Struct(fields) = layout {
            self.constructor_map.insert(symbol, 0);
            self.layout_map.insert(symbol, Layout::Struct(fields));
        }
    }

    fn insert_struct_info(&mut self, symbol: Symbol, fields: &'a [Layout<'a>]) {
        self.constructor_map.insert(symbol, 0);
        self.layout_map.insert(symbol, Layout::Struct(fields));
    }

    fn remove_struct_info(&mut self, symbol: Symbol) {
        self.constructor_map.remove(&symbol);
        self.layout_map.remove(&symbol);
    }

    pub fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }
    #[allow(dead_code)]
    fn manual_unique_symbol(home: ModuleId, ident_ids: &mut IdentIds) -> Symbol {
        let ident_id = ident_ids.gen_unique();

        Symbol::new(home, ident_id)
    }
}

fn layout_for_constructor<'a>(
    _arena: &'a Bump,
    layout: &Layout<'a>,
    constructor: u64,
) -> ConstructorLayout<&'a [Layout<'a>]> {
    use ConstructorLayout::*;
    use Layout::*;

    match layout {
        Union(variant) => {
            use UnionLayout::*;
            match variant {
                NullableUnwrapped {
                    nullable_id,
                    other_fields,
                } => {
                    if (constructor > 0) == *nullable_id {
                        ConstructorLayout::IsNull
                    } else {
                        ConstructorLayout::HasFields(other_fields)
                    }
                }
                NullableWrapped {
                    nullable_id,
                    other_tags,
                } => {
                    if constructor as i64 == *nullable_id {
                        ConstructorLayout::IsNull
                    } else {
                        ConstructorLayout::HasFields(other_tags[constructor as usize])
                    }
                }
                NonRecursive(fields) | Recursive(fields) => HasFields(fields[constructor as usize]),
                NonNullableUnwrapped(fields) => {
                    debug_assert_eq!(constructor, 0);
                    HasFields(fields)
                }
            }
        }
        Struct(fields) => {
            debug_assert_eq!(constructor, 0);
            HasFields(fields)
        }
        other => unreachable!("weird layout {:?}", other),
    }
}

fn work_for_constructor<'a>(
    env: &mut Env<'a, '_>,
    symbol: &Symbol,
) -> ConstructorLayout<Vec<'a, Symbol>> {
    use ConstructorLayout::*;

    let mut result = Vec::new_in(env.arena);

    let constructor = match env.constructor_map.get(symbol) {
        None => return ConstructorLayout::Unknown,
        Some(v) => *v,
    };
    let full_layout = match env.layout_map.get(symbol) {
        None => return ConstructorLayout::Unknown,
        Some(v) => v,
    };

    let field_aliases = env.alias_map.get(symbol);

    match layout_for_constructor(env.arena, full_layout, constructor) {
        Unknown => Unknown,
        IsNull => IsNull,
        HasFields(constructor_layout) => {
            // figure out if there is at least one aliased refcounted field. Only then
            // does it make sense to inline the decrement
            let at_least_one_aliased = (|| {
                for (i, field_layout) in constructor_layout.iter().enumerate() {
                    if field_layout.contains_refcounted()
                        && field_aliases.and_then(|map| map.get(&(i as u64))).is_some()
                    {
                        return true;
                    }
                }
                false
            })();

            // for each field, if it has refcounted content, check if it has an alias
            // if so, use the alias, otherwise load the field.
            for (i, field_layout) in constructor_layout.iter().enumerate() {
                if field_layout.contains_refcounted() {
                    match field_aliases.and_then(|map| map.get(&(i as u64))) {
                        Some(alias_symbol) => {
                            // the field was bound in a pattern match
                            result.push(*alias_symbol);
                        }
                        None if at_least_one_aliased => {
                            // the field was not bound in a pattern match
                            // we have to extract it now, but we only extract it
                            // if at least one field is aliased.

                            todo!("get the tag id");
                            /*
                            let expr = Expr::AccessAtIndex {
                                index: i as u64,
                                field_layouts: constructor_layout,
                                structure: *symbol,
                                wrapped: todo!("get the tag id"),
                            };

                            // create a fresh symbol for this field
                            let alias_symbol = Env::manual_unique_symbol(env.home, env.ident_ids);

                            let layout = if let Layout::RecursivePointer = field_layout {
                                *full_layout
                            } else {
                                *field_layout
                            };

                            env.deferred.assignments.push((alias_symbol, expr, layout));
                            result.push(alias_symbol);
                            */
                        }
                        None => {
                            // if all refcounted fields were unaliased, generate a normal decrement
                            // of the whole structure (less code generated this way)
                            return ConstructorLayout::Unknown;
                        }
                    }
                }
            }
            ConstructorLayout::HasFields(result)
        }
    }
}

fn can_push_inc_through(stmt: &Stmt) -> bool {
    use Stmt::*;

    match stmt {
        Let(_, expr, _, _) => {
            // we can always delay an increment/decrement until after a field access
            matches!(expr, Expr::StructAtIndex { .. } | Expr::Literal(_))
        }

        Refcounting(ModifyRc::Inc(_, _), _) => true,
        Refcounting(ModifyRc::Dec(_), _) => true,

        _ => false,
    }
}

#[derive(Debug)]
enum ConstructorLayout<T> {
    IsNull,
    HasFields(T),
    Unknown,
}

pub fn expand_and_cancel_proc<'a>(
    env: &mut Env<'a, '_>,
    stmt: &'a Stmt<'a>,
    arguments: &'a [(Layout<'a>, Symbol)],
) -> &'a Stmt<'a> {
    let mut introduced = Vec::new_in(env.arena);

    for (layout, symbol) in arguments {
        if let Layout::Struct(fields) = layout {
            env.insert_struct_info(*symbol, fields);

            introduced.push(*symbol);
        }
    }

    let result = expand_and_cancel(env, stmt);

    for symbol in introduced {
        env.remove_struct_info(symbol);
    }

    result
}

fn expand_and_cancel<'a>(env: &mut Env<'a, '_>, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
    use Stmt::*;

    let mut deferred = Deferred {
        inc_dec_map: Default::default(),
        assignments: Vec::new_in(env.arena),
        decrefs: Vec::new_in(env.arena),
    };

    if !can_push_inc_through(stmt) {
        std::mem::swap(&mut deferred, &mut env.deferred);
    }

    let mut result = {
        match stmt {
            Let(mut symbol, expr, layout, cont) => {
                env.layout_map.insert(symbol, *layout);

                let mut expr = expr;
                let mut layout = layout;
                let mut cont = cont;

                // prevent long chains of `Let`s from blowing the stack
                let mut literal_stack = Vec::new_in(env.arena);

                while !matches!(
                    &expr,
                    Expr::StructAtIndex { .. } | Expr::Struct(_) | Expr::Call(_)
                ) {
                    if let Stmt::Let(symbol1, expr1, layout1, cont1) = cont {
                        literal_stack.push((symbol, expr.clone(), *layout));

                        symbol = *symbol1;
                        expr = expr1;
                        layout = layout1;
                        cont = cont1;
                    } else {
                        break;
                    }
                }

                let new_cont;

                match &expr {
                    Expr::StructAtIndex {
                        structure,
                        index,
                        field_layouts,
                    } => {
                        let entry = env
                            .alias_map
                            .entry(*structure)
                            .or_insert_with(MutMap::default);

                        entry.insert(*index, symbol);

                        env.layout_map
                            .insert(*structure, Layout::Struct(field_layouts));

                        // if the field is a struct, we know its constructor too!
                        let field_layout = &field_layouts[*index as usize];
                        env.try_insert_struct_info(symbol, field_layout);

                        new_cont = expand_and_cancel(env, cont);

                        env.remove_struct_info(symbol);

                        // make sure to remove the alias, so other branches don't use it by accident
                        env.alias_map
                            .get_mut(structure)
                            .and_then(|map| map.remove(index));
                    }
                    Expr::Struct(_) => {
                        if let Layout::Struct(fields) = layout {
                            env.insert_struct_info(symbol, fields);

                            new_cont = expand_and_cancel(env, cont);

                            env.remove_struct_info(symbol);
                        } else {
                            new_cont = expand_and_cancel(env, cont);
                        }
                    }
                    Expr::Call(_) => {
                        if let Layout::Struct(fields) = layout {
                            env.insert_struct_info(symbol, fields);

                            new_cont = expand_and_cancel(env, cont);

                            env.remove_struct_info(symbol);
                        } else {
                            new_cont = expand_and_cancel(env, cont);
                        }
                    }
                    _ => {
                        new_cont = expand_and_cancel(env, cont);
                    }
                }

                let stmt = Let(symbol, expr.clone(), *layout, new_cont);
                let mut stmt = &*env.arena.alloc(stmt);

                for (symbol, expr, layout) in literal_stack.into_iter().rev() {
                    stmt = env.arena.alloc(Stmt::Let(symbol, expr, layout, stmt));
                }

                stmt
            }

            Switch {
                cond_symbol,
                cond_layout,
                ret_layout,
                branches,
                default_branch,
            } => {
                let mut new_branches = Vec::with_capacity_in(branches.len(), env.arena);

                for (id, info, branch) in branches.iter() {
                    env.insert_branch_info(info);

                    let branch = expand_and_cancel(env, branch);

                    env.remove_branch_info(info);

                    env.constructor_map.remove(cond_symbol);

                    new_branches.push((*id, info.clone(), branch.clone()));
                }

                env.insert_branch_info(&default_branch.0);
                let new_default = (
                    default_branch.0.clone(),
                    expand_and_cancel(env, default_branch.1),
                );
                env.remove_branch_info(&default_branch.0);

                let stmt = Switch {
                    cond_symbol: *cond_symbol,
                    cond_layout: *cond_layout,
                    ret_layout: *ret_layout,
                    branches: new_branches.into_bump_slice(),
                    default_branch: new_default,
                };

                &*env.arena.alloc(stmt)
            }
            Refcounting(ModifyRc::DecRef(symbol), cont) => {
                // decref the current cell
                env.deferred.decrefs.push(*symbol);

                expand_and_cancel(env, cont)
            }

            Refcounting(ModifyRc::Dec(symbol), cont) => {
                use ConstructorLayout::*;

                match work_for_constructor(env, symbol) {
                    HasFields(dec_symbols) => {
                        // we can inline the decrement

                        // decref the current cell
                        env.deferred.decrefs.push(*symbol);

                        // and record decrements for all the fields
                        for dec_symbol in dec_symbols {
                            let count = env.deferred.inc_dec_map.entry(dec_symbol).or_insert(0);
                            *count -= 1;
                        }
                    }
                    Unknown => {
                        // we can't inline the decrement; just record it
                        let count = env.deferred.inc_dec_map.entry(*symbol).or_insert(0);
                        *count -= 1;
                    }
                    IsNull => {
                        // we decrement a value represented as `NULL` at runtime;
                        // we can drop this decrement completely
                    }
                }

                expand_and_cancel(env, cont)
            }

            Refcounting(ModifyRc::Inc(symbol, inc_amount), cont) => {
                let count = env.deferred.inc_dec_map.entry(*symbol).or_insert(0);
                *count += *inc_amount as i64;

                expand_and_cancel(env, cont)
            }

            Join {
                id,
                parameters,
                body: continuation,
                remainder,
            } => {
                let continuation = expand_and_cancel(env, continuation);
                let remainder = expand_and_cancel(env, remainder);

                let stmt = Join {
                    id: *id,
                    parameters,
                    body: continuation,
                    remainder,
                };

                env.arena.alloc(stmt)
            }

            Ret(_) | Jump(_, _) | RuntimeError(_) => stmt,
        }
    };

    for symbol in deferred.decrefs {
        let stmt = Refcounting(ModifyRc::DecRef(symbol), result);
        result = env.arena.alloc(stmt);
    }

    // do all decrements
    for (symbol, amount) in deferred.inc_dec_map.iter().rev() {
        use std::cmp::Ordering;
        match amount.cmp(&0) {
            Ordering::Equal => {
                // do nothing else
            }
            Ordering::Greater => {
                // do nothing yet
            }
            Ordering::Less => {
                // the RC insertion should not double decrement in a block
                debug_assert_eq!(*amount, -1);

                // insert missing decrements
                let stmt = Refcounting(ModifyRc::Dec(*symbol), result);
                result = env.arena.alloc(stmt);
            }
        }
    }

    for (symbol, amount) in deferred.inc_dec_map.into_iter().rev() {
        use std::cmp::Ordering;
        match amount.cmp(&0) {
            Ordering::Equal => {
                // do nothing else
            }
            Ordering::Greater => {
                // insert missing increments
                let stmt = Refcounting(ModifyRc::Inc(symbol, amount as u64), result);
                result = env.arena.alloc(stmt);
            }
            Ordering::Less => {
                // already done
            }
        }
    }

    for (symbol, expr, layout) in deferred.assignments {
        let stmt = Stmt::Let(symbol, expr, layout, result);
        result = env.arena.alloc(stmt);
    }

    result
}
