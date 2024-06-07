use super::pattern::{build_list_index_probe, store_pattern, DestructType, ListIndex, Pattern};
use crate::ir::{
    substitute_in_exprs_many, BranchInfo, Call, CallType, CompiledGuardStmt, Env, Expr,
    GuardStmtSpec, JoinPointId, Literal, Param, Procs, Stmt,
};
use crate::layout::{
    Builtin, InLayout, Layout, LayoutCache, LayoutInterner, LayoutRepr, TLLayoutInterner,
    TagIdIntType, UnionLayout,
};
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{MutMap, MutSet};
use roc_collections::BumpMap;
use roc_error_macros::internal_error;
use roc_exhaustive::{Ctor, CtorName, ListArity, RenderAs, TagId, Union};
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

/// COMPILE CASES

type Label = u64;
const RECORD_TAG_NAME: &str = "#Record";
const TUPLE_TAG_NAME: &str = "#Tuple";

/// Users of this module will mainly interact with this function. It takes
/// some normal branches and gives out a decision tree that has "labels" at all
/// the leafs and a dictionary that maps these "labels" to the code that should
/// run.
fn compile<'a>(
    interner: &TLLayoutInterner<'a>,
    raw_branches: Vec<(Guard<'a>, Pattern<'a>, u64)>,
) -> DecisionTree<'a> {
    let formatted = raw_branches
        .into_iter()
        .map(|(guard, pattern, index)| Branch {
            goal: index,
            guard,
            patterns: vec![(Vec::new(), pattern)],
        })
        .collect();

    to_decision_tree(interner, formatted)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Guard<'a> {
    NoGuard,
    Guard {
        /// pattern
        pattern: Pattern<'a>,
        /// How to compile the guard statement.
        stmt_spec: GuardStmtSpec,
    },
}

impl<'a> Guard<'a> {
    fn is_none(&self) -> bool {
        self == &Guard::NoGuard
    }

    fn is_some(&self) -> bool {
        !self.is_none()
    }
}

type Edge<'a> = (GuardedTest<'a>, DecisionTree<'a>);

#[derive(Clone, Debug, PartialEq)]
enum DecisionTree<'a> {
    Match(Label),
    Decision {
        path: Vec<PathInstruction>,
        edges: Vec<Edge<'a>>,
        default: Option<Box<DecisionTree<'a>>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum GuardedTest<'a> {
    // e.g. `x if True -> ...`
    GuardedNoTest {
        /// pattern
        pattern: Pattern<'a>,
        /// How to compile the guard body.
        stmt_spec: GuardStmtSpec,
    },
    // e.g. `<pattern> -> ...`
    TestNotGuarded {
        test: Test<'a>,
    },
    // e.g. `_ -> ...` or `x -> ...`
    PlaceholderWithGuard,
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
enum ListLenBound {
    Exact,
    AtLeast,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::enum_variant_names)]
enum Test<'a> {
    IsCtor {
        tag_id: TagIdIntType,
        ctor_name: CtorName,
        union: roc_exhaustive::Union,
        arguments: Vec<(Pattern<'a>, InLayout<'a>)>,
    },
    IsInt([u8; 16], IntWidth),
    // stores the f64 bits; u64 so that this type can impl Hash
    IsFloat(u64, FloatWidth),
    IsDecimal([u8; 16]),
    IsStr(Box<str>),
    IsBit(bool),
    IsByte {
        tag_id: TagIdIntType,
        num_alts: usize,
    },
    IsListLen {
        bound: ListLenBound,
        len: u64,
    },
}

impl<'a> Test<'a> {
    fn can_be_switch(&self) -> bool {
        match self {
            Test::IsCtor { .. } => true,
            Test::IsInt(_, int_width) => {
                // llvm does not like switching on 128-bit values
                !matches!(int_width, IntWidth::U128 | IntWidth::I128)
            }
            Test::IsFloat(_, _) => false,
            Test::IsDecimal(_) => false,
            Test::IsStr(_) => false,
            Test::IsBit(_) => true,
            Test::IsByte { .. } => true,
            Test::IsListLen { bound, .. } => match bound {
                ListLenBound::Exact => true,
                ListLenBound::AtLeast => false,
            },
        }
    }
}

use std::hash::{Hash, Hasher};
impl<'a> Hash for Test<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Test::*;

        match self {
            IsCtor { tag_id, .. } => {
                state.write_u8(0);
                tag_id.hash(state);
                // The point of this custom implementation is to not hash the tag arguments
            }
            IsInt(v, width) => {
                state.write_u8(1);
                v.hash(state);
                width.hash(state);
            }
            IsFloat(v, width) => {
                state.write_u8(2);
                v.hash(state);
                width.hash(state);
            }
            IsStr(v) => {
                state.write_u8(3);
                v.hash(state);
            }
            IsBit(v) => {
                state.write_u8(4);
                v.hash(state);
            }
            IsByte { tag_id, num_alts } => {
                state.write_u8(5);
                tag_id.hash(state);
                num_alts.hash(state);
            }
            IsDecimal(v) => {
                state.write_u8(6);
                v.hash(state);
            }
            IsListLen { len, bound } => {
                state.write_u8(7);
                (len, bound).hash(state);
            }
        }
    }
}

impl<'a> Hash for GuardedTest<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            GuardedTest::GuardedNoTest { stmt_spec, .. } => {
                state.write_u8(1);
                stmt_spec.hash(state);
            }
            GuardedTest::TestNotGuarded { test } => {
                state.write_u8(0);
                test.hash(state);
            }
            GuardedTest::PlaceholderWithGuard => {
                state.write_u8(2);
            }
        }
    }
}

// ACTUALLY BUILD DECISION TREES

#[derive(Clone, Debug, PartialEq)]
struct Branch<'a> {
    goal: Label,
    guard: Guard<'a>,
    patterns: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
}

fn to_decision_tree<'a>(
    interner: &TLLayoutInterner<'a>,
    raw_branches: Vec<Branch<'a>>,
) -> DecisionTree<'a> {
    let branches: Vec<_> = raw_branches.into_iter().map(flatten_patterns).collect();

    debug_assert!(!branches.is_empty());

    match check_for_match(&branches) {
        Match::Exact(goal) => DecisionTree::Match(goal),

        Match::GuardOnly => {
            // the first branch has no more tests to do, but it has an if-guard

            let mut branches = branches;
            let first = branches.remove(0);

            match first.guard {
                Guard::NoGuard => unreachable!(),

                Guard::Guard { pattern, stmt_spec } => {
                    let guarded_test = GuardedTest::GuardedNoTest { pattern, stmt_spec };

                    // the guard test does not have a path
                    let path = vec![];

                    // we expect none of the patterns need tests, those decisions should have been made already
                    debug_assert!(first
                        .patterns
                        .iter()
                        .all(|(_, pattern)| !needs_tests(pattern)));

                    let default = if branches.is_empty() {
                        None
                    } else {
                        Some(Box::new(to_decision_tree(interner, branches)))
                    };

                    DecisionTree::Decision {
                        path,
                        edges: vec![(guarded_test, DecisionTree::Match(first.goal))],
                        default,
                    }
                }
            }
        }

        Match::None => {
            // must clone here to release the borrow on `branches`
            let path = pick_path(&branches).clone();

            let bs = branches.clone();

            let (edges, fallback) = gather_edges(interner, branches, &path);

            let mut decision_edges: Vec<_> = edges
                .into_iter()
                .map(|(test, branches)| {
                    if bs == branches {
                        panic!();
                    } else {
                        (test, to_decision_tree(interner, branches))
                    }
                })
                .collect();

            match (decision_edges.as_slice(), fallback.as_slice()) {
                ([(_test, _decision_tree)], []) => {
                    // only one test with no fallback: we will always enter this branch

                    // get the `_decision_tree` without cloning
                    decision_edges.pop().unwrap().1
                }
                (_, []) => break_out_guard(path, decision_edges, None),
                ([], _) => {
                    // should be guaranteed by the patterns
                    debug_assert!(!fallback.is_empty());
                    to_decision_tree(interner, fallback)
                }
                (_, _) => break_out_guard(
                    path,
                    decision_edges,
                    Some(Box::new(to_decision_tree(interner, fallback))),
                ),
            }
        }
    }
}

/// Give a guard it's own Decision
fn break_out_guard<'a>(
    path: Vec<PathInstruction>,
    mut edges: Vec<(GuardedTest<'a>, DecisionTree<'a>)>,
    default: Option<Box<DecisionTree<'a>>>,
) -> DecisionTree<'a> {
    match edges
        .iter()
        .position(|(t, _)| matches!(t, GuardedTest::PlaceholderWithGuard))
    {
        None => DecisionTree::Decision {
            path,
            edges,
            default,
        },
        Some(index) => {
            let (a, b) = edges.split_at_mut(index + 1);

            let new_default = break_out_guard(path.clone(), b.to_vec(), default);

            let mut left = a.to_vec();
            let guard = left.pop().unwrap();

            let help = DecisionTree::Decision {
                path: path.clone(),
                edges: vec![guard],
                default: Some(Box::new(new_default)),
            };

            DecisionTree::Decision {
                path,
                edges: left,
                default: Some(Box::new(help)),
            }
        }
    }
}

fn guarded_tests_are_complete(tests: &[GuardedTest]) -> bool {
    let length = tests.len();
    debug_assert!(length > 0);

    let no_guard = tests
        .iter()
        .all(|t| matches!(t, GuardedTest::TestNotGuarded { .. }));

    match tests.last().unwrap() {
        GuardedTest::PlaceholderWithGuard => false,
        GuardedTest::GuardedNoTest { .. } => false,
        GuardedTest::TestNotGuarded { test } => no_guard && tests_are_complete_help(test, length),
    }
}

fn tests_are_complete_help(last_test: &Test, number_of_tests: usize) -> bool {
    match last_test {
        Test::IsCtor { union, .. } => number_of_tests == union.alternatives.len(),
        Test::IsByte { num_alts, .. } => number_of_tests == *num_alts,
        Test::IsBit(_) => number_of_tests == 2,
        Test::IsInt(_, _) => false,
        Test::IsFloat(_, _) => false,
        Test::IsDecimal(_) => false,
        Test::IsStr(_) => false,
        Test::IsListLen {
            bound: ListLenBound::AtLeast,
            len: 0,
        } => true, // [..] test
        Test::IsListLen { .. } => false,
    }
}

fn flatten_patterns(branch: Branch) -> Branch {
    let mut result = Vec::with_capacity(branch.patterns.len());

    for path_pattern in branch.patterns {
        flatten(path_pattern, &mut result);
    }

    Branch {
        patterns: result,
        ..branch
    }
}

fn flatten<'a>(
    path_pattern: (Vec<PathInstruction>, Pattern<'a>),
    path_patterns: &mut Vec<(Vec<PathInstruction>, Pattern<'a>)>,
) {
    match path_pattern.1 {
        Pattern::AppliedTag {
            union,
            arguments,
            tag_id,
            tag_name,
            layout,
        } if union.alternatives.len() == 1 && !layout.is_nullable() => {
            // TODO ^ do we need to check that guard.is_none() here?

            let path = path_pattern.0;
            // Theory: unbox doesn't have any value for us, because one-element tag unions
            // don't store the tag anyway.
            if arguments.len() == 1 {
                // NOTE here elm will unbox, but we don't use that
                path_patterns.push((
                    path,
                    Pattern::AppliedTag {
                        union,
                        arguments,
                        tag_id,
                        tag_name,
                        layout,
                    },
                ));
            } else {
                for (index, (arg_pattern, _)) in arguments.iter().enumerate() {
                    let mut new_path = path.clone();
                    new_path.push(PathInstruction::TagIndex {
                        index: index as u64,
                        tag_id,
                    });

                    flatten((new_path, arg_pattern.clone()), path_patterns);
                }
            }
        }

        _ => {
            path_patterns.push(path_pattern);
        }
    }
}

/// SUCCESSFULLY MATCH

/// If the first branch has no more "decision points" we can finally take that
/// path. If that is the case we give the resulting label and a mapping from free
/// variables to "how to get their value". So a pattern like (Just (x,_)) will give
/// us something like ("x" => value.0.0)

#[derive(Debug)]
enum Match {
    Exact(Label),
    GuardOnly,
    None,
}

fn check_for_match(branches: &[Branch]) -> Match {
    match branches.first() {
        Some(Branch {
            goal,
            patterns,
            guard,
        }) if patterns.iter().all(|(_, pattern)| !needs_tests(pattern)) => {
            if guard.is_none() {
                Match::Exact(*goal)
            } else {
                Match::GuardOnly
            }
        }
        _ => Match::None,
    }
}

/// GATHER OUTGOING EDGES

// my understanding: branches that we could jump to based on the pattern at the current path
fn gather_edges<'a>(
    interner: &TLLayoutInterner<'a>,
    branches: Vec<Branch<'a>>,
    path: &[PathInstruction],
) -> (Vec<(GuardedTest<'a>, Vec<Branch<'a>>)>, Vec<Branch<'a>>) {
    let relevant_tests = tests_at_path(path, &branches);

    let check = guarded_tests_are_complete(&relevant_tests);

    let all_edges = relevant_tests
        .into_iter()
        .map(|t| edges_for(interner, path, &branches, t))
        .collect();

    let fallbacks = if check {
        vec![]
    } else {
        branches
            .into_iter()
            .filter(|b| is_irrelevant_to(path, b))
            .collect()
    };

    (all_edges, fallbacks)
}

/// FIND RELEVANT TESTS

fn tests_at_path<'a>(
    selected_path: &[PathInstruction],
    branches: &[Branch<'a>],
) -> Vec<GuardedTest<'a>> {
    // NOTE the ordering of the result is important!

    let mut all_tests = Vec::new();

    for branch in branches {
        all_tests.extend(test_at_path(selected_path, branch));
    }

    // The rust HashMap also uses equality, here we really want to use the custom hash function
    // defined on Test to determine whether a test is unique. So we have to do the hashing
    // explicitly

    use std::collections::hash_map::DefaultHasher;

    let mut visited = MutSet::default();
    let mut unique = Vec::new();

    for test in all_tests {
        let hash = {
            let mut hasher = DefaultHasher::new();
            test.hash(&mut hasher);
            hasher.finish()
        };

        if !visited.contains(&hash) {
            visited.insert(hash);
            unique.push(test);
        }
    }

    unique
}

fn test_for_pattern<'a>(pattern: &Pattern<'a>) -> Option<Test<'a>> {
    use Pattern::*;
    use Test::*;

    let test = match pattern {
        Identifier(_) | Underscore => {
            return None;
        }

        As(subpattern, _) => return test_for_pattern(subpattern),

        RecordDestructure(destructs, _) => {
            // not rendered, so pick the easiest
            let union = Union {
                render_as: RenderAs::Tag,
                alternatives: vec![Ctor {
                    tag_id: TagId(0),
                    name: CtorName::Tag(TagName(RECORD_TAG_NAME.into())),
                    arity: destructs.len(),
                }],
            };

            let mut arguments = std::vec::Vec::new();

            for destruct in destructs {
                match &destruct.typ {
                    DestructType::Guard(guard) => {
                        arguments.push((guard.clone(), destruct.layout));
                    }
                    DestructType::Required(_) => {
                        arguments.push((Pattern::Underscore, destruct.layout));
                    }
                }
            }

            IsCtor {
                tag_id: 0,
                ctor_name: CtorName::Tag(TagName(RECORD_TAG_NAME.into())),
                union,
                arguments,
            }
        }

        TupleDestructure(destructs, _) => {
            // not rendered, so pick the easiest
            let union = Union {
                render_as: RenderAs::Tag,
                alternatives: vec![Ctor {
                    tag_id: TagId(0),
                    name: CtorName::Tag(TagName(TUPLE_TAG_NAME.into())),
                    arity: destructs.len(),
                }],
            };

            let mut arguments = std::vec::Vec::new();

            for destruct in destructs {
                arguments.push((destruct.pat.clone(), destruct.layout));
            }

            IsCtor {
                tag_id: 0,
                ctor_name: CtorName::Tag(TagName(TUPLE_TAG_NAME.into())),
                union,
                arguments,
            }
        }

        NewtypeDestructure {
            tag_name,
            arguments,
        } => {
            let tag_id = 0;
            let union = Union::newtype_wrapper(CtorName::Tag(tag_name.clone()), arguments.len());

            IsCtor {
                tag_id,
                ctor_name: CtorName::Tag(tag_name.clone()),
                union,
                arguments: arguments.to_vec(),
            }
        }

        AppliedTag {
            tag_name,
            tag_id,
            arguments,
            union,
            ..
        } => IsCtor {
            tag_id: *tag_id,
            ctor_name: CtorName::Tag(tag_name.clone()),
            union: union.clone(),
            arguments: arguments.to_vec(),
        },

        List {
            arity,
            list_layout: _,
            element_layout: _,
            elements: _,
            opt_rest: _,
        } => IsListLen {
            bound: match arity {
                ListArity::Exact(_) => ListLenBound::Exact,
                ListArity::Slice(_, _) => ListLenBound::AtLeast,
            },
            len: arity.min_len() as _,
        },

        Voided { .. } => internal_error!("unreachable"),

        OpaqueUnwrap { opaque, argument } => {
            let union = Union {
                render_as: RenderAs::Tag,
                alternatives: vec![Ctor {
                    tag_id: TagId(0),
                    name: CtorName::Opaque(*opaque),
                    arity: 1,
                }],
            };

            IsCtor {
                tag_id: 0,
                ctor_name: CtorName::Opaque(*opaque),
                union,
                arguments: vec![(**argument).clone()],
            }
        }

        BitLiteral { value, .. } => IsBit(*value),
        EnumLiteral { tag_id, union, .. } => IsByte {
            tag_id: *tag_id as _,
            num_alts: union.alternatives.len(),
        },
        IntLiteral(v, precision) => IsInt(*v, *precision),
        FloatLiteral(v, precision) => IsFloat(*v, *precision),
        DecimalLiteral(v) => IsDecimal(*v),
        StrLiteral(v) => IsStr(v.clone()),
    };

    Some(test)
}

fn test_at_path<'a>(
    selected_path: &[PathInstruction],
    branch: &Branch<'a>,
) -> Option<GuardedTest<'a>> {
    let (_, pattern) = branch
        .patterns
        .iter()
        .find(|(path, _)| path == selected_path)?;

    match test_for_pattern(pattern) {
        Some(test) => Some(GuardedTest::TestNotGuarded { test }),
        None => {
            if let Guard::Guard { .. } = &branch.guard {
                // no tests for this pattern remain, but we cannot discard it yet
                // because it has a guard!
                Some(GuardedTest::PlaceholderWithGuard)
            } else {
                None
            }
        }
    }
}

/// BUILD EDGES

// understanding: if the test is successful, where could we go?
fn edges_for<'a>(
    interner: &TLLayoutInterner<'a>,
    path: &[PathInstruction],
    branches: &[Branch<'a>],
    test: GuardedTest<'a>,
) -> (GuardedTest<'a>, Vec<Branch<'a>>) {
    let mut new_branches = Vec::new();

    // if we test for a guard, skip all branches until one that has a guard

    let it = match test {
        GuardedTest::GuardedNoTest { .. } => {
            let index = branches
                .iter()
                .position(|b| b.guard.is_some())
                .expect("if testing for a guard, one branch must have a guard");

            branches[index..].iter()
        }
        GuardedTest::PlaceholderWithGuard => {
            // Skip all branches until we hit the one with a placeholder and a guard.
            let index = branches
                .iter()
                .position(|b| {
                    if b.guard.is_none() {
                        return false;
                    }

                    let (_, pattern) = b
                        .patterns
                        .iter()
                        .find(|(branch_path, _)| branch_path == path)
                        .expect(
                            "if testing for a placeholder with guard, must find a branch matching the path",
                        );

                    test_for_pattern(pattern).is_none()
                })
                .expect("if testing for a guard, one branch must have a guard");

            branches[index..].iter()
        }
        GuardedTest::TestNotGuarded { .. } => branches.iter(),
    };

    for branch in it {
        new_branches.extend(to_relevant_branch(interner, &test, path, branch));
    }

    (test, new_branches)
}

fn to_relevant_branch<'a>(
    interner: &TLLayoutInterner<'a>,
    guarded_test: &GuardedTest<'a>,
    path: &[PathInstruction],
    branch: &Branch<'a>,
) -> Option<Branch<'a>> {
    // TODO remove clone
    match extract(path, branch.patterns.clone()) {
        Extract::NotFound => Some(branch.clone()),
        Extract::Found {
            start,
            found_pattern: pattern,
            end,
        } => match guarded_test {
            GuardedTest::PlaceholderWithGuard | GuardedTest::GuardedNoTest { .. } => {
                // if there is no test, the pattern should not require any
                debug_assert!(
                    matches!(pattern, Pattern::Identifier(_) | Pattern::Underscore,),
                    "{pattern:?}",
                );

                Some(branch.clone())
            }
            GuardedTest::TestNotGuarded { test } => {
                to_relevant_branch_help(interner, test, path, start, end, branch, pattern)
            }
        },
    }
}

fn to_relevant_branch_help<'a>(
    interner: &TLLayoutInterner<'a>,
    test: &Test<'a>,
    path: &[PathInstruction],
    mut start: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
    end: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
    branch: &Branch<'a>,
    pattern: Pattern<'a>,
) -> Option<Branch<'a>> {
    use Pattern::*;
    use Test::*;

    match pattern {
        Identifier(_) | Underscore => Some(branch.clone()),

        As(subpattern, _symbol) => {
            to_relevant_branch_help(interner, test, path, start, end, branch, *subpattern)
        }

        RecordDestructure(destructs, _) => match test {
            IsCtor {
                ctor_name: test_name,
                tag_id,
                ..
            } => {
                debug_assert!(test_name == &CtorName::Tag(TagName(RECORD_TAG_NAME.into())));
                let destructs_len = destructs.len();
                let sub_positions = destructs.into_iter().enumerate().map(|(index, destruct)| {
                    let pattern = match destruct.typ {
                        DestructType::Guard(guard) => guard.clone(),
                        DestructType::Required(_) => Pattern::Underscore,
                    };

                    let mut new_path = path.to_vec();
                    let next_instr = if destructs_len == 1 {
                        PathInstruction::NewType
                    } else {
                        PathInstruction::TagIndex {
                            index: index as u64,
                            tag_id: *tag_id,
                        }
                    };
                    new_path.push(next_instr);

                    (new_path, pattern)
                });
                start.extend(sub_positions);
                start.extend(end);

                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        TupleDestructure(destructs, _) => match test {
            IsCtor {
                ctor_name: test_name,
                tag_id,
                ..
            } => {
                debug_assert!(test_name == &CtorName::Tag(TagName(TUPLE_TAG_NAME.into())));
                let destructs_len = destructs.len();
                let sub_positions = destructs.into_iter().enumerate().map(|(index, destruct)| {
                    let pattern = destruct.pat.clone();

                    let mut new_path = path.to_vec();
                    let next_instr = if destructs_len == 1 {
                        PathInstruction::NewType
                    } else {
                        PathInstruction::TagIndex {
                            index: index as u64,
                            tag_id: *tag_id,
                        }
                    };
                    new_path.push(next_instr);

                    (new_path, pattern)
                });
                start.extend(sub_positions);
                start.extend(end);

                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        OpaqueUnwrap { opaque, argument } => match test {
            IsCtor {
                ctor_name: test_opaque_tag_name,
                tag_id,
                ..
            } => {
                debug_assert_eq!(*tag_id, 0);
                debug_assert_eq!(test_opaque_tag_name, &CtorName::Opaque(opaque));

                let (argument, _) = *argument;

                let mut new_path = path.to_vec();
                new_path.push(PathInstruction::NewType);

                start.push((new_path, argument));
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        List {
            arity: my_arity,
            elements,
            list_layout: _,
            element_layout: _,
            opt_rest: _,
        } => match test {
            IsListLen {
                bound: test_bound,
                len,
            } if my_arity.covers_length(*len as _)
                    // Spread tests [_, ..] can only match spread tests, not exact-sized bounds [_].
                    //
                    // On the other hand, exact-sized tests [_] can match spread bounds [_, ..],
                    // because each spread bound generates 0 or more exact-sized tests.
                    //
                    // See exhaustiveness checking of lists for more details on the tests generated
                    // for spread bounds.
                    && !matches!(
                        (test_bound, my_arity),
                        (ListLenBound::AtLeast, ListArity::Exact(..))
                    ) =>
            {
                let sub_positions = elements.into_iter().enumerate().map(|(index, elem_pat)| {
                    let mut new_path = path.to_vec();

                    let probe_index = ListIndex::from_pattern_index(index, my_arity);

                    let next_instr = PathInstruction::ListIndex {
                        index: probe_index as _,
                    };
                    new_path.push(next_instr);

                    (new_path, elem_pat)
                });
                start.extend(sub_positions);
                start.extend(end);

                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }

            _ => None,
        },

        NewtypeDestructure {
            tag_name,
            arguments,
            ..
        } => match test {
            IsCtor {
                ctor_name: test_name,
                tag_id: test_id,
                ..
            } if test_name.is_tag(&tag_name) => {
                let tag_id = 0;
                debug_assert_eq!(tag_id, *test_id);

                let num_args = arguments.len();
                let sub_positions =
                    arguments
                        .into_iter()
                        .enumerate()
                        .map(|(index, (pattern, _))| {
                            let mut new_path = path.to_vec();
                            let next_instr = if num_args == 1 {
                                PathInstruction::NewType
                            } else {
                                PathInstruction::TagIndex {
                                    index: index as u64,
                                    tag_id,
                                }
                            };
                            new_path.push(next_instr);
                            (new_path, pattern)
                        });
                start.extend(sub_positions);
                start.extend(end);

                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }

            _ => None,
        },

        AppliedTag {
            tag_name,
            tag_id,
            arguments,
            layout,
            ..
        } => {
            match test {
                IsCtor {
                    ctor_name: test_name,
                    tag_id: test_id,
                    ..
                } if test_name.is_tag(&tag_name) => {
                    debug_assert_eq!(tag_id, *test_id);

                    // the test matches the constructor of this pattern
                    match layout {
                        UnionLayout::NonRecursive([[arg]])
                            if matches!(interner.get_repr(*arg), LayoutRepr::Struct([_],)) =>
                        {
                            // a one-element record equivalent
                            // Theory: Unbox doesn't have any value for us
                            debug_assert_eq!(arguments.len(), 1);
                            let arg = arguments[0].clone();
                            {
                                // NOTE here elm unboxes, but we ignore that
                                // Path::Unbox(Box::new(path.clone()))
                                start.push((path.to_vec(), arg.0));
                                start.extend(end);
                            }
                        }
                        UnionLayout::NonRecursive([_]) | UnionLayout::NonNullableUnwrapped(_) => {
                            let sub_positions =
                                arguments
                                    .into_iter()
                                    .enumerate()
                                    .map(|(index, (pattern, _))| {
                                        let mut new_path = path.to_vec();
                                        new_path.push(PathInstruction::TagIndex {
                                            index: index as u64,
                                            tag_id,
                                        });
                                        (new_path, pattern)
                                    });
                            start.extend(sub_positions);
                            start.extend(end);
                        }
                        UnionLayout::NonRecursive(_)
                        | UnionLayout::Recursive(_)
                        | UnionLayout::NullableWrapped { .. }
                        | UnionLayout::NullableUnwrapped { .. } => {
                            let sub_positions =
                                arguments
                                    .into_iter()
                                    .enumerate()
                                    .map(|(index, (pattern, _))| {
                                        let mut new_path = path.to_vec();
                                        new_path.push(PathInstruction::TagIndex {
                                            index: index as u64,
                                            tag_id,
                                        });
                                        (new_path, pattern)
                                    });
                            start.extend(sub_positions);
                            start.extend(end);
                        }
                    }

                    Some(Branch {
                        goal: branch.goal,
                        guard: branch.guard.clone(),
                        patterns: start,
                    })
                }
                _ => None,
            }
        }
        Voided { .. } => internal_error!("unreachable"),
        StrLiteral(string) => match test {
            IsStr(test_str) if string == *test_str => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        IntLiteral(int, p1) => match test {
            IsInt(is_int, p2) if int == *is_int => {
                debug_assert_eq!(p1, *p2);
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        FloatLiteral(float, p1) => match test {
            IsFloat(test_float, p2) if float == *test_float => {
                debug_assert_eq!(p1, *p2);
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        DecimalLiteral(dec) => match test {
            IsDecimal(test_dec) if dec.eq(test_dec) => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        BitLiteral { value: bit, .. } => match test {
            IsBit(test_bit) if bit == *test_bit => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        EnumLiteral { tag_id, .. } => match test {
            IsByte {
                tag_id: test_id, ..
            } if tag_id == *test_id as u8 => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }

            _ => None,
        },
    }
}

enum Extract<'a> {
    NotFound,
    Found {
        start: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
        found_pattern: Pattern<'a>,
        end: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
    },
}

fn extract<'a>(
    selected_path: &[PathInstruction],
    path_patterns: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
) -> Extract<'a> {
    let mut start = Vec::new();

    // TODO potential ordering problem
    let mut it = path_patterns.into_iter();
    while let Some(current) = it.next() {
        if current.0 == selected_path {
            return Extract::Found {
                start,
                found_pattern: current.1,
                end: it.collect::<Vec<_>>(),
            };
        } else {
            start.push(current);
        }
    }

    Extract::NotFound
}

/// FIND IRRELEVANT BRANCHES

fn is_irrelevant_to(selected_path: &[PathInstruction], branch: &Branch) -> bool {
    match branch
        .patterns
        .iter()
        .find(|(path, _)| path == selected_path)
    {
        None => true,
        Some((_, pattern)) => branch.guard.is_none() && !needs_tests(pattern),
    }
}

/// Does this pattern need a branch test?
///
/// Keep up to date with [needs_path_instruction].
fn needs_tests(pattern: &Pattern) -> bool {
    use Pattern::*;

    match pattern {
        Identifier(_) | Underscore => false,

        As(subpattern, _) => needs_tests(subpattern),

        NewtypeDestructure { .. }
        | RecordDestructure(..)
        | TupleDestructure(..)
        | AppliedTag { .. }
        | OpaqueUnwrap { .. }
        | BitLiteral { .. }
        | EnumLiteral { .. }
        | IntLiteral(_, _)
        | FloatLiteral(_, _)
        | DecimalLiteral(_)
        | StrLiteral(_)
        | List { .. } => true,

        Voided { .. } => internal_error!("unreachable"),
    }
}

/// PICK A PATH

fn pick_path<'a>(branches: &'a [Branch]) -> &'a Vec<PathInstruction> {
    let mut all_paths = Vec::with_capacity(branches.len());

    // is choice path
    for branch in branches {
        for (path, pattern) in &branch.patterns {
            // NOTE we no longer check for the guard here
            // if !branch.guard.is_none() || needs_tests(&pattern) {
            if needs_tests(pattern) {
                all_paths.push(path);
            } else {
                // do nothing
            }
        }
    }

    let mut by_small_defaults = bests_by_small_defaults(branches, all_paths.into_iter());

    if by_small_defaults.len() == 1 {
        by_small_defaults.remove(0)
    } else {
        debug_assert!(!by_small_defaults.is_empty());
        let mut result = bests_by_small_branching_factor(branches, by_small_defaults.into_iter());

        match result.pop() {
            None => unreachable!("bests_by will always return at least one value in the vec"),
            Some(path) => path,
        }
    }
}

fn bests_by_small_branching_factor<'a, I>(
    branches: &[Branch],
    mut all_paths: I,
) -> Vec<&'a Vec<PathInstruction>>
where
    I: Iterator<Item = &'a Vec<PathInstruction>>,
{
    match all_paths.next() {
        None => panic!("Cannot choose the best of zero paths. This should never happen."),
        Some(first_path) => {
            let mut min_weight = small_branching_factor(branches, first_path);
            let mut min_paths = vec![first_path];

            for path in all_paths {
                let weight = small_branching_factor(branches, path);

                use std::cmp::Ordering;
                match weight.cmp(&min_weight) {
                    Ordering::Equal => {
                        min_paths.push(path);
                    }
                    Ordering::Less => {
                        min_weight = weight;
                        min_paths.clear();
                        min_paths.push(path);
                    }
                    Ordering::Greater => {}
                }
            }

            min_paths
        }
    }
}

fn bests_by_small_defaults<'a, I>(
    branches: &[Branch],
    mut all_paths: I,
) -> Vec<&'a Vec<PathInstruction>>
where
    I: Iterator<Item = &'a Vec<PathInstruction>>,
{
    match all_paths.next() {
        None => panic!("Cannot choose the best of zero paths. This should never happen."),
        Some(first_path) => {
            let mut min_weight = small_defaults(branches, first_path);
            let mut min_paths = vec![first_path];

            for path in all_paths {
                let weight = small_defaults(branches, path);

                use std::cmp::Ordering;
                match weight.cmp(&min_weight) {
                    Ordering::Equal => {
                        min_paths.push(path);
                    }
                    Ordering::Less => {
                        min_weight = weight;
                        min_paths.clear();
                        min_paths.push(path);
                    }
                    Ordering::Greater => {}
                }
            }

            min_paths
        }
    }
}

/// PATH PICKING HEURISTICS

fn small_defaults(branches: &[Branch], path: &[PathInstruction]) -> usize {
    branches
        .iter()
        .filter(|b| is_irrelevant_to(path, b))
        .map(|_| 1)
        .sum()
}

fn small_branching_factor(branches: &[Branch], path: &[PathInstruction]) -> usize {
    // a specialized version of gather_edges that just counts the number of options

    let relevant_tests = tests_at_path(path, branches);

    let check = guarded_tests_are_complete(&relevant_tests);

    let fallbacks = if check {
        false
    } else {
        branches.iter().any(|b| is_irrelevant_to(path, b))
    };

    relevant_tests.len() + usize::from(fallbacks)
}

#[derive(Debug, PartialEq)]
enum Decider<'a, T> {
    Leaf(T),
    Guarded {
        pattern: Pattern<'a>,

        /// The guard expression and how to compile it.
        stmt_spec: GuardStmtSpec,

        success: Box<Decider<'a, T>>,
        failure: Box<Decider<'a, T>>,
    },
    Chain {
        test_chain: Vec<(Vec<PathInstruction>, Test<'a>)>,
        success: Box<Decider<'a, T>>,
        failure: Box<Decider<'a, T>>,
    },
    FanOut {
        path: Vec<PathInstruction>,
        tests: Vec<(Test<'a>, Decider<'a, T>)>,
        fallback: Box<Decider<'a, T>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum Choice<'a> {
    Inline(Stmt<'a>),
    Jump(Label),
}

type StoresVec<'a> = bumpalo::collections::Vec<'a, (Symbol, InLayout<'a>, Expr<'a>)>;

struct JumpSpec<'a> {
    target_index: u64,
    id: JoinPointId,
    /// Symbols, from the unpacked pattern, to add on when jumping to the target.
    jump_pattern_param_symbols: &'a [Symbol],

    // Used to construct the joinpoint
    join_params: &'a [Param<'a>],
    join_body: Stmt<'a>,
}

pub(crate) fn optimize_when<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    cond_symbol: Symbol,
    cond_layout: InLayout<'a>,
    ret_layout: InLayout<'a>,
    opt_branches: bumpalo::collections::Vec<'a, (Pattern<'a>, Guard<'a>, Stmt<'a>)>,
) -> Stmt<'a> {
    let (patterns, indexed_branches): (_, Vec<_>) = opt_branches
        .into_iter()
        .enumerate()
        .map(|(index, (pattern, guard, branch))| {
            let has_guard = guard.is_some();
            (
                (guard, pattern.clone(), index as u64),
                (index as u64, branch, pattern, has_guard),
            )
        })
        .unzip();

    let decision_tree = compile(&layout_cache.interner, patterns);
    let decider = tree_to_decider(decision_tree);

    // for each target (branch body), count in how many ways it can be reached
    let mut target_counts = bumpalo::vec![in env.arena; 0; indexed_branches.len()];
    count_targets(&mut target_counts, &decider);

    let mut choices = MutMap::default();
    let mut jumps = Vec::new();

    for (target, mut branch, pattern, has_guard) in indexed_branches.into_iter() {
        let should_inline = {
            let target_counts = &target_counts;
            match target_counts.get(target as usize) {
                None => unreachable!(
                    "this should never happen: {:?} not in {:?}",
                    target, target_counts
                ),
                Some(count) => *count == 1,
            }
        };

        let join_params: &'a [Param<'a>];
        let jump_pattern_param_symbols: &'a [Symbol];
        match (has_guard, should_inline) {
            (false, _) => {
                // Bind the fields referenced in the pattern.
                branch = store_pattern(env, procs, layout_cache, &pattern, cond_symbol, branch);

                join_params = &[];
                jump_pattern_param_symbols = &[];
            }
            (true, true) => {
                // Nothing more to do - the patterns will be bound when the guard is evaluated in
                // `decide_to_branching`.
                join_params = &[];
                jump_pattern_param_symbols = &[];
            }
            (true, false) => {
                // The patterns will be bound when the guard is evaluated, and then we need to get
                // them back into the joinpoint here.
                //
                // So, figure out what symbols the pattern binds, and update the joinpoint
                // parameter to take each symbol. Then, when the joinpoint is called, the unpacked
                // symbols will be filled in.
                //
                // Since the joinpoint's parameters will be fresh symbols, the join body also needs
                // updating.
                let pattern_bindings = pattern.collect_symbols(cond_layout);

                let mut parameters_buf = bumpalo::collections::Vec::with_capacity_in(1, env.arena);
                let mut pattern_symbols_buf =
                    bumpalo::collections::Vec::with_capacity_in(1, env.arena);
                let mut substitutions = BumpMap::default();

                for (pattern_symbol, layout) in pattern_bindings {
                    let param_symbol = env.unique_symbol();
                    parameters_buf.push(Param {
                        symbol: param_symbol,
                        layout,
                    });
                    pattern_symbols_buf.push(pattern_symbol);
                    substitutions.insert(pattern_symbol, param_symbol);
                }

                join_params = parameters_buf.into_bump_slice();
                jump_pattern_param_symbols = pattern_symbols_buf.into_bump_slice();

                substitute_in_exprs_many(env.arena, &mut branch, substitutions);
            }
        }

        let ((branch_index, choice), opt_jump) = if should_inline {
            ((target, Choice::Inline(branch)), None)
        } else {
            ((target, Choice::Jump(target)), Some((target, branch)))
        };

        if let Some((target_index, body)) = opt_jump {
            let id = JoinPointId(env.unique_symbol());
            jumps.push(JumpSpec {
                target_index,
                id,
                jump_pattern_param_symbols,
                join_params,
                join_body: body,
            });
        }

        choices.insert(branch_index, choice);
    }

    let choice_decider = insert_choices(&choices, decider);

    let mut stmt = decide_to_branching(
        env,
        procs,
        layout_cache,
        cond_symbol,
        cond_layout,
        ret_layout,
        choice_decider,
        &jumps,
    );

    for JumpSpec {
        target_index: _,
        id,
        jump_pattern_param_symbols: _,
        join_params,
        join_body,
    } in jumps.into_iter()
    {
        stmt = Stmt::Join {
            id,
            parameters: join_params,
            body: env.arena.alloc(join_body),
            remainder: env.arena.alloc(stmt),
        };
    }

    stmt
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PathInstruction {
    NewType,
    TagIndex { index: u64, tag_id: TagIdIntType },
    ListIndex { index: ListIndex },
}

fn path_to_expr_help<'a>(
    env: &mut Env<'a, '_>,
    layout_interner: &mut TLLayoutInterner<'a>,
    mut symbol: Symbol,
    path: &[PathInstruction],
    mut layout: InLayout<'a>,
) -> (Symbol, StoresVec<'a>, InLayout<'a>) {
    let mut stores = bumpalo::collections::Vec::new_in(env.arena);

    // let instructions = reverse_path(path);
    let instructions = path;
    let mut it = instructions.iter().peekable();

    while let Some(path_instr) = it.next() {
        match path_instr {
            PathInstruction::NewType => {
                // pass through
            }

            PathInstruction::TagIndex { index, tag_id } => {
                let index = *index;

                match layout_interner.chase_recursive(layout) {
                    LayoutRepr::Union(union_layout) => {
                        let inner_expr = Expr::UnionAtIndex {
                            tag_id: *tag_id,
                            structure: symbol,
                            index,
                            union_layout,
                        };

                        let inner_layout = union_layout.layout_at(
                            layout_interner,
                            *tag_id as TagIdIntType,
                            index as usize,
                        );

                        symbol = env.unique_symbol();
                        stores.push((symbol, inner_layout, inner_expr));

                        layout = inner_layout;
                    }

                    LayoutRepr::Struct(field_layouts) => {
                        debug_assert!(field_layouts.len() > 1);

                        let inner_expr = Expr::StructAtIndex {
                            index,
                            field_layouts,
                            structure: symbol,
                        };

                        let inner_layout = field_layouts[index as usize];

                        symbol = env.unique_symbol();
                        stores.push((symbol, inner_layout, inner_expr));

                        layout = inner_layout;
                    }

                    _ => {
                        // this MUST be an index into a single-element (hence unwrapped) record

                        debug_assert_eq!(index, 0, "{:?}", &layout);
                        debug_assert_eq!(*tag_id, 0);
                        debug_assert!(it.peek().is_none());

                        break;
                    }
                }
            }

            PathInstruction::ListIndex { index } => {
                let list_sym = symbol;

                match layout_interner.get_repr(layout) {
                    LayoutRepr::Builtin(Builtin::List(elem_layout)) => {
                        let (index_sym, new_stores) = build_list_index_probe(env, list_sym, index);

                        stores.extend(new_stores);

                        let load_sym = env.unique_symbol();
                        let load_expr = Expr::Call(Call {
                            call_type: CallType::LowLevel {
                                op: LowLevel::ListGetUnsafe,
                                update_mode: env.next_update_mode_id(),
                            },
                            arguments: env.arena.alloc([list_sym, index_sym]),
                        });

                        stores.push((load_sym, elem_layout, load_expr));

                        layout = elem_layout;
                        symbol = load_sym;
                    }
                    _ => internal_error!("not a list"),
                }
            }
        }
    }

    (symbol, stores, layout)
}

fn test_to_comparison<'a>(
    env: &mut Env<'a, '_>,
    layout_interner: &mut TLLayoutInterner<'a>,
    cond_symbol: Symbol,
    cond_layout: &InLayout<'a>,
    path: &[PathInstruction],
    test: Test<'a>,
) -> (StoresVec<'a>, Comparison, Option<ConstructorKnown<'a>>) {
    let (rhs_symbol, mut stores, test_layout) =
        path_to_expr_help(env, layout_interner, cond_symbol, path, *cond_layout);

    match test {
        Test::IsCtor { tag_id, union, .. } => {
            let path_symbol = rhs_symbol;
            // the IsCtor check should never be generated for tag unions of size 1
            // (e.g. record pattern guard matches)
            debug_assert!(union.alternatives.len() > 1);

            match layout_interner.chase_recursive(test_layout) {
                LayoutRepr::Union(union_layout) => {
                    let lhs = Expr::Literal(Literal::Int((tag_id as i128).to_ne_bytes()));

                    let rhs = Expr::GetTagId {
                        structure: path_symbol,
                        union_layout,
                    };

                    let lhs_symbol = env.unique_symbol();
                    let rhs_symbol = env.unique_symbol();

                    let tag_id_layout = union_layout.tag_id_layout();

                    stores.push((lhs_symbol, tag_id_layout, lhs));
                    stores.push((rhs_symbol, tag_id_layout, rhs));

                    (
                        stores,
                        (lhs_symbol, Comparator::Eq, rhs_symbol),
                        Some(ConstructorKnown::OneTag {
                            scrutinee: path_symbol,
                            layout: *cond_layout,
                            tag_id,
                        }),
                    )
                }
                _ => unreachable!("{:#?}", (cond_layout, union, test_layout, path)),
            }
        }

        Test::IsInt(test_int, precision) => {
            let lhs = Expr::Literal(Literal::Int(test_int));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::int_width(precision), lhs));

            (stores, (lhs_symbol, Comparator::Eq, rhs_symbol), None)
        }

        Test::IsFloat(test_int, precision) => {
            // TODO maybe we can actually use i64 comparison here?
            let test_float = f64::from_bits(test_int);
            let lhs = Expr::Literal(Literal::Float(test_float));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::float_width(precision), lhs));

            (stores, (lhs_symbol, Comparator::Eq, rhs_symbol), None)
        }

        Test::IsDecimal(test_dec) => {
            let lhs = Expr::Literal(Literal::Decimal(test_dec));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, *cond_layout, lhs));

            (stores, (lhs_symbol, Comparator::Eq, rhs_symbol), None)
        }

        Test::IsByte {
            tag_id: test_byte, ..
        } => {
            debug_assert!(test_byte <= (u8::MAX as u16));

            let lhs = Expr::Literal(Literal::Byte(test_byte as u8));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::U8, lhs));

            (stores, (lhs_symbol, Comparator::Eq, rhs_symbol), None)
        }

        Test::IsBit(test_bit) => {
            let lhs = Expr::Literal(Literal::Bool(test_bit));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::BOOL, lhs));

            (stores, (lhs_symbol, Comparator::Eq, rhs_symbol), None)
        }

        Test::IsStr(test_str) => {
            let lhs = Expr::Literal(Literal::Str(env.arena.alloc(test_str)));
            let lhs_symbol = env.unique_symbol();

            stores.push((lhs_symbol, Layout::STR, lhs));

            (stores, (lhs_symbol, Comparator::Eq, rhs_symbol), None)
        }

        Test::IsListLen { bound, len } => {
            let list_layout = test_layout;
            let list_sym = rhs_symbol;

            match layout_interner.get_repr(list_layout) {
                LayoutRepr::Builtin(Builtin::List(_elem_layout)) => {
                    let real_len_expr = Expr::Call(Call {
                        call_type: CallType::LowLevel {
                            op: LowLevel::ListLenUsize,
                            update_mode: env.next_update_mode_id(),
                        },
                        arguments: env.arena.alloc([list_sym]),
                    });
                    let test_len_expr = Expr::Literal(Literal::Int((len as i128).to_ne_bytes()));

                    let real_len = env.unique_symbol();
                    let test_len = env.unique_symbol();

                    let usize_layout = Layout::usize(env.target);

                    stores.push((real_len, usize_layout, real_len_expr));
                    stores.push((test_len, usize_layout, test_len_expr));

                    let comparison = match bound {
                        ListLenBound::Exact => (real_len, Comparator::Eq, test_len),
                        ListLenBound::AtLeast => (real_len, Comparator::Geq, test_len),
                    };

                    (stores, comparison, None)
                }
                _ => internal_error!(
                    "test path is not a list: {:#?}",
                    (cond_layout, test_layout, path)
                ),
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Comparator {
    Eq,
    Geq,
}

type Comparison = (Symbol, Comparator, Symbol);

type Tests<'a> = std::vec::Vec<(
    bumpalo::collections::Vec<'a, (Symbol, InLayout<'a>, Expr<'a>)>,
    Comparison,
    Option<ConstructorKnown<'a>>,
)>;

fn stores_and_condition<'a>(
    env: &mut Env<'a, '_>,
    layout_interner: &mut TLLayoutInterner<'a>,
    cond_symbol: Symbol,
    cond_layout: &InLayout<'a>,
    test_chain: Vec<(Vec<PathInstruction>, Test<'a>)>,
) -> Tests<'a> {
    let mut tests: Tests = Vec::with_capacity(test_chain.len());

    // Assumption: there is at most 1 guard, and it is the outer layer.
    for (path, test) in test_chain {
        tests.push(test_to_comparison(
            env,
            layout_interner,
            cond_symbol,
            cond_layout,
            &path,
            test,
        ))
    }

    tests
}

#[allow(clippy::too_many_arguments)]
fn compile_test<'a>(
    env: &mut Env<'a, '_>,
    ret_layout: InLayout<'a>,
    stores: bumpalo::collections::Vec<'a, (Symbol, InLayout<'a>, Expr<'a>)>,
    lhs: Symbol,
    cmp: Comparator,
    rhs: Symbol,
    fail: &'a Stmt<'a>,
    cond: Stmt<'a>,
) -> Stmt<'a> {
    compile_test_help(
        env,
        ConstructorKnown::None,
        ret_layout,
        stores,
        lhs,
        cmp,
        rhs,
        fail,
        cond,
    )
}

#[allow(clippy::too_many_arguments)]
fn compile_test_help<'a>(
    env: &mut Env<'a, '_>,
    branch_info: ConstructorKnown<'a>,
    ret_layout: InLayout<'a>,
    stores: bumpalo::collections::Vec<'a, (Symbol, InLayout<'a>, Expr<'a>)>,
    lhs: Symbol,
    cmp: Comparator,
    rhs: Symbol,
    fail: &'a Stmt<'a>,
    mut cond: Stmt<'a>,
) -> Stmt<'a> {
    // if test_symbol then cond else fail
    let test_symbol = env.unique_symbol();
    let arena = env.arena;

    let (pass_info, fail_info) = {
        use ConstructorKnown::*;
        match branch_info {
            BothTags {
                scrutinee,
                layout,
                pass,
                fail,
            } => {
                let pass_info = BranchInfo::Constructor {
                    scrutinee,
                    layout,
                    tag_id: pass,
                };
                let fail_info = BranchInfo::Constructor {
                    scrutinee,
                    layout,
                    tag_id: fail,
                };

                (pass_info, fail_info)
            }

            OneTag {
                scrutinee,
                layout,
                tag_id,
            } => {
                let pass_info = BranchInfo::Constructor {
                    scrutinee,
                    layout,
                    tag_id,
                };

                (pass_info, BranchInfo::None)
            }

            ListLen { scrutinee, len } => {
                let pass_info = BranchInfo::List { scrutinee, len };

                (pass_info, BranchInfo::None)
            }

            None => (BranchInfo::None, BranchInfo::None),
        }
    };

    let branches = env.arena.alloc([(1u64, pass_info, cond)]);
    let default_branch = (fail_info, &*env.arena.alloc(fail.clone()));

    cond = Stmt::Switch {
        cond_symbol: test_symbol,
        cond_layout: Layout::BOOL,
        ret_layout,
        branches,
        default_branch,
    };

    let op = match cmp {
        Comparator::Eq => LowLevel::Eq,
        Comparator::Geq => LowLevel::NumGte,
    };
    let test = Expr::Call(crate::ir::Call {
        call_type: crate::ir::CallType::LowLevel {
            op,
            update_mode: env.next_update_mode_id(),
        },
        arguments: arena.alloc([lhs, rhs]),
    });

    // write to the test symbol
    cond = Stmt::Let(test_symbol, test, Layout::BOOL, arena.alloc(cond));

    // stores are in top-to-bottom order, so we have to add them in reverse
    for (symbol, layout, expr) in stores.into_iter().rev() {
        cond = Stmt::Let(symbol, expr, layout, arena.alloc(cond));
    }

    cond
}

fn compile_tests<'a>(
    env: &mut Env<'a, '_>,
    ret_layout: InLayout<'a>,
    tests: Tests<'a>,
    fail: &'a Stmt<'a>,
    mut cond: Stmt<'a>,
) -> Stmt<'a> {
    for (new_stores, (lhs, cmp, rhs), opt_constructor_info) in tests.into_iter() {
        match opt_constructor_info {
            None => {
                cond = compile_test(env, ret_layout, new_stores, lhs, cmp, rhs, fail, cond);
            }
            Some(cinfo) => {
                cond = compile_test_help(
                    env, cinfo, ret_layout, new_stores, lhs, cmp, rhs, fail, cond,
                );
            }
        }
    }
    cond
}

#[derive(Debug)]
enum ConstructorKnown<'a> {
    BothTags {
        scrutinee: Symbol,
        layout: InLayout<'a>,
        pass: TagIdIntType,
        fail: TagIdIntType,
    },
    OneTag {
        scrutinee: Symbol,
        layout: InLayout<'a>,
        tag_id: TagIdIntType,
    },
    ListLen {
        scrutinee: Symbol,
        len: u64,
    },
    None,
}

impl<'a> ConstructorKnown<'a> {
    fn from_test_chain(
        cond_symbol: Symbol,
        cond_layout: InLayout<'a>,
        test_chain: &[(Vec<PathInstruction>, Test)],
    ) -> Self {
        match test_chain {
            [(path, test)] => match test {
                Test::IsCtor { tag_id, union, .. } if path.is_empty() => {
                    if union.alternatives.len() == 2 {
                        // excluded middle: we also know the tag_id in the fail branch
                        ConstructorKnown::BothTags {
                            layout: cond_layout,
                            scrutinee: cond_symbol,
                            pass: *tag_id,
                            fail: (*tag_id == 0) as _,
                        }
                    } else {
                        ConstructorKnown::OneTag {
                            layout: cond_layout,
                            scrutinee: cond_symbol,
                            tag_id: *tag_id,
                        }
                    }
                }
                Test::IsListLen {
                    bound: ListLenBound::Exact,
                    len,
                } if path.is_empty() => ConstructorKnown::ListLen {
                    scrutinee: cond_symbol,
                    len: *len,
                },
                _ => ConstructorKnown::None,
            },
            _ => ConstructorKnown::None,
        }
    }
}

// TODO procs and layout are currently unused, but potentially required
// for defining optional fields?
// if not, do remove
#[allow(clippy::too_many_arguments, clippy::needless_collect)]
fn decide_to_branching<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    cond_symbol: Symbol,
    cond_layout: InLayout<'a>,
    ret_layout: InLayout<'a>,
    decider: Decider<'a, Choice<'a>>,
    jumps: &[JumpSpec<'a>],
) -> Stmt<'a> {
    use Choice::*;
    use Decider::*;

    let arena = env.arena;

    match decider {
        Leaf(Jump(label)) => {
            let index = jumps
                .binary_search_by_key(&label, |r| r.target_index)
                .expect("jump not in list of jumps");

            Stmt::Jump(jumps[index].id, jumps[index].jump_pattern_param_symbols)
        }
        Leaf(Inline(expr)) => expr,
        Guarded {
            pattern,
            stmt_spec,
            success,
            failure,
        } => {
            // the guard is the final thing that we check, so needs to be layered on first!
            let test_symbol = env.unique_symbol();
            let arena = env.arena;

            let pass_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *success,
                jumps,
            );

            let fail_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *failure,
                jumps,
            );

            let decide = crate::ir::cond(
                env,
                test_symbol,
                Layout::BOOL,
                pass_expr,
                fail_expr,
                ret_layout,
            );

            // calculate the guard value
            let param = Param {
                symbol: test_symbol,
                layout: Layout::BOOL,
            };

            let CompiledGuardStmt {
                join_point_id,
                stmt,
            } = stmt_spec.generate_guard_and_join(env, procs, layout_cache);

            let join = Stmt::Join {
                id: join_point_id,
                parameters: arena.alloc([param]),
                body: arena.alloc(decide),
                remainder: arena.alloc(stmt),
            };

            store_pattern(env, procs, layout_cache, &pattern, cond_symbol, join)
        }
        Chain {
            test_chain,
            success,
            failure,
        } => {
            // generate a (nested) if-then-else

            let pass_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *success,
                jumps,
            );

            let fail_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *failure,
                jumps,
            );

            let chain_branch_info =
                ConstructorKnown::from_test_chain(cond_symbol, cond_layout, &test_chain);

            let tests = stores_and_condition(
                env,
                &mut layout_cache.interner,
                cond_symbol,
                &cond_layout,
                test_chain,
            );

            let number_of_tests = tests.len() as i64;

            debug_assert!(number_of_tests > 0);

            let fail = env.arena.alloc(fail_expr);
            if number_of_tests == 1 {
                // if there is just one test, compile to a simple if-then-else

                let (new_stores, (lhs, cmp, rhs), _cinfo) = tests.into_iter().next().unwrap();

                compile_test_help(
                    env,
                    chain_branch_info,
                    ret_layout,
                    new_stores,
                    lhs,
                    cmp,
                    rhs,
                    fail,
                    pass_expr,
                )
            } else {
                // otherwise, we use a join point so the code for the `else` case
                // is only generated once.
                let fail_jp_id = JoinPointId(env.unique_symbol());
                let jump = arena.alloc(Stmt::Jump(fail_jp_id, &[]));

                let test_stmt = compile_tests(env, ret_layout, tests, jump, pass_expr);

                Stmt::Join {
                    id: fail_jp_id,
                    parameters: &[],
                    body: fail,
                    remainder: arena.alloc(test_stmt),
                }
            }
        }
        FanOut {
            path,
            tests,
            fallback,
        } => {
            // the cond_layout can change in the process. E.g. if the cond is a Tag, we actually
            // switch on the tag discriminant (currently an i64 value)
            // NOTE the tag discriminant is not actually loaded, `cond` can point to a tag
            let (inner_cond_symbol, cond_stores_vec, inner_cond_layout) = path_to_expr_help(
                env,
                &mut layout_cache.interner,
                cond_symbol,
                &path,
                cond_layout,
            );

            let default_branch = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *fallback,
                jumps,
            );

            let mut branches = bumpalo::collections::Vec::with_capacity_in(tests.len(), env.arena);

            let mut tag_id_sum: i64 = (0..tests.len() as i64 + 1).sum();
            let mut union_size: i64 = -1;

            for (test, decider) in tests {
                let branch = decide_to_branching(
                    env,
                    procs,
                    layout_cache,
                    cond_symbol,
                    cond_layout,
                    ret_layout,
                    decider,
                    jumps,
                );

                let tag = match test {
                    Test::IsInt(v, _) => i128::from_ne_bytes(v) as u64,
                    Test::IsFloat(_, _) => unreachable!("floats cannot be switched on"),
                    Test::IsBit(v) => v as u64,
                    Test::IsByte { tag_id, .. } => tag_id as u64,
                    Test::IsCtor { tag_id, .. } => tag_id as u64,
                    Test::IsListLen { len, bound } => match bound {
                        ListLenBound::Exact => len as _,
                        ListLenBound::AtLeast => {
                            unreachable!("at-least bounds cannot be switched on")
                        }
                    },
                    Test::IsDecimal(_) => unreachable!("decimals cannot be switched on"),
                    Test::IsStr(_) => unreachable!("strings cannot be switched on"),
                };

                // branch info is only useful for refcounted values
                let branch_info = match test {
                    Test::IsCtor { tag_id, union, .. } => {
                        tag_id_sum -= tag_id as i64;
                        union_size = union.alternatives.len() as i64;

                        BranchInfo::Constructor {
                            scrutinee: inner_cond_symbol,
                            layout: inner_cond_layout,
                            tag_id,
                        }
                    }
                    Test::IsListLen {
                        bound: ListLenBound::Exact,
                        len,
                    } => {
                        tag_id_sum = -1;
                        BranchInfo::List {
                            scrutinee: inner_cond_symbol,
                            len,
                        }
                    }
                    _ => {
                        tag_id_sum = -1;
                        BranchInfo::None
                    }
                };

                branches.push((tag, branch_info, branch));
            }

            // determine if the switch is exhaustive
            let default_branch_info = if tag_id_sum > 0 && union_size > 0 {
                BranchInfo::Constructor {
                    scrutinee: inner_cond_symbol,
                    layout: inner_cond_layout,
                    tag_id: tag_id_sum as u8 as _,
                }
            } else {
                BranchInfo::None
            };

            // We have learned more about the exact layout of the cond (based on the path)
            // but tests are still relative to the original cond symbol
            let inner_cond_layout_raw = layout_cache.interner.chase_recursive(inner_cond_layout);
            let mut switch = if let LayoutRepr::Union(union_layout) = inner_cond_layout_raw {
                let tag_id_symbol = env.unique_symbol();

                let temp = Stmt::Switch {
                    cond_layout: union_layout.tag_id_layout(),
                    cond_symbol: tag_id_symbol,
                    branches: branches.into_bump_slice(),
                    default_branch: (default_branch_info, env.arena.alloc(default_branch)),
                    ret_layout,
                };

                let expr = Expr::GetTagId {
                    structure: inner_cond_symbol,
                    union_layout,
                };

                Stmt::Let(
                    tag_id_symbol,
                    expr,
                    union_layout.tag_id_layout(),
                    env.arena.alloc(temp),
                )
            } else if let LayoutRepr::Builtin(Builtin::List(_)) = inner_cond_layout_raw {
                let len_symbol = env.unique_symbol();

                let switch = Stmt::Switch {
                    cond_layout: Layout::usize(env.target),
                    cond_symbol: len_symbol,
                    branches: branches.into_bump_slice(),
                    default_branch: (default_branch_info, env.arena.alloc(default_branch)),
                    ret_layout,
                };

                let len_expr = Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::ListLenUsize,
                        update_mode: env.next_update_mode_id(),
                    },
                    arguments: env.arena.alloc([inner_cond_symbol]),
                });

                Stmt::Let(
                    len_symbol,
                    len_expr,
                    Layout::usize(env.target),
                    env.arena.alloc(switch),
                )
            } else {
                Stmt::Switch {
                    cond_layout: inner_cond_layout,
                    cond_symbol: inner_cond_symbol,
                    branches: branches.into_bump_slice(),
                    default_branch: (default_branch_info, env.arena.alloc(default_branch)),
                    ret_layout,
                }
            };

            for (symbol, layout, expr) in cond_stores_vec.into_iter().rev() {
                switch = Stmt::Let(symbol, expr, layout, env.arena.alloc(switch));
            }

            // make a jump table based on the tests
            switch
        }
    }
}

/*
fn boolean_all<'a>(arena: &'a Bump, tests: Vec<(Expr<'a>, Expr<'a>, InLayout<'a>)>) -> Expr<'a> {
    let mut expr = Expr::Bool(true);

    for (lhs, rhs, layout) in tests.into_iter().rev() {
        let test = Expr::RunLowLevel(
            LowLevel::Eq,
            bumpalo::vec![in arena; (lhs, layout.clone()), (rhs, layout.clone())].into_bump_slice(),
        );

        expr = Expr::RunLowLevel(
            LowLevel::And,
            arena.alloc([
                (test, LayoutRepr::Builtin(Builtin::Int1)),
                (expr, LayoutRepr::Builtin(Builtin::Int1)),
            ]),
        );
    }

    expr
}
*/

/// TREE TO DECIDER
///
/// Decision trees may have some redundancies, so we convert them to a Decider
/// which has special constructs to avoid code duplication when possible.

/// If a test always succeeds, we don't need to branch on it
/// this saves on work and jumps
fn test_always_succeeds(test: &Test) -> bool {
    match test {
        Test::IsCtor { union, .. } => union.alternatives.len() == 1,
        _ => false,
    }
}

fn sort_edge_tests_by_priority(edges: &mut [Edge<'_>]) {
    use std::cmp::{Ordering, Ordering::*};
    use GuardedTest::*;
    edges.sort_by(|(t1, _), (t2, _)| match (t1, t2) {
        // Guarded takes priority
        (GuardedNoTest { .. }, GuardedNoTest { .. }) => Equal,
        (GuardedNoTest { .. }, TestNotGuarded { .. })
        | (GuardedNoTest { .. }, PlaceholderWithGuard) => Less,
        // Interesting case: what test do we pick?
        (TestNotGuarded { test: t1 }, TestNotGuarded { test: t2 }) => order_tests(t1, t2),
        // Otherwise we are between guarded and fall-backs
        (TestNotGuarded { .. }, GuardedNoTest { .. }) => Greater,
        (TestNotGuarded { .. }, PlaceholderWithGuard) => Less,
        // Placeholder is always last
        (PlaceholderWithGuard, PlaceholderWithGuard) => Equal,
        (PlaceholderWithGuard, GuardedNoTest { .. })
        | (PlaceholderWithGuard, TestNotGuarded { .. }) => Greater,
    });

    fn order_tests(t1: &Test, t2: &Test) -> Ordering {
        match (t1, t2) {
            (
                Test::IsListLen {
                    bound: bound_l,
                    len: l,
                },
                Test::IsListLen {
                    bound: bound_m,
                    len: m,
                },
            ) => {
                // List tests can either check for
                //   - exact length (= l)
                //   - a size greater or equal to a given length (>= l)
                // (>= l) tests can be superset of other tests
                //   - (>= m) where m > l
                //   - (= m)
                // So, if m > l, we enforce the following order for list tests
                //   (>= m) then (= l) then (>= l)
                match m.cmp(l) {
                    Less => Less, // (>= m) then (>= l)
                    Greater => Greater,

                    Equal => {
                        use ListLenBound::*;
                        match (bound_l, bound_m) {
                            (Exact, AtLeast) => Less, // (= l) then (>= l)
                            (AtLeast, Exact) => Greater,

                            (AtLeast, AtLeast) | (Exact, Exact) => Equal,
                        }
                    }
                }
            }

            (Test::IsListLen { .. }, t) | (t, Test::IsListLen { .. }) => internal_error!(
                "list-length tests should never pair with another test {t:?} at the same level"
            ),
            // We don't care about anything other than list-length tests, since all other tests
            // should be disjoint.
            _ => Equal,
        }
    }
}

fn tree_to_decider(tree: DecisionTree) -> Decider<u64> {
    use Decider::*;
    use DecisionTree::*;

    match tree {
        Match(target) => Leaf(target),

        Decision {
            path,
            mut edges,
            default,
        } => {
            // Some of the head-constructor tests we generate can be supersets of other tests.
            // Edges must be ordered so that more general tests always happen after their
            // specialized variants.
            //
            // For example, patterns
            //
            //   [1, ..] -> ...
            //   [2, 1, ..] -> ...
            //
            // may generate the edges
            //
            //   ListLen(>=1) -> <rest>
            //   ListLen(>=2) -> <rest>
            //
            // but evaluated in exactly this order, the second edge is never reachable.
            // The necessary ordering is
            //
            //   ListLen(>=2) -> <rest>
            //   ListLen(>=1) -> <rest>
            sort_edge_tests_by_priority(&mut edges);

            match default {
                None => match edges.len() {
                    0 => panic!("compiler bug, somehow created an empty decision tree"),
                    1 => {
                        let (_, sub_tree) = edges.remove(0);

                        tree_to_decider(sub_tree)
                    }
                    2 => {
                        let (_, failure_tree) = edges.remove(1);
                        let (guarded_test, success_tree) = edges.remove(0);

                        chain_decider(path, guarded_test, failure_tree, success_tree)
                    }

                    _ => {
                        let fallback = edges.remove(edges.len() - 1).1;

                        fanout_decider(path, fallback, edges)
                    }
                },

                Some(last) => match edges.len() {
                    0 => tree_to_decider(*last),
                    1 => {
                        let failure_tree = *last;
                        let (guarded_test, success_tree) = edges.remove(0);

                        chain_decider(path, guarded_test, failure_tree, success_tree)
                    }

                    _ => {
                        let fallback = *last;

                        fanout_decider(path, fallback, edges)
                    }
                },
            }
        }
    }
}

fn fanout_decider<'a>(
    path: Vec<PathInstruction>,
    fallback: DecisionTree<'a>,
    edges: Vec<(GuardedTest<'a>, DecisionTree<'a>)>,
) -> Decider<'a, u64> {
    let fallback_decider = tree_to_decider(fallback);
    let necessary_tests: Vec<_> = edges
        .into_iter()
        .map(|(test, tree)| fanout_decider_help(tree, test))
        .collect();

    if necessary_tests.iter().all(|(t, _)| t.can_be_switch()) {
        Decider::FanOut {
            path,
            tests: necessary_tests,
            fallback: Box::new(fallback_decider),
        }
    } else {
        // in llvm, we cannot switch on strings so must chain
        let mut decider = fallback_decider;

        for (test, branch_decider) in necessary_tests.into_iter().rev() {
            decider = Decider::Chain {
                test_chain: vec![(path.clone(), test)],
                success: Box::new(branch_decider),
                failure: Box::new(decider),
            };
        }

        decider
    }
}

fn fanout_decider_help<'a>(
    dectree: DecisionTree<'a>,
    guarded_test: GuardedTest<'a>,
) -> (Test<'a>, Decider<'a, u64>) {
    match guarded_test {
        GuardedTest::PlaceholderWithGuard | GuardedTest::GuardedNoTest { .. } => {
            unreachable!("this would not end up in a switch")
        }
        GuardedTest::TestNotGuarded { test } => {
            let decider = tree_to_decider(dectree);
            (test, decider)
        }
    }
}

fn chain_decider<'a>(
    path: Vec<PathInstruction>,
    guarded_test: GuardedTest<'a>,
    failure_tree: DecisionTree<'a>,
    success_tree: DecisionTree<'a>,
) -> Decider<'a, u64> {
    match guarded_test {
        GuardedTest::GuardedNoTest { pattern, stmt_spec } => {
            let failure = Box::new(tree_to_decider(failure_tree));
            let success = Box::new(tree_to_decider(success_tree));

            Decider::Guarded {
                pattern,
                stmt_spec,
                success,
                failure,
            }
        }
        GuardedTest::TestNotGuarded { test } => {
            if test_always_succeeds(&test) {
                tree_to_decider(success_tree)
            } else {
                to_chain(path, test, success_tree, failure_tree)
            }
        }

        GuardedTest::PlaceholderWithGuard => {
            // ?
            tree_to_decider(success_tree)
        }
    }
}

fn to_chain<'a>(
    path: Vec<PathInstruction>,
    test: Test<'a>,
    success_tree: DecisionTree<'a>,
    failure_tree: DecisionTree<'a>,
) -> Decider<'a, u64> {
    use Decider::*;

    let failure = tree_to_decider(failure_tree);

    match tree_to_decider(success_tree) {
        Chain {
            mut test_chain,
            success,
            failure: sub_failure,
        } if failure == *sub_failure => {
            test_chain.push((path, test));

            Chain {
                test_chain,
                success,
                failure: Box::new(failure),
            }
        }

        success => Chain {
            test_chain: vec![(path, test)],
            success: Box::new(success),
            failure: Box::new(failure),
        },
    }
}

/// INSERT CHOICES
///
/// If a target appears exactly once in a Decider, the corresponding expression
/// can be inlined. Whether things are inlined or jumps is called a "choice".

fn count_targets(targets: &mut bumpalo::collections::Vec<u64>, initial: &Decider<u64>) {
    use Decider::*;

    let mut stack = vec![initial];

    while let Some(decision_tree) = stack.pop() {
        match decision_tree {
            Leaf(target) => {
                targets[*target as usize] += 1;
            }

            Guarded {
                success, failure, ..
            } => {
                stack.push(success);
                stack.push(failure);
            }

            Chain {
                success, failure, ..
            } => {
                stack.push(success);
                stack.push(failure);
            }

            FanOut {
                tests, fallback, ..
            } => {
                stack.push(fallback);

                for (_, decider) in tests {
                    stack.push(decider);
                }
            }
        }
    }
}

fn insert_choices<'a>(
    choice_dict: &MutMap<u64, Choice<'a>>,
    decider: Decider<'a, u64>,
) -> Decider<'a, Choice<'a>> {
    use Decider::*;
    match decider {
        Leaf(target) => {
            // TODO remove clone
            // Only targes that appear once are Inline, so it's safe to remove them from the dict.
            Leaf(choice_dict[&target].clone())
        }

        Guarded {
            pattern,
            stmt_spec,
            success,
            failure,
        } => Guarded {
            pattern,
            stmt_spec,
            success: Box::new(insert_choices(choice_dict, *success)),
            failure: Box::new(insert_choices(choice_dict, *failure)),
        },

        Chain {
            test_chain,
            success,
            failure,
        } => Chain {
            test_chain,
            success: Box::new(insert_choices(choice_dict, *success)),
            failure: Box::new(insert_choices(choice_dict, *failure)),
        },

        FanOut {
            path,
            tests,
            fallback,
        } => FanOut {
            path,
            tests: tests
                .into_iter()
                .map(|(test, nested)| (test, insert_choices(choice_dict, nested)))
                .collect(),
            fallback: Box::new(insert_choices(choice_dict, *fallback)),
        },
    }
}
