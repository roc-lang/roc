use crate::error::canonicalize::{to_circular_def_doc, CIRCULAR_DEF};
use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder, Severity};
use roc_can::expected::{Expected, PExpected};
use roc_collections::all::{HumanIndex, MutSet, SendMap};
use roc_collections::VecMap;
use roc_error_macros::internal_error;
use roc_exhaustive::{CtorName, ListArity};
use roc_module::called_via::{BinOp, CalledVia};
use roc_module::ident::{IdentStr, Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::{LineInfo, Region};
use roc_solve_problem::{
    NotDerivableContext, NotDerivableDecode, NotDerivableEq, TypeError, UnderivableReason,
    Unfulfilled,
};
use roc_std::RocDec;
use roc_types::pretty_print::{Parens, WILDCARD};
use roc_types::types::{
    AbilitySet, AliasKind, Category, ErrorType, PatternCategory, Polarity, Reason, RecordField,
    TypeExt,
};
use std::path::PathBuf;
use ven_pretty::DocAllocator;

const ADD_ANNOTATIONS: &str = r#"Can more type annotations be added? Type annotations always help me give more specific messages, and I think they could help a lot in this case"#;

const OPAQUE_NUM_SYMBOLS: &[Symbol] = &[
    Symbol::NUM_NUM,
    Symbol::NUM_INTEGER,
    Symbol::NUM_FLOATINGPOINT,
];

pub fn type_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    problem: TypeError,
) -> Option<Report<'b>> {
    use TypeError::*;

    fn report(title: String, doc: RocDocBuilder<'_>, filename: PathBuf) -> Option<Report<'_>> {
        Some(Report {
            title,
            filename,
            doc,
            severity: Severity::RuntimeError,
        })
    }

    match problem {
        BadExpr(region, category, found, expected) => Some(to_expr_report(
            alloc, lines, filename, region, category, found, expected,
        )),
        BadPattern(region, category, found, expected) => Some(to_pattern_report(
            alloc, lines, filename, region, category, found, expected,
        )),
        CircularType(region, symbol, overall_type) => Some(to_circular_report(
            alloc,
            lines,
            filename,
            region,
            symbol,
            overall_type,
        )),
        UnexposedLookup(symbol) => {
            let title = "UNRECOGNIZED NAME".to_string();
            let doc = alloc
                .stack(vec![alloc
                    .reflow("The ")
                    .append(alloc.module(symbol.module_id()))
                    .append(alloc.reflow(" module does not expose anything by the name "))
                    .append(alloc.symbol_unqualified(symbol))])
                .append(alloc.reflow("."));

            report(title, doc, filename)
        }
        UnfulfilledAbility(incomplete) => {
            let title = "INCOMPLETE ABILITY IMPLEMENTATION".to_string();

            let doc = report_unfulfilled_ability(alloc, lines, incomplete);

            report(title, doc, filename)
        }
        BadExprMissingAbility(region, _category, _found, incomplete) => {
            if region == roc_can::DERIVED_REGION {
                return None;
            }

            let incomplete = incomplete
                .into_iter()
                .map(|unfulfilled| report_unfulfilled_ability(alloc, lines, unfulfilled));
            let note = alloc.stack(incomplete);
            let snippet = alloc.region(lines.convert_region(region));
            let stack = [
                alloc.text(
                    "This expression has a type that does not implement the abilities it's expected to:",
                ),
                snippet,
                note
            ];

            let report = Report {
                title: "TYPE MISMATCH".to_string(),
                filename,
                doc: alloc.stack(stack),
                severity: Severity::RuntimeError,
            };
            Some(report)
        }
        BadPatternMissingAbility(region, _category, _found, incomplete) => {
            let incomplete = incomplete
                .into_iter()
                .map(|unfulfilled| report_unfulfilled_ability(alloc, lines, unfulfilled));
            let note = alloc.stack(incomplete);
            let snippet = alloc.region(lines.convert_region(region));
            let stack = [
                alloc.text(
                    "This expression has a type does not implement the abilities it's expected to:",
                ),
                snippet,
                note,
            ];

            let report = Report {
                title: "TYPE MISMATCH".to_string(),
                filename,
                doc: alloc.stack(stack),
                severity: Severity::RuntimeError,
            };
            Some(report)
        }
        Exhaustive(problem) => Some(exhaustive_problem(alloc, lines, filename, problem)),
        CircularDef(entries) => {
            let doc = to_circular_def_doc(alloc, lines, &entries);
            let title = CIRCULAR_DEF.to_string();
            let severity = Severity::RuntimeError;

            Some(Report {
                title,
                filename,
                doc,
                severity,
            })
        }
        StructuralSpecialization {
            region,
            typ,
            ability,
            member,
        } => {
            let stack = [
                alloc.concat([
                    alloc.reflow("This specialization of "),
                    alloc.symbol_unqualified(member),
                    alloc.reflow(" is for a non-opaque type:"),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.reflow("It is specialized for"),
                alloc.type_block(error_type_to_doc(alloc, typ)),
                alloc.reflow("but structural types can never specialize abilities!"),
                alloc.note("").append(alloc.concat([
                    alloc.symbol_unqualified(member),
                    alloc.reflow(" is a member of "),
                    alloc.symbol_qualified(ability),
                ])),
            ];

            Some(Report {
                title: "ILLEGAL SPECIALIZATION".to_string(),
                filename,
                doc: alloc.stack(stack),
                severity: Severity::RuntimeError,
            })
        }
        WrongSpecialization {
            region,
            ability_member,
            expected_opaque,
            found_opaque,
        } => {
            let stack = [
                alloc.concat([
                    alloc.reflow("This specialization of "),
                    alloc.symbol_unqualified(ability_member),
                    alloc.reflow(" is not for the expected type:"),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.concat([
                    alloc.reflow("It was previously claimed to be a specialization for "),
                    alloc.symbol_unqualified(expected_opaque),
                    alloc.reflow(", but was determined to actually specialize "),
                    alloc.symbol_unqualified(found_opaque),
                    alloc.reflow("!"),
                ]),
            ];

            Some(Report {
                title: "WRONG SPECIALIZATION TYPE".to_string(),
                filename,
                doc: alloc.stack(stack),
                severity: Severity::RuntimeError,
            })
        }
    }
}

fn report_unfulfilled_ability<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    unfulfilled: Unfulfilled,
) -> RocDocBuilder<'a> {
    match unfulfilled {
        Unfulfilled::OpaqueDoesNotImplement { typ, ability } => {
            let stack = vec![alloc.concat([
                alloc.reflow("The type "),
                alloc.symbol_unqualified(typ),
                alloc.reflow(" does not fully implement the ability "),
                alloc.symbol_unqualified(ability),
                alloc.reflow("."),
            ])];

            alloc.stack(stack)
        }
        Unfulfilled::AdhocUnderivable {
            typ,
            ability,
            reason,
        } => {
            let reason = report_underivable_reason(alloc, reason, ability, &typ);
            let stack = [
                alloc.concat([
                    alloc.reflow("I can't generate an implementation of the "),
                    alloc.symbol_foreign_qualified(ability),
                    alloc.reflow(" ability for"),
                ]),
                alloc.type_block(error_type_to_doc(alloc, typ)),
            ]
            .into_iter()
            .chain(reason);

            alloc.stack(stack)
        }
        Unfulfilled::OpaqueUnderivable {
            typ,
            ability,
            opaque,
            derive_region,
            reason,
        } => {
            let reason = report_underivable_reason(alloc, reason, ability, &typ);
            let stack = [
                alloc.concat([
                    alloc.reflow("I can't derive an implementation of the "),
                    alloc.symbol_foreign_qualified(ability),
                    alloc.reflow(" ability for "),
                    alloc.symbol_foreign_qualified(opaque),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(derive_region)),
            ]
            .into_iter()
            .chain(reason)
            .chain(std::iter::once(alloc.tip().append(alloc.concat([
                alloc.reflow("You can define a custom implementation of "),
                alloc.symbol_unqualified(ability),
                alloc.reflow(" for "),
                alloc.symbol_unqualified(opaque),
                alloc.reflow("."),
            ]))));

            alloc.stack(stack)
        }
    }
}

fn report_underivable_reason<'a>(
    alloc: &'a RocDocAllocator<'a>,
    reason: UnderivableReason,
    ability: Symbol,
    typ: &ErrorType,
) -> Option<RocDocBuilder<'a>> {
    match reason {
        UnderivableReason::NotABuiltin => {
            Some(alloc.reflow("Only builtin abilities can have generated implementations!"))
        }
        UnderivableReason::SurfaceNotDerivable(context) => {
            underivable_hint(alloc, ability, context, typ)
        }
        UnderivableReason::NestedNotDerivable(nested_typ, context) => {
            let hint = underivable_hint(alloc, ability, context, &nested_typ);
            let reason = alloc.stack(
                [
                    alloc.reflow("In particular, an implementation for"),
                    alloc.type_block(error_type_to_doc(alloc, nested_typ)),
                    alloc.reflow("cannot be generated."),
                ]
                .into_iter()
                .chain(hint),
            );
            Some(reason)
        }
    }
}

fn underivable_hint<'b>(
    alloc: &'b RocDocAllocator<'b>,
    ability: Symbol,
    context: NotDerivableContext,
    typ: &ErrorType,
) -> Option<RocDocBuilder<'b>> {
    match context {
        NotDerivableContext::NoContext => None,
        NotDerivableContext::Function => Some(alloc.note("").append(alloc.concat([
            alloc.symbol_unqualified(ability),
            alloc.reflow(" cannot be generated for functions."),
        ]))),
        NotDerivableContext::Opaque(symbol) => Some(alloc.tip().append(alloc.concat([
            alloc.symbol_unqualified(symbol),
            alloc.reflow(" does not implement "),
            alloc.symbol_unqualified(ability),
            alloc.reflow("."),
            if symbol.module_id() == alloc.home {
                alloc.concat([
                    alloc.reflow(" Consider adding a custom implementation"),
                    if ability.is_builtin() {
                        alloc.concat([
                            alloc.reflow(" or "),
                            alloc.inline_type_block(alloc.concat([
                                alloc.keyword("has"),
                                alloc.space(),
                                alloc.symbol_qualified(ability),
                            ])),
                            alloc.reflow(" to the definition of "),
                            alloc.symbol_unqualified(symbol),
                        ])
                    } else {
                        alloc.nil()
                    },
                    alloc.reflow("."),
                ])
            } else {
                alloc.nil()
            },
        ]))),
        NotDerivableContext::UnboundVar => {
            let v = match typ {
                ErrorType::FlexVar(v) => v,
                ErrorType::RigidVar(v) => v,
                _ => internal_error!("unbound variable context only applicable for variables"),
            };

            Some(alloc.tip().append(alloc.concat([
                alloc.reflow("This type variable is not bound to "),
                alloc.symbol_unqualified(ability),
                alloc.reflow(". Consider adding a "),
                alloc.keyword("has"),
                alloc.reflow(" clause to bind the type variable, like "),
                alloc.inline_type_block(alloc.concat([
                    alloc.string("| ".to_string()),
                    alloc.type_variable(v.clone()),
                    alloc.space(),
                    alloc.keyword("has"),
                    alloc.space(),
                    alloc.symbol_qualified(ability),
                ])),
            ])))
        }
        NotDerivableContext::Decode(reason) => match reason {
            NotDerivableDecode::OptionalRecordField(field) => {
                Some(alloc.note("").append(alloc.concat([
                    alloc.reflow("I can't derive decoding for a record with an optional field, which in this case is "),
                    alloc.record_field(field),
                    alloc.reflow(". Optional record fields are polymorphic over records that may or may not contain them at compile time, "),
                    alloc.reflow("but are not a concept that extends to runtime!"),
                    alloc.hardline(),
                    alloc.reflow("Maybe you wanted to use a "),
                    alloc.symbol_unqualified(Symbol::RESULT_RESULT),
                    alloc.reflow("?"),
                ])))
            }
        },
        NotDerivableContext::Eq(reason) => match reason {
            NotDerivableEq::FloatingPoint => {
                Some(alloc.note("").append(alloc.concat([
                    alloc.reflow("I can't derive "),
                    alloc.symbol_qualified(Symbol::BOOL_IS_EQ),
                    alloc.reflow(" for floating-point types. That's because Roc's floating-point numbers cannot be compared for total equality - in Roc, `NaN` is never comparable to `NaN`."),
                    alloc.reflow(" If a type doesn't support total equality, it cannot support the "),
                    alloc.symbol_unqualified(Symbol::BOOL_EQ),
                    alloc.reflow(" ability!"),
                ])))
            }
        },
    }
}

pub fn cyclic_alias<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    symbol: Symbol,
    region: roc_region::all::Region,
    others: Vec<Symbol>,
    alias_kind: AliasKind,
) -> (RocDocBuilder<'b>, String) {
    let when_is_recursion_legal =
        alloc.reflow("Recursion in ")
        .append(alloc.reflow(alias_kind.as_str()))
        .append(alloc.reflow("es is only allowed if recursion happens behind a tagged union, at least one variant of which is not recursive."));

    let doc = if others.is_empty() {
        alloc.stack([
            alloc
                .reflow("The ")
                .append(alloc.symbol_unqualified(symbol))
                .append(alloc.reflow(" "))
                .append(alloc.reflow(alias_kind.as_str()))
                .append(alloc.reflow(" is self-recursive in an invalid way:")),
            alloc.region(lines.convert_region(region)),
            when_is_recursion_legal,
        ])
    } else {
        alloc.stack([
            alloc
                .reflow("The ")
                .append(alloc.symbol_unqualified(symbol))
                .append(alloc.reflow(" "))
                .append(alloc.reflow(alias_kind.as_str()))
                .append(alloc.reflow(" is recursive in an invalid way:")),
            alloc.region(lines.convert_region(region)),
            alloc
                .reflow("The ")
                .append(alloc.symbol_unqualified(symbol))
                .append(alloc.reflow(" "))
                .append(alloc.reflow(alias_kind.as_str()))
                .append(
                    alloc.reflow(" depends on itself through the following chain of definitions:"),
                ),
            crate::report::cycle(
                alloc,
                4,
                alloc.symbol_unqualified(symbol),
                others
                    .into_iter()
                    .map(|other| alloc.symbol_unqualified(other))
                    .collect::<Vec<_>>(),
            ),
            when_is_recursion_legal,
        ])
    };

    (doc, "CYCLIC ALIAS".to_string())
}

#[allow(clippy::too_many_arguments)]
fn report_mismatch<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    category: &Category,
    found: ErrorType,
    expected_type: ErrorType,
    region: roc_region::all::Region,
    opt_highlight: Option<roc_region::all::Region>,
    problem: RocDocBuilder<'b>,
    this_is: RocDocBuilder<'b>,
    instead_of: RocDocBuilder<'b>,
    further_details: Option<RocDocBuilder<'b>>,
) -> Report<'b> {
    let snippet = if let Some(highlight) = opt_highlight {
        alloc.region_with_subregion(
            lines.convert_region(highlight),
            lines.convert_region(region),
        )
    } else {
        alloc.region(lines.convert_region(region))
    };
    let lines = vec![
        problem,
        snippet,
        type_comparison(
            alloc,
            found,
            expected_type,
            ExpectationContext::Arbitrary,
            add_category(alloc, this_is, category),
            instead_of,
            further_details,
        ),
    ];

    Report {
        title: "TYPE MISMATCH".to_string(),
        filename,
        doc: alloc.stack(lines),
        severity: Severity::RuntimeError,
    }
}

#[allow(clippy::too_many_arguments)]
fn report_bad_type<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    category: &Category,
    found: ErrorType,
    expected_type: ErrorType,
    region: roc_region::all::Region,
    opt_highlight: Option<roc_region::all::Region>,
    problem: RocDocBuilder<'b>,
    this_is: RocDocBuilder<'b>,
    further_details: RocDocBuilder<'b>,
) -> Report<'b> {
    let snippet = if let Some(highlight) = opt_highlight {
        alloc.region_with_subregion(
            lines.convert_region(highlight),
            lines.convert_region(region),
        )
    } else {
        alloc.region(lines.convert_region(region))
    };
    let lines = vec![
        problem,
        snippet,
        lone_type(
            alloc,
            found,
            expected_type,
            ExpectationContext::Arbitrary,
            add_category(alloc, this_is, category),
            further_details,
        ),
    ];

    Report {
        title: "TYPE MISMATCH".to_string(),
        filename,
        doc: alloc.stack(lines),
        severity: Severity::RuntimeError,
    }
}

fn pattern_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: &roc_can::pattern::Pattern,
) -> Option<RocDocBuilder<'b>> {
    use roc_can::pattern::Pattern::*;

    match pattern {
        Identifier(symbol) => Some(alloc.symbol_unqualified(*symbol)),
        _ => None,
    }
}

fn lowercase_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => Default::default(),
        Some(c) => c.to_lowercase().chain(chars).collect(),
    }
}

fn to_expr_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    expr_region: roc_region::all::Region,
    category: Category,
    found: ErrorType,
    expected: Expected<ErrorType>,
) -> Report<'b> {
    match expected {
        Expected::NoExpectation(expected_type) => {
            // If it looks like a record field typo, early return with a special report for that.
            if let ErrorType::Record(expected_fields, _) =
                expected_type.clone().unwrap_structural_alias()
            {
                if let ErrorType::Record(found_fields, found_ext) =
                    found.clone().unwrap_structural_alias()
                {
                    let expected_set: MutSet<_> = expected_fields.keys().cloned().collect();
                    let found_set: MutSet<_> = found_fields.keys().cloned().collect();
                    let mut diff = expected_set.difference(&found_set);

                    if let Some(field) = diff.next() {
                        let opt_sym = match category {
                            Category::Lookup(name) => Some(name),
                            _ => None,
                        };
                        return report_record_field_typo(
                            alloc,
                            lines,
                            filename,
                            opt_sym,
                            ".",
                            field,
                            "",
                            expr_region,
                            found_fields,
                            found_ext,
                        );
                    }
                }
            };

            let comparison = type_comparison(
                alloc,
                found,
                expected_type,
                ExpectationContext::Arbitrary,
                add_category(alloc, alloc.text("It is"), &category),
                alloc.text("But you are trying to use it as:"),
                None,
            );

            Report {
                filename,
                title: "TYPE MISMATCH".to_string(),
                doc: alloc.stack([
                    alloc.text("This expression is used in an unexpected way:"),
                    alloc.region(lines.convert_region(expr_region)),
                    comparison,
                ]),
                severity: Severity::RuntimeError,
            }
        }
        Expected::FromAnnotation(name, _arity, annotation_source, expected_type) => {
            use roc_types::types::AnnotationSource::*;

            let (the_name_text, on_name_text) = match pattern_to_doc(alloc, &name.value) {
                Some(doc) => (
                    alloc.concat([alloc.reflow("the "), doc.clone()]),
                    alloc.concat([alloc.reflow(" on "), doc]),
                ),
                None => (alloc.text("this"), alloc.nil()),
            };

            let ann_region = annotation_source.region();

            let thing = match annotation_source {
                TypedIfBranch {
                    index,
                    num_branches,
                    ..
                } if num_branches == 2 => alloc.concat([
                    alloc.keyword(if index == HumanIndex::FIRST {
                        "then"
                    } else {
                        "else"
                    }),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("if"),
                    alloc.text(" expression:"),
                ]),
                TypedIfBranch { index, .. } => alloc.concat([
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("if"),
                    alloc.text(" expression:"),
                ]),
                TypedWhenBranch { index, .. } => alloc.concat([
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("when"),
                    alloc.text(" expression:"),
                ]),
                TypedBody { .. } => alloc.concat([
                    alloc.text("body of "),
                    the_name_text,
                    alloc.text(" definition:"),
                ]),
                RequiredSymbol { .. } => alloc.concat([
                    alloc.text("type annotation of "),
                    the_name_text,
                    alloc.text(" required symbol:"),
                ]),
            };

            let it_is = match annotation_source {
                TypedIfBranch { index, .. } => format!("The {} branch is", index.ordinal()),
                TypedWhenBranch { index, .. } => format!("The {} branch is", index.ordinal()),
                TypedBody { .. } => "The body is".into(),
                RequiredSymbol { .. } => "The provided type is".into(),
            };

            let expectation_context = ExpectationContext::Annotation {
                on: on_name_text.clone(),
            };

            let comparison = if diff_is_wildcard_comparison(
                alloc,
                found.clone(),
                expected_type.clone(),
            ) {
                let it_is = lowercase_first(&it_is);
                let (it, _) = format_category(alloc, alloc.text(it_is), &category, false);
                lone_type(
                        alloc,
                        found,
                        expected_type,
                        expectation_context,
                        alloc.concat([
                            alloc.reflow("The type annotation"),
                            on_name_text,
                            alloc.reflow(" says "),
                            it.clone(),
                            alloc.reflow(" should have the type:"),
                        ]),
                        alloc.concat([
                            alloc.reflow("However, the type of "),
                            it,
                            alloc.reflow(" is connected to another type in a way that isn't reflected in this annotation.")
                        ]),
                    )
            } else {
                type_comparison(
                    alloc,
                    found,
                    expected_type,
                    expectation_context,
                    add_category(alloc, alloc.text(it_is), &category),
                    alloc.concat([
                        alloc.text("But the type annotation"),
                        on_name_text,
                        alloc.text(" says it should be:"),
                    ]),
                    None,
                )
            };

            Report {
                title: "TYPE MISMATCH".to_string(),
                filename,
                doc: alloc.stack([
                    alloc.text("Something is off with the ").append(thing),
                    {
                        // for typed bodies, include the line(s) with the signature
                        let joined =
                            roc_region::all::Region::span_across(&ann_region, &expr_region);
                        alloc.region_with_subregion(
                            lines.convert_region(joined),
                            lines.convert_region(expr_region),
                        )
                    },
                    comparison,
                ]),
                severity: Severity::RuntimeError,
            }
        }
        Expected::ForReason(reason, expected_type, region) => match reason {
            Reason::ExpectCondition => {
                let problem = alloc.concat([
                    alloc.text("This "),
                    alloc.keyword("expect"),
                    alloc.text(" condition needs to be a "),
                    alloc.type_str("Bool"),
                    alloc.text(":"),
                ]);

                report_bad_type(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    alloc.text("Right now it’s"),
                    alloc.concat([
                        alloc.reflow("But I need every "),
                        alloc.keyword("expect"),
                        alloc.reflow(" condition to evaluate to a "),
                        alloc.type_str("Bool"),
                        alloc.reflow("—either "),
                        alloc.tag("Bool.true".into()),
                        alloc.reflow(" or "),
                        alloc.tag("Bool.false".into()),
                        alloc.reflow("."),
                    ]),
                    // Note: Elm has a hint here about truthiness. I think that
                    // makes sense for Elm, since most Elm users will come from
                    // JS, where truthiness is a thing. I don't really know
                    // what the background of Roc programmers will be, and I'd
                    // rather not create a distraction by introducing a term
                    // they don't know. ("Wait, what's truthiness?")
                )
            }
            Reason::IfCondition => {
                let problem = alloc.concat([
                    alloc.text("This "),
                    alloc.keyword("if"),
                    alloc.text(" condition needs to be a "),
                    alloc.type_str("Bool"),
                    alloc.text(":"),
                ]);

                report_bad_type(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    alloc.text("Right now it’s"),
                    alloc.concat([
                        alloc.reflow("But I need every "),
                        alloc.keyword("if"),
                        alloc.reflow(" condition to evaluate to a "),
                        alloc.type_str("Bool"),
                        alloc.reflow("—either "),
                        alloc.tag("Bool.true".into()),
                        alloc.reflow(" or "),
                        alloc.tag("Bool.false".into()),
                        alloc.reflow("."),
                    ]),
                    // Note: Elm has a hint here about truthiness. I think that
                    // makes sense for Elm, since most Elm users will come from
                    // JS, where truthiness is a thing. I don't really know
                    // what the background of Roc programmers will be, and I'd
                    // rather not create a distraction by introducing a term
                    // they don't know. ("Wait, what's truthiness?")
                )
            }
            Reason::WhenGuard => {
                let problem = alloc.concat([
                    alloc.text("This "),
                    alloc.keyword("if"),
                    alloc.text(" guard condition needs to be a "),
                    alloc.type_str("Bool"),
                    alloc.text(":"),
                ]);
                report_bad_type(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    alloc.text("Right now it’s"),
                    alloc.concat([
                        alloc.reflow("But I need every "),
                        alloc.keyword("if"),
                        alloc.reflow(" guard condition to evaluate to a "),
                        alloc.type_str("Bool"),
                        alloc.reflow("—either "),
                        alloc.tag("Bool.true".into()),
                        alloc.reflow(" or "),
                        alloc.tag("Bool.false".into()),
                        alloc.reflow("."),
                    ]),
                )
            }
            Reason::IfBranch {
                index,
                total_branches,
            } => match total_branches {
                2 => report_mismatch(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.concat([
                        alloc.text("This "),
                        alloc.keyword("if"),
                        alloc.text(" has an "),
                        alloc.keyword("else"),
                        alloc.text(" branch with a different type from its "),
                        alloc.keyword("then"),
                        alloc.text(" branch:"),
                    ]),
                    alloc.concat([
                        alloc.text("The "),
                        alloc.keyword("else"),
                        alloc.text(" branch is"),
                    ]),
                    alloc.concat([
                        alloc.text("but the "),
                        alloc.keyword("then"),
                        alloc.text(" branch has the type:"),
                    ]),
                    Some(alloc.concat([
                        alloc.text("All branches in an "),
                        alloc.keyword("if"),
                        alloc.text(" must have the same type!"),
                    ])),
                ),
                _ => report_mismatch(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.concat([
                        alloc.reflow("The "),
                        alloc.string(index.ordinal()),
                        alloc.reflow(" branch of this "),
                        alloc.keyword("if"),
                        alloc.reflow(" does not match all the previous branches:"),
                    ]),
                    alloc.string(format!("The {} branch is", index.ordinal())),
                    alloc.reflow("But all the previous branches have type:"),
                    Some(alloc.concat([
                        alloc.reflow("All branches in an "),
                        alloc.keyword("if"),
                        alloc.reflow(" must have the same type!"),
                    ])),
                ),
            },
            Reason::WhenBranch { index } => report_mismatch(
                alloc,
                lines,
                filename,
                &category,
                found,
                expected_type,
                region,
                Some(expr_region),
                alloc.concat([
                    alloc.reflow("The "),
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("when"),
                    alloc.reflow(" does not match all the previous branches:"),
                ]),
                alloc.concat([
                    alloc.reflow("The "),
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch is"),
                ]),
                alloc.reflow("But all the previous branches have type:"),
                Some(alloc.concat([
                    alloc.reflow("All branches of a "),
                    alloc.keyword("when"),
                    alloc.reflow(" must have the same type!"),
                ])),
            ),
            Reason::ElemInList { index } => {
                let ith = index.ordinal();

                // Don't say "the previous elements all have the type" if
                // there was only 1 previous element!
                let prev_elems_msg = if index.to_zero_based() == 1 {
                    "However, the 1st element has the type:"
                } else {
                    "However, the preceding elements in the list all have the type:"
                };

                report_mismatch(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.reflow("This list contains elements with different types:"),
                    alloc.string(format!("Its {} element is", ith)),
                    alloc.reflow(prev_elems_msg),
                    Some(alloc.reflow("Every element in a list must have the same type!")),
                )
            }
            Reason::RecordUpdateValue(field) => report_mismatch(
                alloc,
                lines,
                filename,
                &category,
                found,
                expected_type,
                region,
                Some(expr_region),
                alloc.concat([
                    alloc.text("I cannot update the "),
                    alloc.record_field(field.to_owned()),
                    alloc.text(" field like this:"),
                ]),
                alloc.concat([
                    alloc.text("You are trying to update "),
                    alloc.record_field(field),
                    alloc.text(" to be"),
                ]),
                alloc.text("But it should be:"),
                Some(alloc.reflow(
                    "Record update syntax does not allow you \
                        to change the type of fields. \
                        You can achieve that with record literal syntax.",
                )),
            ),
            Reason::RecordUpdateKeys(symbol, expected_fields) => {
                match found.clone().unwrap_structural_alias() {
                    ErrorType::Record(actual_fields, ext) => {
                        let expected_set: MutSet<_> = expected_fields.keys().cloned().collect();
                        let actual_set: MutSet<_> = actual_fields.keys().cloned().collect();

                        let mut diff = expected_set.difference(&actual_set);

                        match diff.next().and_then(|k| Some((k, expected_fields.get(k)?))) {
                            None => report_mismatch(
                                alloc,
                                lines,
                                filename,
                                &category,
                                found,
                                expected_type,
                                region,
                                Some(expr_region),
                                alloc.reflow("Something is off with this record update:"),
                                alloc.concat([
                                    alloc.reflow("The"),
                                    alloc.symbol_unqualified(symbol),
                                    alloc.reflow(" record is"),
                                ]),
                                alloc.reflow("But this update needs it to be compatible with:"),
                                None,
                            ),
                            Some((field, field_region)) => report_record_field_typo(
                                alloc,
                                lines,
                                filename,
                                Some(symbol),
                                "",
                                field,
                                ":",
                                *field_region,
                                actual_fields,
                                ext,
                            ),
                        }
                    }
                    _ => report_bad_type(
                        alloc,
                        lines,
                        filename,
                        &category,
                        found,
                        expected_type,
                        region,
                        Some(expr_region),
                        alloc.reflow("This is not a record, so it has no fields to update!"),
                        alloc.reflow("It is"),
                        alloc.reflow("But I need a record!"),
                    ),
                }
            }
            Reason::FnCall { name, arity } => match count_arguments(&found) {
                0 => {
                    let this_value = match name {
                        None => alloc.text("This value"),
                        Some(symbol) => alloc.concat([
                            alloc.text("The "),
                            alloc.symbol_unqualified(symbol),
                            alloc.text(" value"),
                        ]),
                    };

                    let lines = vec![
                        alloc.concat([
                            this_value,
                            alloc.string(format!(
                                " is not a function, but it was given {}:",
                                if arity == 1 {
                                    "1 argument".into()
                                } else {
                                    format!("{} arguments", arity)
                                }
                            )),
                        ]),
                        alloc.region(lines.convert_region(expr_region)),
                        alloc.reflow("Are there any missing commas? Or missing parentheses?"),
                    ];

                    Report {
                        filename,
                        title: "TOO MANY ARGS".to_string(),
                        doc: alloc.stack(lines),
                        severity: Severity::RuntimeError,
                    }
                }
                n => {
                    let this_function = match name {
                        None => alloc.text("This function"),
                        Some(symbol) => alloc.concat([
                            alloc.text("The "),
                            alloc.symbol_unqualified(symbol),
                            alloc.text(" function"),
                        ]),
                    };

                    if n < arity as usize {
                        let lines = vec![
                            alloc.concat([
                                this_function,
                                alloc.string(format!(
                                    " expects {}, but it got {} instead:",
                                    if n == 1 {
                                        "1 argument".into()
                                    } else {
                                        format!("{} arguments", n)
                                    },
                                    arity
                                )),
                            ]),
                            alloc.region(lines.convert_region(expr_region)),
                            alloc.reflow("Are there any missing commas? Or missing parentheses?"),
                        ];

                        Report {
                            filename,
                            title: "TOO MANY ARGS".to_string(),
                            doc: alloc.stack(lines),
                            severity: Severity::RuntimeError,
                        }
                    } else {
                        let lines = vec![
                            alloc.concat([
                                this_function,
                                alloc.string(format!(
                                    " expects {}, but it got only {}:",
                                    if n == 1 {
                                        "1 argument".into()
                                    } else {
                                        format!("{} arguments", n)
                                    },
                                    arity
                                )),
                            ]),
                            alloc.region(lines.convert_region(expr_region)),
                            alloc.reflow(
                                "Roc does not allow functions to be partially applied. \
                                Use a closure to make partial application explicit.",
                            ),
                        ];

                        Report {
                            filename,
                            title: "TOO FEW ARGS".to_string(),
                            doc: alloc.stack(lines),
                            severity: Severity::RuntimeError,
                        }
                    }
                }
            },
            Reason::FnArg { name, arg_index } => {
                let ith = arg_index.ordinal();

                let this_function = match name {
                    None => alloc.text("this function"),
                    Some(symbol) => alloc.symbol_unqualified(symbol),
                };

                report_mismatch(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.concat([
                        alloc.string(format!("This {ith} argument to ")),
                        this_function.clone(),
                        alloc.text(" has an unexpected type:"),
                    ]),
                    alloc.text("The argument is"),
                    alloc.concat([
                        alloc.text("But "),
                        this_function,
                        alloc.string(format!(" needs its {ith} argument to be:")),
                    ]),
                    None,
                )
            }

            Reason::NumericLiteralSuffix => report_mismatch(
                alloc,
                lines,
                filename,
                &category,
                found,
                expected_type,
                region,
                Some(expr_region),
                alloc.text("This numeric literal is being used improperly:"),
                alloc.text("Here the value is used as a:"),
                alloc.text("But its suffix says it's a:"),
                None,
            ),

            Reason::InvalidAbilityMemberSpecialization {
                member_name,
                def_region: _,
                unimplemented_abilities,
            } => {
                let problem = alloc.concat([
                    alloc.reflow("Something is off with this specialization of "),
                    alloc.symbol_unqualified(member_name),
                    alloc.reflow(":"),
                ]);
                let this_is = alloc.reflow("This value is");
                let instead_of = alloc.concat([
                    alloc.reflow("But the type annotation on "),
                    alloc.symbol_unqualified(member_name),
                    alloc.reflow(" says it must match:"),
                ]);

                let hint = if unimplemented_abilities.is_empty() {
                    None
                } else {
                    let mut stack = Vec::with_capacity(unimplemented_abilities.len());
                    for (err_type, ability) in unimplemented_abilities.into_iter() {
                        stack.push(does_not_implement(alloc, err_type, ability));
                    }

                    let hint = alloc.stack([
                        alloc.concat([
                            alloc.note(""),
                            alloc.reflow("Some types in this specialization don't implement the abilities they are expected to. I found the following missing implementations:"),
                        ]),
                        alloc.type_block(alloc.stack(stack)),
                    ]);

                    Some(hint)
                };

                report_mismatch(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    this_is,
                    instead_of,
                    hint,
                )
            }

            Reason::GeneralizedAbilityMemberSpecialization {
                member_name,
                def_region: _,
            } => {
                let problem = alloc.concat([
                    alloc.reflow("This specialization of "),
                    alloc.symbol_unqualified(member_name),
                    alloc.reflow(" is overly general:"),
                ]);
                let this_is = alloc.reflow("This value is");
                let instead_of = alloc.concat([
                    alloc.reflow("But the type annotation on "),
                    alloc.symbol_unqualified(member_name),
                    alloc.reflow(" says it must match:"),
                ]);

                let note = alloc.stack([
                    alloc.concat([
                        alloc.note(""),
                        alloc.reflow("The specialized type is too general, and does not provide a concrete type where a type variable is bound to an ability."),
                    ]),
                    alloc.reflow("Specializations can only be made for concrete types. If you have a generic implementation for this value, perhaps you don't need an ability?"),
                ]);

                report_mismatch(
                    alloc,
                    lines,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    this_is,
                    instead_of,
                    Some(note),
                )
            }

            Reason::WhenBranches => {
                let snippet = alloc.region_with_subregion(
                    lines.convert_region(region),
                    lines.convert_region(expr_region),
                );

                let this_is = alloc.concat([
                    alloc.reflow("The "),
                    alloc.keyword("when"),
                    alloc.reflow(" condition is"),
                ]);

                let wanted = alloc.reflow("But the branch patterns have type:");
                let details = Some(alloc.concat([
                    alloc.reflow("The branches must be cases of the "),
                    alloc.keyword("when"),
                    alloc.reflow(" condition's type!"),
                ]));

                let lines = [
                    alloc.concat([
                        alloc.reflow("The branches of this "),
                        alloc.keyword("when"),
                        alloc.reflow(" expression don't match the condition:"),
                    ]),
                    snippet,
                    type_comparison(
                        alloc,
                        found,
                        expected_type,
                        ExpectationContext::WhenCondition,
                        add_category(alloc, this_is, &category),
                        wanted,
                        details,
                    ),
                ];

                Report {
                    title: "TYPE MISMATCH".to_string(),
                    filename,
                    doc: alloc.stack(lines),
                    severity: Severity::RuntimeError,
                }
            }

            Reason::TypedArg { name, arg_index } => {
                let name = match name {
                    Some(n) => alloc.symbol_unqualified(n),
                    None => alloc.text(" this definition "),
                };
                let doc = alloc.stack([
                    alloc
                        .text("The ")
                        .append(alloc.text(arg_index.ordinal()))
                        .append(alloc.text(" argument to "))
                        .append(name.clone())
                        .append(alloc.text(" is weird:")),
                    alloc.region(lines.convert_region(region)),
                    pattern_type_comparison(
                        alloc,
                        expected_type,
                        found,
                        add_category(alloc, alloc.text("The argument matches"), &category),
                        alloc.concat([
                            alloc.text("But the annotation on "),
                            name,
                            alloc.text(" says the "),
                            alloc.text(arg_index.ordinal()),
                            alloc.text(" argument should be:"),
                        ]),
                        vec![],
                    ),
                ]);

                Report {
                    filename,
                    title: "TYPE MISMATCH".to_string(),
                    doc,
                    severity: Severity::RuntimeError,
                }
            }

            Reason::CrashArg => {
                let this_is = alloc.reflow("The value is");

                let wanted = alloc.concat([
                    alloc.reflow("But I can only "),
                    alloc.keyword("crash"),
                    alloc.reflow(" with messages of type"),
                ]);

                let details = None;

                let lines = [
                    alloc
                        .reflow("This value passed to ")
                        .append(alloc.keyword("crash"))
                        .append(alloc.reflow(" is not a string:")),
                    alloc.region(lines.convert_region(region)),
                    type_comparison(
                        alloc,
                        found,
                        expected_type,
                        ExpectationContext::WhenCondition,
                        add_category(alloc, this_is, &category),
                        wanted,
                        details,
                    ),
                ];

                Report {
                    filename,
                    title: "TYPE MISMATCH".to_string(),
                    doc: alloc.stack(lines),
                    severity: Severity::RuntimeError,
                }
            }

            Reason::LowLevelOpArg { op, arg_index } => {
                panic!(
                    "Compiler bug: argument #{} to low-level operation {:?} was the wrong type!",
                    arg_index.ordinal(),
                    op
                );
            }

            Reason::ForeignCallArg {
                foreign_symbol,
                arg_index,
            } => {
                panic!(
                    "Compiler bug: argument #{} to foreign symbol {:?} was the wrong type!",
                    arg_index.ordinal(),
                    foreign_symbol
                );
            }

            Reason::FloatLiteral | Reason::IntLiteral | Reason::NumLiteral => {
                unreachable!("I don't think these can be reached")
            }

            Reason::StrInterpolation => {
                unimplemented!("string interpolation is not implemented yet")
            }

            Reason::RecordDefaultField(_) => {
                unimplemented!("record default field is not implemented yet")
            }
        },
    }
}

fn does_not_implement<'a>(
    alloc: &'a RocDocAllocator<'a>,
    err_type: ErrorType,
    ability: Symbol,
) -> RocDocBuilder<'a> {
    alloc.concat([
        to_doc(alloc, Parens::Unnecessary, err_type).0,
        alloc.reflow(" does not implement "),
        alloc.symbol_unqualified(ability),
    ])
}

fn count_arguments(tipe: &ErrorType) -> usize {
    use ErrorType::*;

    match tipe {
        Function(args, _, _) => args.len(),
        Alias(_, _, actual, _) => count_arguments(actual),
        _ => 0,
    }
}

/// The context a type expectation is derived from.
#[derive(Clone)]
enum ExpectationContext<'a> {
    /// An expected type was discovered from a type annotation. Corresponds to
    /// [`Expected::FromAnnotation`](Expected::FromAnnotation).
    Annotation {
        on: RocDocBuilder<'a>,
    },
    WhenCondition,
    /// When we don't know the context, or it's not relevant.
    Arbitrary,
}

impl<'a> std::fmt::Debug for ExpectationContext<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectationContext::Annotation { .. } => f.write_str("Annotation"),
            ExpectationContext::WhenCondition => f.write_str("WhenCondition"),
            ExpectationContext::Arbitrary => f.write_str("Arbitrary"),
        }
    }
}

fn type_comparison<'b>(
    alloc: &'b RocDocAllocator<'b>,
    actual: ErrorType,
    expected: ErrorType,
    expectation_context: ExpectationContext<'b>,
    i_am_seeing: RocDocBuilder<'b>,
    instead_of: RocDocBuilder<'b>,
    context_hints: Option<RocDocBuilder<'b>>,
) -> RocDocBuilder<'b> {
    let comparison = to_comparison(alloc, actual, expected);

    let mut lines = vec![
        i_am_seeing,
        comparison.actual,
        instead_of,
        comparison.expected,
    ];

    if context_hints.is_some() {
        lines.push(alloc.concat(context_hints));
    }

    lines.extend(problems_to_tip(
        alloc,
        comparison.problems,
        expectation_context,
    ));

    alloc.stack(lines)
}

fn lone_type<'b>(
    alloc: &'b RocDocAllocator<'b>,
    actual: ErrorType,
    expected: ErrorType,
    expectation_context: ExpectationContext<'b>,
    i_am_seeing: RocDocBuilder<'b>,
    further_details: RocDocBuilder<'b>,
) -> RocDocBuilder<'b> {
    let comparison = to_comparison(alloc, actual, expected);

    let mut lines = vec![i_am_seeing, comparison.actual, further_details];

    lines.extend(problems_to_tip(
        alloc,
        comparison.problems,
        expectation_context,
    ));

    alloc.stack(lines)
}

/// Formats an item in a Roc program to a tuple (summary, has_type_colon), where
/// concatenation of the tuple items introduces the item and leads up to its type.
fn format_category<'b>(
    alloc: &'b RocDocAllocator<'b>,
    this_is: RocDocBuilder<'b>,
    category: &Category,
    capitalize_start: bool,
) -> (RocDocBuilder<'b>, RocDocBuilder<'b>) {
    use Category::*;

    let t = if capitalize_start { "T" } else { "t" };

    match category {
        Lookup(name) => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.symbol_foreign_qualified(*name),
                alloc.text(" value"),
            ]),
            alloc.text(" is a:"),
        ),

        If => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.keyword("if"),
                alloc.text(" expression"),
            ]),
            alloc.text(" produces:"),
        ),
        When => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.keyword("when"),
                alloc.text(" expression"),
            ]),
            alloc.text(" produces:"),
        ),
        List => (
            alloc.concat([this_is, alloc.text(" a list")]),
            alloc.text(" of type:"),
        ),
        Num => (
            alloc.concat([this_is, alloc.text(" a number")]),
            alloc.text(" of type:"),
        ),
        Int => (
            alloc.concat([this_is, alloc.text(" an integer")]),
            alloc.text(" of type:"),
        ),
        Frac => (
            alloc.concat([this_is, alloc.text(" a fraction")]),
            alloc.text(" of type:"),
        ),
        Str => (
            alloc.concat([this_is, alloc.text(" a string")]),
            alloc.text(" of type:"),
        ),
        StrInterpolation => (
            alloc.concat([this_is, alloc.text(" a value in a string interpolation,")]),
            alloc.text(" which was of type:"),
        ),
        Character => (
            alloc.concat([this_is, alloc.text(" a Unicode scalar value")]),
            alloc.text(" of type:"),
        ),
        Lambda => (
            alloc.concat([this_is, alloc.text(" an anonymous function")]),
            alloc.text(" of type:"),
        ),
        ClosureSize => (
            alloc.concat([this_is, alloc.text(" the closure size of a function")]),
            alloc.text(" of type:"),
        ),

        OpaqueWrap(opaque) => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.opaque_name(*opaque),
                alloc.text(" opaque wrapping"),
            ]),
            alloc.text(" has the type:"),
        ),

        OpaqueArg => (
            alloc.concat([alloc.text(format!("{}his argument to an opaque type", t))]),
            alloc.text(" has type:"),
        ),

        TagApply {
            tag_name: TagName(name),
            args_count: 0,
        } => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.tag(name.to_owned()),
                alloc.text(" tag"),
            ]),
            alloc.text(" has the type:"),
        ),

        TagApply {
            tag_name: TagName(name),
            args_count: _,
        } => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.tag(name.to_owned()),
                alloc.text(" tag application"),
            ]),
            alloc.text(" has the type:"),
        ),

        Record => (
            alloc.concat([this_is, alloc.text(" a record")]),
            alloc.text(" of type:"),
        ),

        Accessor(field) => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.record_field(field.to_owned()),
                alloc.text(" value"),
            ]),
            alloc.text(" is a:"),
        ),
        Access(field) => (
            alloc.concat([
                alloc.text(format!("{}he value at ", t)),
                alloc.record_field(field.to_owned()),
            ]),
            alloc.text(" is a:"),
        ),
        CallResult(
            Some(_),
            CalledVia::BinOp(
                BinOp::Equals
                | BinOp::NotEquals
                | BinOp::LessThan
                | BinOp::GreaterThan
                | BinOp::LessThanOrEq
                | BinOp::GreaterThanOrEq,
            ),
        ) => (
            alloc.text(format!("{}his comparison", t)),
            alloc.text(" produces:"),
        ),
        CallResult(Some(_), CalledVia::StringInterpolation) => (
            alloc.concat([this_is, alloc.text(" a string")]),
            alloc.text(" of type:"),
        ),
        CallResult(Some(symbol), _) => (
            alloc.concat([
                alloc.text(format!("{}his ", t)),
                alloc.symbol_foreign_qualified(*symbol),
                alloc.text(" call"),
            ]),
            alloc.text(" produces:"),
        ),
        CallResult(None, _) => (this_is, alloc.text(":")),
        LowLevelOpResult(op) => {
            panic!(
                "Compiler bug: invalid return type from low-level op {:?}",
                op
            );
        }
        ForeignCall => {
            panic!("Compiler bug: invalid return type from foreign call",);
        }

        Uniqueness => (
            alloc.concat([this_is, alloc.text(" an uniqueness attribute")]),
            alloc.text(" of type:"),
        ),
        Crash => {
            internal_error!("calls to crash should be unconditionally admitted in any context, unexpected reachability!");
        }

        Storage(..) | Unknown => (
            alloc.concat([this_is, alloc.text(" a value")]),
            alloc.text(" of type:"),
        ),
        DefaultValue(_) => (
            alloc.concat([this_is, alloc.text(" a default field")]),
            alloc.text(" of type:"),
        ),
        AbilityMemberSpecialization(_ability_member) => (
            alloc.concat([this_is, alloc.text(" a declared specialization")]),
            alloc.text(" of type:"),
        ),
        Expect => (
            alloc.concat([this_is, alloc.text(" an expectation")]),
            alloc.text(" of type:"),
        ),
        Dbg => (
            alloc.concat([this_is, alloc.text(" a dbg statement")]),
            alloc.text(" of type:"),
        ),
    }
}

fn add_category<'b>(
    alloc: &'b RocDocAllocator<'b>,
    this_is: RocDocBuilder<'b>,
    category: &Category,
) -> RocDocBuilder<'b> {
    let (summary, suffix) = format_category(alloc, this_is, category, true);
    alloc.concat([summary, suffix])
}

fn to_pattern_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    expr_region: roc_region::all::Region,
    category: PatternCategory,
    found: ErrorType,
    expected: PExpected<ErrorType>,
) -> Report<'b> {
    use roc_types::types::PReason;

    match expected {
        PExpected::NoExpectation(expected_type) => {
            let doc = alloc.stack([
                alloc.text("This pattern is being used in an unexpected way:"),
                alloc.region(lines.convert_region(expr_region)),
                pattern_type_comparison(
                    alloc,
                    found,
                    expected_type,
                    add_pattern_category(alloc, alloc.text("It is"), &category),
                    alloc.text("But it needs to match:"),
                    vec![],
                ),
            ]);

            Report {
                filename,
                title: "TYPE MISMATCH".to_string(),
                doc,
                severity: Severity::RuntimeError,
            }
        }

        PExpected::ForReason(reason, expected_type, region) => match reason {
            PReason::OptionalField => unreachable!("this will never be reached I think"),
            PReason::TypedArg { opt_name, index } => {
                let name = match opt_name {
                    Some(n) => alloc.symbol_unqualified(n),
                    None => alloc.text(" this definition "),
                };
                let doc = alloc.stack([
                    alloc
                        .text("The ")
                        .append(alloc.text(index.ordinal()))
                        .append(alloc.text(" argument to "))
                        .append(name.clone())
                        .append(alloc.text(" is weird:")),
                    alloc.region(lines.convert_region(region)),
                    pattern_type_comparison(
                        alloc,
                        found,
                        expected_type,
                        add_pattern_category(
                            alloc,
                            alloc.text("The argument is a pattern that matches"),
                            &category,
                        ),
                        alloc.concat([
                            alloc.text("But the annotation on "),
                            name,
                            alloc.text(" says the "),
                            alloc.text(index.ordinal()),
                            alloc.text(" argument should be:"),
                        ]),
                        vec![],
                    ),
                ]);

                Report {
                    filename,
                    title: "TYPE MISMATCH".to_string(),
                    doc,
                    severity: Severity::RuntimeError,
                }
            }
            PReason::WhenMatch { index, sub_pattern } => {
                let doc = match (index, sub_pattern) {
                    (HumanIndex::FIRST, HumanIndex::FIRST) => alloc.stack([
                        alloc
                            .text("The 1st pattern in this ")
                            .append(alloc.keyword("when"))
                            .append(alloc.text(" is causing a mismatch:")),
                        alloc.region_with_subregion(
                            lines.convert_region(region),
                            lines.convert_region(expr_region),
                        ),
                        pattern_type_comparison(
                            alloc,
                            found,
                            expected_type,
                            add_pattern_category(
                                alloc,
                                alloc.text("The first pattern is trying to match"),
                                &category,
                            ),
                            alloc.concat([
                                alloc.text("But the expression between "),
                                alloc.keyword("when"),
                                alloc.text(" and "),
                                alloc.keyword("is"),
                                alloc.text(" has the type:"),
                            ]),
                            vec![],
                        ),
                    ]),
                    (index, sub_pattern) => {
                        let (first, index) = match sub_pattern {
                            HumanIndex::FIRST => {
                                let doc = alloc
                                    .string(format!("The {} pattern in this ", index.ordinal()))
                                    .append(alloc.keyword("when"))
                                    .append(alloc.text(" does not match the previous ones:"));
                                (doc, index)
                            }

                            _ => {
                                let doc = alloc.string(format!(
                                    "The {} pattern in this branch does not match the previous ones:",
                                    sub_pattern.ordinal()
                                ));
                                (doc, sub_pattern)
                            }
                        };

                        alloc.stack([
                            first,
                            alloc.region_with_subregion(
                                lines.convert_region(region),
                                lines.convert_region(expr_region),
                            ),
                            pattern_type_comparison(
                                alloc,
                                found,
                                expected_type,
                                add_pattern_category(
                                    alloc,
                                    alloc.string(format!(
                                        "The {} pattern is trying to match",
                                        index.ordinal()
                                    )),
                                    &category,
                                ),
                                alloc.text("But all the previous branches match:"),
                                vec![],
                            ),
                        ])
                    }
                };
                Report {
                    filename,
                    title: "TYPE MISMATCH".to_string(),
                    doc,
                    severity: Severity::RuntimeError,
                }
            }
            PReason::ListElem => {
                let doc = alloc.stack([
                    alloc.concat([alloc.reflow("This list element doesn't match the types of other elements in the pattern:")]),
                    alloc.region(lines.convert_region(region)),
                    pattern_type_comparison(
                        alloc,
                        found,
                        expected_type,
                        add_pattern_category(
                            alloc,
                            alloc.text("It matches"),
                            &category,
                        ),
                        alloc.concat([
                            alloc.text("But the other elements in this list pattern match")
                        ]),
                        vec![],
                    ),
                ]);

                Report {
                    filename,
                    title: "TYPE MISMATCH".to_string(),
                    doc,
                    severity: Severity::RuntimeError,
                }
            }
            PReason::TagArg { .. } | PReason::PatternGuard => {
                internal_error!("We didn't think this could trigger. Please tell us about it on Zulip if it does!")
            }
        },
    }
}

fn pattern_type_comparison<'b>(
    alloc: &'b RocDocAllocator<'b>,
    actual: ErrorType,
    expected: ErrorType,
    i_am_seeing: RocDocBuilder<'b>,
    instead_of: RocDocBuilder<'b>,
    reason_hints: Vec<RocDocBuilder<'b>>,
) -> RocDocBuilder<'b> {
    let comparison = to_comparison(alloc, actual, expected);

    let mut lines = vec![
        i_am_seeing,
        comparison.actual,
        instead_of,
        comparison.expected,
    ];

    lines.extend(problems_to_tip(
        alloc,
        comparison.problems,
        ExpectationContext::Arbitrary,
    ));
    lines.extend(reason_hints);

    alloc.stack(lines)
}

fn add_pattern_category<'b>(
    alloc: &'b RocDocAllocator<'b>,
    i_am_trying_to_match: RocDocBuilder<'b>,
    category: &PatternCategory,
) -> RocDocBuilder<'b> {
    use PatternCategory::*;

    let rest = match category {
        Record => alloc.reflow(" record values of type:"),
        EmptyRecord => alloc.reflow(" an empty record:"),
        PatternGuard => alloc.reflow(" a pattern guard of type:"),
        PatternDefault => alloc.reflow(" an optional field of type:"),
        Set => alloc.reflow(" sets of type:"),
        Map => alloc.reflow(" maps of type:"),
        List => alloc.reflow(" lists of type:"),
        Ctor(tag_name) => alloc.concat([
            alloc.reflow(" a "),
            alloc.tag_name(tag_name.clone()),
            alloc.reflow(" tag of type:"),
        ]),
        Opaque(opaque) => alloc.concat([
            alloc.opaque_name(*opaque),
            alloc.reflow(" unwrappings of type:"),
        ]),
        Str => alloc.reflow(" strings:"),
        Num => alloc.reflow(" numbers:"),
        Int => alloc.reflow(" integers:"),
        Float => alloc.reflow(" floats:"),
        Character => alloc.reflow(" characters:"),
    };

    alloc.concat([i_am_trying_to_match, rest])
}

fn to_circular_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    region: roc_region::all::Region,
    symbol: Symbol,
    overall_type: ErrorType,
) -> Report<'b> {
    Report {
        title: "CIRCULAR TYPE".to_string(),
        filename,
        doc: {
            alloc.stack([
                alloc
                    .reflow("I'm inferring a weird self-referential type for ")
                    .append(alloc.symbol_unqualified(symbol))
                    .append(alloc.text(":")),
                alloc.region(lines.convert_region(region)),
                alloc.stack([
                    alloc.reflow(
                        "Here is my best effort at writing down the type. \
                        You will see ∞ for parts of the type that repeat \
                        something already printed out infinitely.",
                    ),
                    alloc.type_block(to_doc(alloc, Parens::Unnecessary, overall_type).0),
                ]),
            ])
        },
        severity: Severity::RuntimeError,
    }
}

#[derive(Debug, Clone)]
pub enum Problem {
    IntFloat,
    ArityMismatch(usize, usize),
    FieldTypo(Lowercase, Vec<Lowercase>),
    FieldsMissing(Vec<Lowercase>),
    TagTypo(TagName, Vec<TagName>),
    TagsMissing(Vec<TagName>),
    BadRigidVar(Lowercase, ErrorType, Option<AbilitySet>),
    OptionalRequiredMismatch(Lowercase),
    OpaqueComparedToNonOpaque,
    BoolVsBoolTag(TagName),
}

fn problems_to_tip<'b>(
    alloc: &'b RocDocAllocator<'b>,
    mut problems: Vec<Problem>,
    expectation_context: ExpectationContext<'b>,
) -> Option<RocDocBuilder<'b>> {
    if problems.is_empty() {
        None
    } else {
        let problem = problems.remove(problems.len() - 1);
        Some(type_problem_to_pretty(alloc, problem, expectation_context))
    }
}

pub mod suggest {
    use roc_module::ident::Lowercase;

    pub trait ToStr {
        fn to_str(&self) -> &str;
    }

    impl ToStr for Lowercase {
        fn to_str(&self) -> &str {
            self.as_str()
        }
    }

    impl ToStr for &Lowercase {
        fn to_str(&self) -> &str {
            self.as_str()
        }
    }

    impl ToStr for &str {
        fn to_str(&self) -> &str {
            self
        }
    }

    impl ToStr for super::IdentStr {
        fn to_str(&self) -> &str {
            self.as_str()
        }
    }

    impl<A, B> ToStr for (A, B)
    where
        A: ToStr,
    {
        fn to_str(&self) -> &str {
            self.0.to_str()
        }
    }

    pub fn sort<T>(typo: &str, mut options: Vec<T>) -> Vec<T>
    where
        T: ToStr,
    {
        options.sort_by(|a, b| {
            let l = distance::damerau_levenshtein(typo, a.to_str());
            let r = distance::damerau_levenshtein(typo, b.to_str());

            l.cmp(&r)
        });

        options
    }
}

pub struct Comparison<'b> {
    actual: RocDocBuilder<'b>,
    expected: RocDocBuilder<'b>,
    problems: Vec<Problem>,
}

fn to_comparison<'b>(
    alloc: &'b RocDocAllocator<'b>,
    actual: ErrorType,
    expected: ErrorType,
) -> Comparison<'b> {
    let diff = to_diff(alloc, Parens::Unnecessary, actual, expected);
    let actual = type_with_able_vars(alloc, diff.left, diff.left_able);
    let expected = type_with_able_vars(alloc, diff.right, diff.right_able);

    Comparison {
        actual: alloc.type_block(actual),
        expected: alloc.type_block(expected),
        problems: match diff.status {
            Status::Similar => vec![],
            Status::Different(problems) => problems,
        },
    }
}

fn diff_is_wildcard_comparison<'b>(
    alloc: &'b RocDocAllocator<'b>,
    actual: ErrorType,
    expected: ErrorType,
) -> bool {
    let Comparison { problems, .. } = to_comparison(alloc, actual, expected);
    match problems.last() {
        Some(Problem::BadRigidVar(v1, ErrorType::RigidVar(v2), None)) => {
            v1.as_str() == WILDCARD && v2.as_str() == WILDCARD
        }
        _ => false,
    }
}

#[derive(Debug)]
pub enum Status {
    Similar,                 // the structure is the same or e.g. record fields are different
    Different(Vec<Problem>), // e.g. found Bool, expected Int
}

impl Status {
    pub fn merge(&mut self, other: Self) {
        use Status::*;
        match self {
            Similar => {
                *self = other;
            }
            Different(problems1) => match other {
                Similar => { /* nothing */ }
                Different(problems2) => {
                    // TODO pick a data structure that makes this merge cheaper
                    let mut problems = Vec::with_capacity(problems1.len() + problems2.len());
                    problems.extend(problems1.iter().cloned());
                    problems.extend(problems2);
                    *self = Different(problems);
                }
            },
        }
    }
}

pub struct Diff<T> {
    left: T,
    right: T,
    status: Status,
    // idea: lift "able" type variables so they are shown at the top of a type.
    left_able: AbleVariables,
    right_able: AbleVariables,
}

fn tag_ext_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pol: Polarity,
    gen_usages: &VecMap<Lowercase, usize>,
    ext: TypeExt,
) -> Option<RocDocBuilder<'b>> {
    use TypeExt::*;

    match ext {
        Closed => None,
        FlexOpen(lowercase) if is_generated_name(&lowercase) => {
            let &usages = gen_usages.get(&lowercase).unwrap_or(&1);

            if usages > 1 {
                Some(alloc.type_variable(display_generated_name(&lowercase).into()))
            } else {
                match pol {
                    Polarity::Neg => Some(alloc.type_variable(WILDCARD.into())),
                    Polarity::Pos => {
                        // Wildcard in output position is irrelevant and is elided.
                        None
                    }
                }
            }
        }
        FlexOpen(lowercase) | RigidOpen(lowercase) => Some(alloc.type_variable(lowercase)),
    }
}

fn record_ext_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    ext: TypeExt,
) -> Option<RocDocBuilder<'b>> {
    use TypeExt::*;

    match ext {
        Closed => None,
        FlexOpen(lowercase) if is_generated_name(&lowercase) => {
            Some(alloc.type_variable(display_generated_name(&lowercase).into()))
        }
        FlexOpen(lowercase) | RigidOpen(lowercase) => Some(alloc.type_variable(lowercase)),
    }
}

type AbleVariables = Vec<(Lowercase, AbilitySet)>;

#[derive(Default)]
struct Context {
    able_variables: AbleVariables,
}

pub fn to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    tipe: ErrorType,
) -> (RocDocBuilder<'b>, AbleVariables) {
    let mut ctx = Context::default();

    let mut generated_name_usages = VecMap::default();
    count_generated_name_usages(&mut generated_name_usages, [&tipe]);
    let doc = to_doc_help(&mut ctx, &generated_name_usages, alloc, parens, tipe);

    (doc, ctx.able_variables)
}

fn is_generated_name(name: &Lowercase) -> bool {
    name.as_str().starts_with('#')
}

fn display_generated_name(name: &Lowercase) -> &str {
    &name.as_str()[1..]
}

fn to_doc_help<'b>(
    ctx: &mut Context,
    gen_usages: &VecMap<Lowercase, usize>,
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    tipe: ErrorType,
) -> RocDocBuilder<'b> {
    use ErrorType::*;

    match tipe {
        Function(args, _, ret) => report_text::function(
            alloc,
            parens,
            args.into_iter()
                .map(|arg| to_doc_help(ctx, gen_usages, alloc, Parens::InFn, arg))
                .collect(),
            to_doc_help(ctx, gen_usages, alloc, Parens::InFn, *ret),
        ),
        Infinite => alloc.text("∞"),
        Error => alloc.text("?"),

        FlexVar(lowercase) if is_generated_name(&lowercase) => {
            let &usages = gen_usages
                .get(&lowercase)
                .expect("flex var appears, but not captured here");

            if usages > 1 {
                alloc.type_variable(display_generated_name(&lowercase).into())
            } else {
                alloc.type_variable(WILDCARD.into())
            }
        }
        FlexVar(lowercase) | RigidVar(lowercase) => alloc.type_variable(lowercase),
        FlexAbleVar(lowercase, ability) | RigidAbleVar(lowercase, ability) => {
            ctx.able_variables.push((lowercase.clone(), ability));
            alloc.type_variable(lowercase)
        }

        Type(symbol, args) => report_text::apply(
            alloc,
            parens,
            alloc.symbol_foreign_qualified(symbol),
            args.into_iter()
                .map(|arg| to_doc_help(ctx, gen_usages, alloc, Parens::InTypeParam, arg))
                .collect(),
        ),

        Alias(Symbol::NUM_NUM, mut args, _, _) => {
            debug_assert!(args.len() == 1);
            let type_arg = args.remove(0);

            let (symbol, args) = match type_arg {
                Alias(Symbol::NUM_FLOATINGPOINT, inner_args, _, _) => {
                    (Symbol::NUM_FRAC, inner_args)
                }
                Alias(Symbol::NUM_INTEGER, inner_args, _, _) => (Symbol::NUM_INT, inner_args),
                _ => (Symbol::NUM_NUM, vec![type_arg]),
            };

            report_text::apply(
                alloc,
                parens,
                alloc.symbol_foreign_qualified(symbol),
                args.into_iter()
                    .map(|arg| to_doc_help(ctx, gen_usages, alloc, Parens::InTypeParam, arg))
                    .collect(),
            )
        }

        Alias(symbol, args, _, _) => report_text::apply(
            alloc,
            parens,
            alloc.symbol_foreign_qualified(symbol),
            args.into_iter()
                .map(|arg| to_doc_help(ctx, gen_usages, alloc, Parens::InTypeParam, arg))
                .collect(),
        ),

        Record(fields_map, ext) => {
            let mut fields = fields_map.into_iter().collect::<Vec<_>>();
            fields.sort_by(|(a, _), (b, _)| a.cmp(b));

            report_text::record(
                alloc,
                fields
                    .into_iter()
                    .map(|(k, value)| {
                        (
                            alloc.string(k.as_str().to_string()),
                            match value {
                                RecordField::Optional(v) => RecordField::Optional(to_doc_help(
                                    ctx,
                                    gen_usages,
                                    alloc,
                                    Parens::Unnecessary,
                                    v,
                                )),
                                RecordField::RigidOptional(v) => RecordField::RigidOptional(
                                    to_doc_help(ctx, gen_usages, alloc, Parens::Unnecessary, v),
                                ),
                                RecordField::Required(v) => RecordField::Required(to_doc_help(
                                    ctx,
                                    gen_usages,
                                    alloc,
                                    Parens::Unnecessary,
                                    v,
                                )),
                                RecordField::RigidRequired(v) => RecordField::RigidRequired(
                                    to_doc_help(ctx, gen_usages, alloc, Parens::Unnecessary, v),
                                ),
                                RecordField::Demanded(v) => RecordField::Demanded(to_doc_help(
                                    ctx,
                                    gen_usages,
                                    alloc,
                                    Parens::Unnecessary,
                                    v,
                                )),
                            },
                        )
                    })
                    .collect(),
                record_ext_to_doc(alloc, ext),
            )
        }

        TagUnion(tags_map, ext, pol) => {
            let mut tags = tags_map
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.into_iter()
                            .map(|arg| {
                                to_doc_help(ctx, gen_usages, alloc, Parens::InTypeParam, arg)
                            })
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>();
            tags.sort_by(|(a, _), (b, _)| a.cmp(b));

            report_text::tag_union(
                alloc,
                tags.into_iter()
                    .map(|(k, v)| (alloc.tag_name(k), v))
                    .collect(),
                tag_ext_to_doc(alloc, pol, gen_usages, ext),
            )
        }

        RecursiveTagUnion(rec_var, tags_map, ext, pol) => {
            let mut tags = tags_map
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.into_iter()
                            .map(|arg| {
                                to_doc_help(ctx, gen_usages, alloc, Parens::InTypeParam, arg)
                            })
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>();
            tags.sort_by(|(a, _), (b, _)| a.cmp(b));

            report_text::recursive_tag_union(
                alloc,
                to_doc_help(ctx, gen_usages, alloc, Parens::Unnecessary, *rec_var),
                tags.into_iter()
                    .map(|(k, v)| (alloc.tag_name(k), v))
                    .collect(),
                tag_ext_to_doc(alloc, pol, gen_usages, ext),
            )
        }

        Range(range_types) => {
            let range_types = range_types
                .into_iter()
                .map(|arg| to_doc_help(ctx, gen_usages, alloc, Parens::Unnecessary, arg))
                .collect();
            report_text::range(alloc, range_types)
        }
    }
}

fn count_generated_name_usages<'a>(
    usages: &mut VecMap<Lowercase, usize>,
    types: impl IntoIterator<Item = &'a ErrorType>,
) {
    // Stack consists of (type, only_unseen) where if `only_unseen`, then the count should only be
    // incremented if the variable has not already been seen. This is to deal with counting phantom
    // variables in type aliases, while not double-counting alias type arguments that also appear
    // in the real type.
    let mut stack = types.into_iter().map(|t| (t, false)).collect::<Vec<_>>();

    let mut ext_stack = vec![];

    use ErrorType::*;
    while let Some((tipe, only_unseen)) = stack.pop() {
        match tipe {
            FlexVar(name) | FlexAbleVar(name, _) => {
                if is_generated_name(name) {
                    let count = usages.get_or_insert(name.clone(), || 0);
                    if !only_unseen || *count == 0 {
                        *count += 1;
                    }
                }
            }
            RigidVar(name) | RigidAbleVar(name, _) => {
                debug_assert!(!is_generated_name(name));
            }
            Type(_, tys) => {
                stack.extend(tys.iter().map(|t| (t, only_unseen)));
            }
            Record(fields, ext) => {
                stack.extend(fields.values().map(|f| (f.as_inner(), only_unseen)));
                ext_stack.push((ext, only_unseen));
            }
            TagUnion(tags, ext, _) => {
                stack.extend(tags.values().flatten().map(|t| (t, only_unseen)));
                ext_stack.push((ext, only_unseen));
            }
            RecursiveTagUnion(rec, tags, ext, _) => {
                stack.push((rec, only_unseen));
                stack.extend(tags.values().flatten().map(|t| (t, only_unseen)));
                ext_stack.push((ext, only_unseen));
            }
            Function(args, _lset, ret) => {
                stack.extend(args.iter().map(|t| (t, only_unseen)));
                stack.push((ret, only_unseen));
            }
            Alias(_, args, real, _) => {
                // Then, count up any phantom args that were missed b/c they're not referenced in
                // the real var. Set `only_unseen` so that we don not double-count vars that do
                // appear in the real var.
                stack.extend(args.iter().map(|t| (t, true)));

                // First, count the occurrences in the real var
                stack.push((real, only_unseen));
            }
            Infinite | Error => {}
            Range(_) => {}
        }
    }

    count_generated_name_usages_in_exts(usages, ext_stack);
}

fn count_generated_name_usages_in_exts<'a>(
    usages: &mut VecMap<Lowercase, usize>,
    exts: impl IntoIterator<Item = (&'a TypeExt, bool)>,
) {
    for (ext, only_unseen) in exts {
        match ext {
            TypeExt::FlexOpen(name) => {
                if is_generated_name(name) {
                    let count = usages.get_or_insert(name.clone(), || 0);
                    if !only_unseen || *count == 0 {
                        *count += 1;
                    }
                }
            }
            TypeExt::RigidOpen(name) => {
                debug_assert!(!is_generated_name(name));
            }
            TypeExt::Closed => {}
        }
    }
}

fn same<'b>(
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    tipe: ErrorType,
) -> Diff<RocDocBuilder<'b>> {
    let (doc, able) = to_doc(alloc, parens, tipe);

    Diff {
        left: doc.clone(),
        right: doc,
        status: Status::Similar,
        left_able: able.clone(),
        right_able: able,
    }
}

fn type_with_able_vars<'b>(
    alloc: &'b RocDocAllocator<'b>,
    typ: RocDocBuilder<'b>,
    able: AbleVariables,
) -> RocDocBuilder<'b> {
    if able.is_empty() {
        // fast path: taken the vast majority of the time
        return typ;
    }

    let mut doc = Vec::with_capacity(1 + 6 * able.len());
    doc.push(typ);

    for (i, (var, abilities)) in able.into_iter().enumerate() {
        doc.push(alloc.string(if i == 0 { " | " } else { ", " }.to_string()));
        doc.push(alloc.type_variable(var));
        doc.push(alloc.space());
        doc.push(alloc.keyword("has"));

        for (i, ability) in abilities.into_sorted_iter().enumerate() {
            if i > 0 {
                doc.push(alloc.space());
                doc.push(alloc.text("&"));
            }
            doc.push(alloc.space());
            doc.push(alloc.symbol_foreign_qualified(ability));
        }
    }

    alloc.concat(doc)
}

pub fn error_type_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    error_type: ErrorType,
) -> RocDocBuilder<'b> {
    let (typ, able_vars) = to_doc(alloc, Parens::Unnecessary, error_type);
    type_with_able_vars(alloc, typ, able_vars)
}

fn compact_builtin_aliases(typ: ErrorType) -> ErrorType {
    use ErrorType::*;
    match typ {
        Alias(Symbol::NUM_NUM, mut args, real, kind) => {
            debug_assert!(args.len() == 1);
            let type_arg = args.remove(0);

            match type_arg {
                Alias(Symbol::NUM_FLOATINGPOINT, inner_args, real, _) => {
                    Alias(Symbol::NUM_FRAC, inner_args, real, AliasKind::Structural)
                }
                Alias(Symbol::NUM_INTEGER, inner_args, real, _) => {
                    Alias(Symbol::NUM_INT, inner_args, real, AliasKind::Structural)
                }
                _ => Alias(Symbol::NUM_NUM, vec![type_arg], real, kind),
            }
        }
        typ => typ,
    }
}

fn to_diff<'b>(
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    type1: ErrorType,
    type2: ErrorType,
) -> Diff<RocDocBuilder<'b>> {
    use ErrorType::*;

    let (type1, type2) = (
        compact_builtin_aliases(type1),
        compact_builtin_aliases(type2),
    );

    // TODO remove clone
    match (type1.clone(), type2.clone()) {
        (Error, Error) | (Infinite, Infinite) => same(alloc, parens, type1),

        (FlexVar(x), FlexVar(y)) if x == y => same(alloc, parens, type1),
        // Wildcards are always different!
        (RigidVar(x), RigidVar(y)) if x == y && x.as_str() != WILDCARD => {
            same(alloc, parens, type1)
        }

        (RigidVar(x), other) | (other, RigidVar(x)) => {
            let (left, left_able) = to_doc(alloc, Parens::InFn, type1);
            let (right, right_able) = to_doc(alloc, Parens::InFn, type2);

            Diff {
                left,
                right,
                status: Status::Different(vec![Problem::BadRigidVar(x, other, None)]),
                left_able,
                right_able,
            }
        }

        (RigidAbleVar(x, abs), other) | (other, RigidAbleVar(x, abs)) => {
            let (left, left_able) = to_doc(alloc, Parens::InFn, type1);
            let (right, right_able) = to_doc(alloc, Parens::InFn, type2);

            Diff {
                left,
                right,
                status: Status::Different(vec![Problem::BadRigidVar(x, other, Some(abs))]),
                left_able,
                right_able,
            }
        }

        (Function(args1, _, ret1), Function(args2, _, ret2)) => {
            if args1.len() == args2.len() {
                let mut status = Status::Similar;
                let arg_diff = traverse(alloc, Parens::InFn, args1, args2);
                let ret_diff = to_diff(alloc, Parens::InFn, *ret1, *ret2);
                status.merge(arg_diff.status);
                status.merge(ret_diff.status);

                let left = report_text::function(alloc, parens, arg_diff.left, ret_diff.left);
                let right = report_text::function(alloc, parens, arg_diff.right, ret_diff.right);
                let mut left_able = arg_diff.left_able;
                left_able.extend(ret_diff.left_able);
                let mut right_able = arg_diff.right_able;
                right_able.extend(ret_diff.right_able);

                Diff {
                    left,
                    right,
                    status,
                    left_able,
                    right_able,
                }
            } else {
                let (left, left_able) = to_doc(alloc, Parens::InFn, type1);
                let (right, right_able) = to_doc(alloc, Parens::InFn, type2);

                Diff {
                    left,
                    right,
                    status: Status::Different(vec![Problem::ArityMismatch(
                        args1.len(),
                        args2.len(),
                    )]),
                    left_able,
                    right_able,
                }
            }
        }
        (Type(symbol1, args1), Type(symbol2, args2)) if symbol1 == symbol2 => {
            let args_diff = traverse(alloc, Parens::InTypeParam, args1, args2);
            let left = report_text::apply(
                alloc,
                parens,
                alloc.symbol_unqualified(symbol1),
                args_diff.left,
            );
            let right = report_text::apply(
                alloc,
                parens,
                alloc.symbol_unqualified(symbol2),
                args_diff.right,
            );

            Diff {
                left,
                right,
                status: args_diff.status,
                left_able: args_diff.left_able,
                right_able: args_diff.right_able,
            }
        }

        (Alias(symbol1, args1, _, _), Alias(symbol2, args2, _, _)) if symbol1 == symbol2 => {
            let args_diff = traverse(alloc, Parens::InTypeParam, args1, args2);
            let left = report_text::apply(
                alloc,
                parens,
                alloc.symbol_unqualified(symbol1),
                args_diff.left,
            );
            let right = report_text::apply(
                alloc,
                parens,
                alloc.symbol_unqualified(symbol2),
                args_diff.right,
            );

            Diff {
                left,
                right,
                status: args_diff.status,
                left_able: args_diff.left_able,
                right_able: args_diff.right_able,
            }
        }

        (Alias(Symbol::BOOL_BOOL, _, _, _), TagUnion(tags, _, _)) | (TagUnion(tags, _, _), Alias(Symbol::BOOL_BOOL, _, _, _))
            if tags.len() == 1
                && tags.keys().all(|t| t.0.as_str() == "True" || t.0.as_str() == "False") =>
        {
            let written_tag = tags.keys().next().unwrap().clone();
            let (left, left_able) = to_doc(alloc, Parens::InFn, type1);
            let (right, right_able) = to_doc(alloc, Parens::InFn, type2);

            Diff {
                left,
                right,
                status: Status::Different(vec![Problem::BoolVsBoolTag(written_tag)]),
                left_able,
                right_able,
            }
        }

        (Alias(sym, _, _, AliasKind::Opaque), _) | (_, Alias(sym, _, _, AliasKind::Opaque))
            // Skip the hint for numbers; it's not as useful as saying "this type is not a number"
            if !OPAQUE_NUM_SYMBOLS.contains(&sym)
                // And same for bools
                && sym != Symbol::BOOL_BOOL =>
        {
            let (left, left_able) = to_doc(alloc, Parens::InFn, type1);
            let (right, right_able) = to_doc(alloc, Parens::InFn, type2);

            Diff {
                left,
                right,
                status: Status::Different(vec![Problem::OpaqueComparedToNonOpaque]),
                left_able,
                right_able,
            }
        }

        (Alias(symbol, _, actual, AliasKind::Structural), other)
            if !symbol.module_id().is_builtin() =>
        {
            // when diffing a structural alias with a non-alias, de-alias
            to_diff(alloc, parens, *actual, other)
        }
        (other, Alias(symbol, _, actual, AliasKind::Structural))
            if !symbol.module_id().is_builtin() =>
        {
            // when diffing a structural alias with a non-alias, de-alias
            to_diff(alloc, parens, other, *actual)
        }

        (Record(fields1, ext1), Record(fields2, ext2)) => {
            diff_record(alloc, fields1, ext1, fields2, ext2)
        }

        (TagUnion(tags1, ext1, pol), TagUnion(tags2, ext2, _)) => {
            diff_tag_union(alloc, pol, &tags1, ext1, &tags2, ext2)
        }

        (RecursiveTagUnion(_rec1, _tags1, _ext1, _), RecursiveTagUnion(_rec2, _tags2, _ext2, _)) => {
            // TODO do a better job here
            let (left, left_able) = to_doc(alloc, Parens::Unnecessary, type1);
            let (right, right_able) = to_doc(alloc, Parens::Unnecessary, type2);

            Diff {
                left,
                right,
                status: Status::Similar,
                left_able,
                right_able,
            }
        }

        pair => {
            // We hit none of the specific cases where we give more detailed information
            let (left, left_able) = to_doc(alloc, parens, type1);
            let (right, right_able) = to_doc(alloc, parens, type2);

            let is_int = |t: &ErrorType| match t {
                ErrorType::Type(Symbol::NUM_INT, _) => true,
                ErrorType::Alias(Symbol::NUM_INT, _, _, _) => true,

                ErrorType::Type(Symbol::NUM_NUM, args) => {
                    matches!(
                        &args.get(0),
                        Some(ErrorType::Type(Symbol::NUM_INTEGER, _))
                            | Some(ErrorType::Alias(Symbol::NUM_INTEGER, _, _, _))
                    )
                }
                ErrorType::Alias(Symbol::NUM_NUM, args, _, _) => {
                    matches!(
                        &args.get(0),
                        Some(ErrorType::Type(Symbol::NUM_INTEGER, _))
                            | Some(ErrorType::Alias(Symbol::NUM_INTEGER, _, _, _))
                    )
                }
                _ => false,
            };
            let is_float = |t: &ErrorType| match t {
                ErrorType::Type(Symbol::NUM_FRAC, _) => true,
                ErrorType::Alias(Symbol::NUM_FRAC, _, _, _) => true,

                ErrorType::Type(Symbol::NUM_NUM, args) => {
                    matches!(
                        &args.get(0),
                        Some(ErrorType::Type(Symbol::NUM_FLOATINGPOINT, _))
                            | Some(ErrorType::Alias(Symbol::NUM_FLOATINGPOINT, _, _, _))
                    )
                }

                ErrorType::Alias(Symbol::NUM_NUM, args, _, _) => {
                    matches!(
                        &args.get(0),
                        Some(ErrorType::Type(Symbol::NUM_FLOATINGPOINT, _))
                            | Some(ErrorType::Alias(Symbol::NUM_FLOATINGPOINT, _, _, _))
                    )
                }
                _ => false,
            };

            let problems = match pair {
                (a, b) if (is_int(&a) && is_float(&b)) || (is_float(&a) && is_int(&b)) => {
                    vec![Problem::IntFloat]
                }
                _ => vec![],
            };

            Diff {
                left,
                right,
                status: Status::Different(problems),
                left_able,
                right_able,
            }
        }
    }
}

fn traverse<'b, I>(
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    args1: I,
    args2: I,
) -> Diff<Vec<RocDocBuilder<'b>>>
where
    I: IntoIterator<Item = ErrorType>,
{
    let mut status = Status::Similar;

    // TODO use ExactSizeIterator to pre-allocate here
    let mut left = Vec::new();
    let mut right = Vec::new();
    let mut left_able = Vec::new();
    let mut right_able = Vec::new();

    for (arg1, arg2) in args1.into_iter().zip(args2.into_iter()) {
        let diff = to_diff(alloc, parens, arg1, arg2);

        left.push(diff.left);
        right.push(diff.right);
        status.merge(diff.status);
        left_able.extend(diff.left_able);
        right_able.extend(diff.right_able);
    }

    Diff {
        left,
        right,
        status,
        left_able,
        right_able,
    }
}

fn ext_has_fixed_fields(ext: &TypeExt) -> bool {
    match ext {
        TypeExt::Closed => true,
        TypeExt::FlexOpen(_) => false,
        TypeExt::RigidOpen(_) => true,
    }
}

fn diff_record<'b>(
    alloc: &'b RocDocAllocator<'b>,
    fields1: SendMap<Lowercase, RecordField<ErrorType>>,
    ext1: TypeExt,
    fields2: SendMap<Lowercase, RecordField<ErrorType>>,
    ext2: TypeExt,
) -> Diff<RocDocBuilder<'b>> {
    let to_overlap_docs = |(field, (t1, t2)): (
        &Lowercase,
        &(RecordField<ErrorType>, RecordField<ErrorType>),
    )| {
        let diff = to_diff(
            alloc,
            Parens::Unnecessary,
            t1.clone().into_inner(),
            t2.clone().into_inner(),
        );

        Diff {
            left: (
                field.clone(),
                alloc.string(field.as_str().to_string()),
                t1.replace(diff.left),
            ),
            right: (
                field.clone(),
                alloc.string(field.as_str().to_string()),
                t2.replace(diff.right),
            ),
            status: {
                match (&t1, &t2) {
                    (RecordField::Demanded(_), RecordField::Optional(_))
                    | (RecordField::Optional(_), RecordField::Demanded(_))
                    | (
                        RecordField::Demanded(_) | RecordField::Required(_),
                        RecordField::RigidOptional(_),
                    )
                    | (
                        RecordField::RigidOptional(_),
                        RecordField::Demanded(_) | RecordField::Required(_),
                    ) => match diff.status {
                        Status::Similar => {
                            Status::Different(vec![Problem::OptionalRequiredMismatch(
                                field.clone(),
                            )])
                        }
                        Status::Different(mut problems) => {
                            problems.push(Problem::OptionalRequiredMismatch(field.clone()));

                            Status::Different(problems)
                        }
                    },
                    _ => diff.status,
                }
            },
            left_able: diff.left_able,
            right_able: diff.right_able,
        }
    };

    let to_unknown_docs = |(field, tipe): (&Lowercase, &RecordField<ErrorType>)| {
        (
            field.clone(),
            alloc.string(field.as_str().to_string()),
            tipe.map(|t| to_doc(alloc, Parens::Unnecessary, t.clone()).0),
        )
    };
    let shared_keys = fields1
        .clone()
        .intersection_with(fields2.clone(), |v1, v2| (v1, v2));
    let left_keys = fields1.clone().relative_complement(fields2.clone());
    let right_keys = fields2.clone().relative_complement(fields1.clone());

    let both = shared_keys.iter().map(to_overlap_docs);
    let mut left = left_keys.iter().map(to_unknown_docs).peekable();
    let mut right = right_keys.iter().map(to_unknown_docs).peekable();

    let all_fields_shared = left.peek().is_none() && right.peek().is_none();

    let status = match (ext_has_fixed_fields(&ext1), ext_has_fixed_fields(&ext2)) {
        (true, true) => match left.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::FieldTypo(
                f.clone(),
                fields2.keys().cloned().collect(),
            )]),
            None => {
                if right.peek().is_none() {
                    Status::Similar
                } else {
                    let result = Status::Different(vec![Problem::FieldsMissing(
                        right.map(|v| v.0).collect(),
                    )]);
                    // we just used the values in `right`.  in
                    right = right_keys.iter().map(to_unknown_docs).peekable();
                    result
                }
            }
        },
        (false, true) => match left.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::FieldTypo(
                f.clone(),
                fields2.keys().cloned().collect(),
            )]),
            None => Status::Similar,
        },
        (true, false) => match right.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::FieldTypo(
                f.clone(),
                fields1.keys().cloned().collect(),
            )]),
            None => Status::Similar,
        },
        (false, false) => Status::Similar,
    };

    let ext_diff = record_ext_to_diff(alloc, ext1, ext2);

    let mut fields_diff: Diff<Vec<(Lowercase, RocDocBuilder<'b>, RecordField<RocDocBuilder<'b>>)>> =
        Diff {
            left: vec![],
            right: vec![],
            status: Status::Similar,
            left_able: vec![],
            right_able: vec![],
        };

    for diff in both {
        fields_diff.left.push(diff.left);
        fields_diff.right.push(diff.right);
        fields_diff.status.merge(diff.status);
        fields_diff.left_able.extend(diff.left_able);
        fields_diff.right_able.extend(diff.right_able);
    }

    if !all_fields_shared {
        fields_diff.left.extend(left);
        fields_diff.right.extend(right);
        fields_diff.status.merge(Status::Different(vec![]));
    }

    // sort fields for display
    fields_diff.left.sort_by(|a, b| a.0.cmp(&b.0));
    fields_diff.right.sort_by(|a, b| a.0.cmp(&b.0));

    let doc1 = report_text::record(
        alloc,
        fields_diff
            .left
            .into_iter()
            .map(|(_, b, c)| (b, c))
            .collect(),
        ext_diff.left,
    );
    let doc2 = report_text::record(
        alloc,
        fields_diff
            .right
            .into_iter()
            .map(|(_, b, c)| (b, c))
            .collect(),
        ext_diff.right,
    );

    fields_diff.status.merge(status);

    Diff {
        left: doc1,
        right: doc2,
        status: fields_diff.status,
        left_able: fields_diff.left_able,
        right_able: fields_diff.right_able,
    }
}

fn same_tag_name_overlap_diff<'b>(
    alloc: &'b RocDocAllocator<'b>,
    field: TagName,
    args1: Vec<ErrorType>,
    args2: Vec<ErrorType>,
) -> Diff<(TagName, RocDocBuilder<'b>, Vec<RocDocBuilder<'b>>)> {
    if args1.len() == args2.len() {
        let diff = traverse(alloc, Parens::InTypeParam, args1, args2);

        Diff {
            left: (field.clone(), alloc.tag_name(field.clone()), diff.left),
            right: (field.clone(), alloc.tag_name(field), diff.right),
            status: diff.status,
            left_able: diff.left_able,
            right_able: diff.right_able,
        }
    } else {
        let (left_doc, left_able): (_, Vec<AbleVariables>) = args1
            .into_iter()
            .map(|arg| to_doc(alloc, Parens::InTypeParam, arg))
            .unzip();
        let (right_doc, right_able): (_, Vec<AbleVariables>) = args2
            .into_iter()
            .map(|arg| to_doc(alloc, Parens::InTypeParam, arg))
            .unzip();

        Diff {
            left: (field.clone(), alloc.tag_name(field.clone()), left_doc),
            right: (field.clone(), alloc.tag_name(field), right_doc),
            status: Status::Similar,
            left_able: left_able.into_iter().flatten().collect(),
            right_able: right_able.into_iter().flatten().collect(),
        }
    }
}

fn diff_tag_union<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pol: Polarity,
    fields1: &SendMap<TagName, Vec<ErrorType>>,
    ext1: TypeExt,
    fields2: &SendMap<TagName, Vec<ErrorType>>,
    ext2: TypeExt,
) -> Diff<RocDocBuilder<'b>> {
    let gen_usages1 = {
        let mut usages = VecMap::default();
        count_generated_name_usages(&mut usages, fields1.values().flatten());
        count_generated_name_usages_in_exts(&mut usages, [(&ext1, false)]);
        usages
    };
    let gen_usages2 = {
        let mut usages = VecMap::default();
        count_generated_name_usages(&mut usages, fields2.values().flatten());
        count_generated_name_usages_in_exts(&mut usages, [(&ext2, false)]);
        usages
    };

    let to_overlap_docs = |(field, (t1, t2)): (TagName, (Vec<ErrorType>, Vec<ErrorType>))| {
        same_tag_name_overlap_diff(alloc, field, t1, t2)
    };
    let to_unknown_docs = |(field, args): (&TagName, &Vec<ErrorType>)| -> (
        TagName,
        RocDocBuilder<'b>,
        Vec<RocDocBuilder<'b>>,
        AbleVariables,
    ) {
        let (args, able): (_, Vec<AbleVariables>) =
            // TODO add spaces between args
            args.iter()
                .map(|arg| to_doc(alloc, Parens::InTypeParam, arg.clone()))
                .unzip();
        (
            field.clone(),
            alloc.tag_name(field.clone()),
            args,
            able.into_iter().flatten().collect(),
        )
    };
    let shared_keys = fields1
        .clone()
        .intersection_with(fields2.clone(), |v1, v2| (v1, v2));

    let left_keys = fields1.clone().relative_complement(fields2.clone());
    let right_keys = fields2.clone().relative_complement(fields1.clone());

    let both = shared_keys.into_iter().map(to_overlap_docs);
    let mut left = left_keys.iter().map(to_unknown_docs).peekable();
    let mut right = right_keys.iter().map(to_unknown_docs).peekable();

    let all_fields_shared = left.peek().is_none() && right.peek().is_none();

    let status = match (ext_has_fixed_fields(&ext1), ext_has_fixed_fields(&ext2)) {
        (false, false) => Status::Similar,
        _ => match (left.peek(), right.peek()) {
            (Some((f, _, _, _)), Some(_)) => Status::Different(vec![Problem::TagTypo(
                f.clone(),
                fields2.keys().cloned().collect(),
            )]),
            (Some(_), None) => {
                let status =
                    Status::Different(vec![Problem::TagsMissing(left.map(|v| v.0).collect())]);
                left = left_keys.iter().map(to_unknown_docs).peekable();
                status
            }
            (None, Some(_)) => {
                let status =
                    Status::Different(vec![Problem::TagsMissing(right.map(|v| v.0).collect())]);
                right = right_keys.iter().map(to_unknown_docs).peekable();
                status
            }
            (None, None) => Status::Similar,
        },
    };

    let ext_diff = tag_ext_to_diff(alloc, pol, ext1, ext2, &gen_usages1, &gen_usages2);

    let mut fields_diff: Diff<Vec<(TagName, RocDocBuilder<'b>, Vec<RocDocBuilder<'b>>)>> = Diff {
        left: vec![],
        right: vec![],
        status: Status::Similar,
        left_able: vec![],
        right_able: vec![],
    };

    for diff in both {
        fields_diff.left.push(diff.left);
        fields_diff.right.push(diff.right);
        fields_diff.status.merge(diff.status);
        fields_diff.left_able.extend(diff.left_able);
        fields_diff.right_able.extend(diff.right_able);
    }

    if !all_fields_shared {
        for (tag, tag_doc, args, able) in left {
            fields_diff.left.push((tag, tag_doc, args));
            fields_diff.left_able.extend(able);
        }
        for (tag, tag_doc, args, able) in right {
            fields_diff.right.push((tag, tag_doc, args));
            fields_diff.right_able.extend(able);
        }
        fields_diff.status.merge(Status::Different(vec![]));
    }

    fields_diff.left.sort_by(|a, b| a.0.cmp(&b.0));
    fields_diff.right.sort_by(|a, b| a.0.cmp(&b.0));

    let lefts = fields_diff
        .left
        .into_iter()
        .map(|(_, a, b)| (a, b))
        .collect();
    let rights = fields_diff
        .right
        .into_iter()
        .map(|(_, a, b)| (a, b))
        .collect();

    let doc1 = report_text::tag_union(alloc, lefts, ext_diff.left);
    let doc2 = report_text::tag_union(alloc, rights, ext_diff.right);

    fields_diff.status.merge(status);

    Diff {
        left: doc1,
        right: doc2,
        status: fields_diff.status,
        left_able: fields_diff.left_able,
        right_able: fields_diff.right_able,
    }
}

fn tag_ext_to_diff<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pol: Polarity,
    ext1: TypeExt,
    ext2: TypeExt,
    gen_usages1: &VecMap<Lowercase, usize>,
    gen_usages2: &VecMap<Lowercase, usize>,
) -> Diff<Option<RocDocBuilder<'b>>> {
    let status = ext_to_status(&ext1, &ext2);
    let ext_doc_1 = tag_ext_to_doc(alloc, pol, gen_usages1, ext1);
    let ext_doc_2 = tag_ext_to_doc(alloc, pol, gen_usages2, ext2);

    match &status {
        Status::Similar => Diff {
            left: ext_doc_1,
            right: ext_doc_2,
            status,
            left_able: vec![],
            right_able: vec![],
        },
        Status::Different(_) => Diff {
            // NOTE elm colors these differently at this point
            left: ext_doc_1,
            right: ext_doc_2,
            status,
            left_able: vec![],
            right_able: vec![],
        },
    }
}

fn record_ext_to_diff<'b>(
    alloc: &'b RocDocAllocator<'b>,
    ext1: TypeExt,
    ext2: TypeExt,
) -> Diff<Option<RocDocBuilder<'b>>> {
    let status = ext_to_status(&ext1, &ext2);
    let ext_doc_1 = record_ext_to_doc(alloc, ext1);
    let ext_doc_2 = record_ext_to_doc(alloc, ext2);

    match &status {
        Status::Similar => Diff {
            left: ext_doc_1,
            right: ext_doc_2,
            status,
            left_able: vec![],
            right_able: vec![],
        },
        Status::Different(_) => Diff {
            // NOTE elm colors these differently at this point
            left: ext_doc_1,
            right: ext_doc_2,
            status,
            left_able: vec![],
            right_able: vec![],
        },
    }
}

fn ext_to_status(ext1: &TypeExt, ext2: &TypeExt) -> Status {
    use TypeExt::*;
    match ext1 {
        Closed => match ext2 {
            Closed => Status::Similar,
            FlexOpen(_) => Status::Similar,
            RigidOpen(_) => Status::Different(vec![]),
        },
        FlexOpen(_) => Status::Similar,

        RigidOpen(x) => match ext2 {
            Closed => Status::Different(vec![]),
            FlexOpen(_) => Status::Similar,
            RigidOpen(y) => {
                if x == y {
                    Status::Similar
                } else {
                    Status::Different(vec![Problem::BadRigidVar(
                        x.clone(),
                        ErrorType::RigidVar(y.clone()),
                        None,
                    )])
                }
            }
        },
    }
}

mod report_text {
    use crate::report::{Annotation, RocDocAllocator, RocDocBuilder};
    use roc_module::ident::Lowercase;
    use roc_types::pretty_print::Parens;
    use roc_types::types::{ErrorType, RecordField, TypeExt};
    use ven_pretty::DocAllocator;

    fn with_parens<'b>(
        alloc: &'b RocDocAllocator<'b>,
        text: RocDocBuilder<'b>,
    ) -> RocDocBuilder<'b> {
        alloc.text("(").append(text).append(alloc.text(")"))
    }

    pub fn function<'b>(
        alloc: &'b RocDocAllocator<'b>,
        parens: Parens,
        args: Vec<RocDocBuilder<'b>>,
        ret: RocDocBuilder<'b>,
    ) -> RocDocBuilder<'b> {
        let function_doc = alloc.concat([
            alloc.intersperse(args, alloc.reflow(", ")),
            alloc.reflow(" -> "),
            ret,
        ]);

        match parens {
            Parens::Unnecessary => function_doc,
            _ => with_parens(alloc, function_doc),
        }
    }

    pub fn apply<'b>(
        alloc: &'b RocDocAllocator<'b>,
        parens: Parens,
        name: RocDocBuilder<'b>,
        args: Vec<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        if args.is_empty() {
            name
        } else {
            let apply_doc =
                alloc.concat([name, alloc.space(), alloc.intersperse(args, alloc.space())]);

            match parens {
                Parens::Unnecessary | Parens::InFn => apply_doc,
                Parens::InTypeParam => with_parens(alloc, apply_doc),
            }
        }
    }

    pub fn record<'b>(
        alloc: &'b RocDocAllocator<'b>,
        entries: Vec<(RocDocBuilder<'b>, RecordField<RocDocBuilder<'b>>)>,
        opt_ext: Option<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        let ext_doc = if let Some(t) = opt_ext {
            t
        } else {
            alloc.nil()
        };

        if entries.is_empty() {
            alloc.text("{}").append(ext_doc)
        } else {
            let entry_to_doc =
                |(field_name, field_type): (RocDocBuilder<'b>, RecordField<RocDocBuilder<'b>>)| {
                    match field_type {
                        RecordField::Demanded(field)
                        | RecordField::Required(field)
                        | RecordField::RigidRequired(field) => {
                            field_name.append(alloc.text(" : ")).append(field)
                        }
                        RecordField::Optional(field) | RecordField::RigidOptional(field) => {
                            field_name.append(alloc.text(" ? ")).append(field)
                        }
                    }
                };

            let starts =
                std::iter::once(alloc.reflow("{ ")).chain(std::iter::repeat(alloc.reflow(", ")));

            let entries_doc = alloc.concat(
                entries
                    .into_iter()
                    .zip(starts)
                    .map(|(entry, start)| start.append(entry_to_doc(entry))),
            );

            entries_doc.append(alloc.reflow(" }")).append(ext_doc)
        }
    }

    pub fn to_suggestion_record<'b>(
        alloc: &'b RocDocAllocator<'b>,
        f: (Lowercase, RecordField<ErrorType>),
        fs: Vec<(Lowercase, RecordField<ErrorType>)>,
        ext: TypeExt,
    ) -> RocDocBuilder<'b> {
        use crate::error::r#type::{record_ext_to_doc, to_doc};

        let entry_to_doc = |(name, tipe): (Lowercase, RecordField<ErrorType>)| {
            (
                alloc.string(name.as_str().to_string()),
                to_doc(alloc, Parens::Unnecessary, tipe.into_inner()).0,
            )
        };

        let mut selection = vec![f];
        selection.extend(fs);

        let fields = selection.into_iter().map(entry_to_doc).collect();

        vertical_record(alloc, fields, record_ext_to_doc(alloc, ext))
            .annotate(Annotation::TypeBlock)
            .indent(4)
    }

    fn vertical_record<'b>(
        alloc: &'b RocDocAllocator<'b>,
        entries: Vec<(RocDocBuilder<'b>, RocDocBuilder<'b>)>,
        opt_ext: Option<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        let fields = if entries.is_empty() {
            alloc.text("{}")
        } else {
            const MAX_ENTRIES_TO_DISPLAY: usize = 4;

            let is_truncated = entries.len() > MAX_ENTRIES_TO_DISPLAY;
            let entry_to_doc =
                |(field_name, field_type): (RocDocBuilder<'b>, RocDocBuilder<'b>)| {
                    field_name
                        .indent(4)
                        .append(alloc.text(" : "))
                        .append(field_type)
                        .append(alloc.text(","))
                };

            let closing = std::iter::once(alloc.text("}"));
            let fields = std::iter::once(alloc.reflow("{")).chain(
                entries
                    .into_iter()
                    .map(entry_to_doc)
                    .take(MAX_ENTRIES_TO_DISPLAY),
            );

            if is_truncated {
                alloc.vcat(
                    fields
                        .chain(std::iter::once(alloc.text("…").indent(4)))
                        .chain(closing),
                )
            } else {
                alloc.vcat(fields.chain(closing))
            }
        };

        match opt_ext {
            Some(ext) => fields.append(ext),
            None => fields,
        }
    }

    pub fn tag_union<'b>(
        alloc: &'b RocDocAllocator<'b>,
        entries: Vec<(RocDocBuilder<'b>, Vec<RocDocBuilder<'b>>)>,
        opt_ext: Option<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        let ext_doc = if let Some(t) = opt_ext {
            t
        } else {
            alloc.nil()
        };

        if entries.is_empty() {
            alloc.text("[]")
        } else {
            let entry_to_doc = |(tag_name, arguments): (RocDocBuilder<'b>, Vec<_>)| {
                if arguments.is_empty() {
                    tag_name
                } else {
                    tag_name
                        .append(alloc.space())
                        .append(alloc.intersperse(arguments, alloc.space()))
                }
            };

            let starts =
                std::iter::once(alloc.reflow("[")).chain(std::iter::repeat(alloc.reflow(", ")));

            let entries_doc = alloc.concat(
                entries
                    .into_iter()
                    .zip(starts)
                    .map(|(entry, start)| start.append(entry_to_doc(entry))),
            );

            entries_doc.append(alloc.reflow("]")).append(ext_doc)
        }
    }

    pub fn recursive_tag_union<'b>(
        alloc: &'b RocDocAllocator<'b>,
        rec_var: RocDocBuilder<'b>,
        entries: Vec<(RocDocBuilder<'b>, Vec<RocDocBuilder<'b>>)>,
        opt_ext: Option<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        let ext_doc = if let Some(t) = opt_ext {
            t
        } else {
            alloc.nil()
        };

        if entries.is_empty() {
            alloc.text("[]")
        } else {
            let entry_to_doc = |(tag_name, arguments): (RocDocBuilder<'b>, Vec<_>)| {
                if arguments.is_empty() {
                    tag_name
                } else {
                    tag_name
                        .append(alloc.space())
                        .append(alloc.intersperse(arguments, alloc.space()))
                }
            };

            let starts =
                std::iter::once(alloc.reflow("[")).chain(std::iter::repeat(alloc.reflow(", ")));

            let entries_doc = alloc.concat(
                entries
                    .into_iter()
                    .zip(starts)
                    .map(|(entry, start)| start.append(entry_to_doc(entry))),
            );

            entries_doc
                .append(alloc.reflow("]"))
                .append(ext_doc)
                .append(alloc.text(" as "))
                .append(rec_var)
        }
    }

    pub fn range<'b>(
        alloc: &'b RocDocAllocator<'b>,
        ranged_types: Vec<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        let mut doc = Vec::with_capacity(ranged_types.len() * 2);

        let last = ranged_types.len() - 1;
        for (i, choice) in ranged_types.into_iter().enumerate() {
            if i == last && i == 1 {
                doc.push(alloc.reflow(" or "));
            } else if i == last && i > 1 {
                doc.push(alloc.reflow(", or "));
            } else if i > 0 {
                doc.push(alloc.reflow(", "));
            }

            doc.push(choice);
        }

        alloc.concat(doc)
    }
}

fn list_abilities<'a>(alloc: &'a RocDocAllocator<'a>, abilities: &AbilitySet) -> RocDocBuilder<'a> {
    let mut abilities = abilities.sorted_iter();
    if abilities.len() == 1 {
        alloc.concat([
            alloc.reflow("ability "),
            alloc.symbol_unqualified(*abilities.next().unwrap()),
        ])
    } else if abilities.len() == 2 {
        alloc.concat([
            alloc.reflow("abilities "),
            alloc.symbol_unqualified(*abilities.next().unwrap()),
            alloc.reflow(" and "),
            alloc.symbol_unqualified(*abilities.next().unwrap()),
        ])
    } else {
        let last_ability = abilities.len() - 1;

        alloc.concat([
            alloc.reflow("abilities "),
            alloc.intersperse(
                abilities.enumerate().map(|(i, &ab)| {
                    if i == last_ability {
                        alloc.concat([alloc.reflow(" and "), alloc.symbol_unqualified(ab)])
                    } else {
                        alloc.symbol_unqualified(ab)
                    }
                }),
                alloc.reflow(", "),
            ),
            alloc.reflow(" abilities"),
        ])
    }
}

fn type_problem_to_pretty<'b>(
    alloc: &'b RocDocAllocator<'b>,
    problem: crate::error::r#type::Problem,
    expectation_context: ExpectationContext<'b>,
) -> RocDocBuilder<'b> {
    use crate::error::r#type::Problem::*;

    match (problem, expectation_context) {
        (FieldTypo(typo, possibilities), _) => {
            let suggestions = suggest::sort(typo.as_str(), possibilities);

            match suggestions.get(0) {
                None => alloc.nil(),
                Some(nearest) => {
                    let typo_str = format!("{}", typo);
                    let nearest_str = format!("{}", nearest);

                    let found = alloc.text(typo_str).annotate(Annotation::Typo);
                    let suggestion = alloc.text(nearest_str).annotate(Annotation::TypoSuggestion);

                    let tip1 = alloc
                        .tip()
                        .append(alloc.reflow("Seems like a record field typo. Maybe "))
                        .append(found)
                        .append(alloc.reflow(" should be "))
                        .append(suggestion)
                        .append(alloc.text("?"));

                    let tip2 = alloc.tip().append(alloc.reflow(ADD_ANNOTATIONS));

                    tip1.append(alloc.line()).append(alloc.line()).append(tip2)
                }
            }
        }
        (FieldsMissing(missing), _) => match missing.split_last() {
            None => alloc.nil(),
            Some((f1, [])) => alloc
                .tip()
                .append(alloc.reflow("Looks like the "))
                .append(f1.as_str().to_owned())
                .append(alloc.reflow(" field is missing.")),
            Some((last, init)) => {
                let separator = alloc.reflow(", ");

                alloc
                    .tip()
                    .append(alloc.reflow("Looks like the "))
                    .append(
                        alloc.intersperse(init.iter().map(|v| v.as_str().to_owned()), separator),
                    )
                    .append(alloc.reflow(" and "))
                    .append(alloc.text(last.as_str().to_owned()))
                    .append(alloc.reflow(" fields are missing."))
            }
        },
        (TagTypo(typo, possibilities_tn), _) => {
            let possibilities: Vec<IdentStr> = possibilities_tn
                .into_iter()
                .map(|tag_name| tag_name.as_ident_str())
                .collect();
            let typo_str = format!("{}", typo.as_ident_str());
            let suggestions = suggest::sort(&typo_str, possibilities);

            match suggestions.get(0) {
                None => alloc.nil(),
                Some(nearest) => {
                    let nearest_str = format!("{}", nearest);

                    let found = alloc.text(typo_str).annotate(Annotation::Typo);
                    let suggestion = alloc.text(nearest_str).annotate(Annotation::TypoSuggestion);

                    let tip1 = alloc
                        .tip()
                        .append(alloc.reflow("Seems like a tag typo. Maybe "))
                        .append(found)
                        .append(" should be ")
                        .append(suggestion)
                        .append(alloc.text("?"));

                    let tip2 = alloc.tip().append(alloc.reflow(ADD_ANNOTATIONS));

                    tip1.append(alloc.line()).append(alloc.line()).append(tip2)
                }
            }
        }
        (ArityMismatch(found, expected), _) => {
            let line = if found < expected {
                format!(
                    "It looks like it takes too few arguments. I was expecting {} more.",
                    expected - found
                )
            } else {
                format!(
                    "It looks like it takes too many arguments. I'm seeing {} extra.",
                    found - expected
                )
            };

            alloc.tip().append(line)
        }

        (BadRigidVar(x, tipe, Some(abilities)), expectation) => {
            use ErrorType::*;

            let rigid_able_vs_concrete = |name: Lowercase, a_thing| {
                alloc.stack([
                    alloc
                        .note("")
                        .append(alloc.reflow("The type variable "))
                        .append(alloc.type_variable(name.clone()))
                        .append(alloc.reflow(" says it can take on any value that has the "))
                        .append(list_abilities(alloc, &abilities))
                        .append(alloc.reflow(".")),
                    alloc.concat([
                        alloc.reflow("But, I see that the type is only ever used as a "),
                        a_thing,
                        alloc.reflow(". Can you replace "),
                        alloc.type_variable(name),
                        alloc.reflow(" with a more specific type?"),
                    ]),
                ])
            };

            let rigid_able_vs_different_flex_able =
                |name: Lowercase, abilities: AbilitySet, other_abilities: AbilitySet| {
                    let extra_abilities = other_abilities
                        .into_sorted_iter()
                        .filter(|ability| !abilities.contains(ability))
                        .collect::<AbilitySet>();

                    let type_var_doc = match expectation {
                        ExpectationContext::Annotation { on } => alloc.concat([
                            alloc.reflow("The type annotation "),
                            on,
                            alloc.reflow(" says that the type variable "),
                            alloc.type_variable(name.clone()),
                        ]),
                        ExpectationContext::WhenCondition | ExpectationContext::Arbitrary => alloc
                            .concat([
                                alloc.reflow("The type variable "),
                                alloc.type_variable(name.clone()),
                                alloc.reflow(" says it"),
                            ]),
                    };

                    let n_extra_abilities = extra_abilities.sorted_iter().len();

                    alloc.stack([
                        alloc
                            .note("")
                            .append(type_var_doc)
                            .append(alloc.reflow(" can take on any value that has only the "))
                            .append(list_abilities(alloc, &abilities))
                            .append(alloc.reflow(".")),
                        alloc.concat([
                            alloc.reflow("But, I see that it's also used as if it has the "),
                            list_abilities(alloc, &extra_abilities),
                            alloc.reflow(". Can you use "),
                            alloc.type_variable(name.clone()),
                            alloc.reflow(" without "),
                            if n_extra_abilities > 1 {
                                alloc.reflow("those abilities")
                            } else {
                                alloc.reflow("that ability")
                            },
                            alloc.reflow("? If not, consider adding "),
                            if n_extra_abilities > 1 {
                                alloc.reflow("them")
                            } else {
                                alloc.reflow("it")
                            },
                            alloc.reflow(" to the "),
                            alloc.keyword("has"),
                            alloc.reflow(" clause of "),
                            alloc.type_variable(name),
                            alloc.reflow("."),
                        ]),
                    ])
                };

            let bad_double_rigid = |a: Lowercase, b: Lowercase| {
                alloc
                    .tip()
                    .append(alloc.reflow("Your type annotation uses "))
                    .append(alloc.type_variable(a))
                    .append(alloc.reflow(" and "))
                    .append(alloc.type_variable(b))
                    .append(alloc.reflow(" as separate type variables. Your code seems to be saying they are the same though. Maybe they should be the same in your type annotation? Maybe your code uses them in a weird way?"))
            };

            match tipe {
                Infinite | Error | FlexVar(_) => alloc.nil(),
                FlexAbleVar(_, other_abilities) => {
                    rigid_able_vs_different_flex_able(x, abilities, other_abilities)
                }
                RigidVar(y) | RigidAbleVar(y, _) => bad_double_rigid(x, y),
                Function(_, _, _) => rigid_able_vs_concrete(x, alloc.reflow("a function value")),
                Record(_, _) => rigid_able_vs_concrete(x, alloc.reflow("a record value")),
                TagUnion(_, _, _) | RecursiveTagUnion(_, _, _, _) => {
                    rigid_able_vs_concrete(x, alloc.reflow("a tag value"))
                }
                Alias(symbol, _, _, _) | Type(symbol, _) => rigid_able_vs_concrete(
                    x,
                    alloc.concat([
                        alloc.reflow("a "),
                        alloc.symbol_unqualified(symbol),
                        alloc.reflow(" value"),
                    ]),
                ),
                Range(..) => rigid_able_vs_concrete(x, alloc.reflow("a range")),
            }
        }

        (BadRigidVar(x, tipe, None), expectation) => {
            use ErrorType::*;

            let bad_rigid_var = |name: Lowercase, a_thing| {
                alloc
                    .tip()
                    .append(alloc.reflow("The type annotation uses the type variable "))
                    .append(alloc.type_variable(name))
                    .append(alloc.reflow(" to say that this definition can produce any type of value.")
                    .append(alloc.reflow(" But in the body I see that it will only produce ")))
                    .append(a_thing)
                    .append(alloc.reflow(" of a single specific type. Maybe change the type annotation to be more specific? Maybe change the code to be more general?"))
            };

            let bad_double_wildcard = || {
                let mut hints_lines = vec![
                    alloc.reflow(
                        "Any connection between types must use a named type variable, not a ",
                    ),
                    alloc.type_variable(WILDCARD.into()),
                    alloc.reflow("!"),
                ];
                if let ExpectationContext::Annotation { on } = expectation {
                    hints_lines.append(&mut vec![
                        alloc.reflow(" Maybe the annotation "),
                        on,
                        alloc.reflow(" should have a named type variable in place of the "),
                        alloc.type_variable(WILDCARD.into()),
                        alloc.reflow("?"),
                    ]);
                }
                alloc.tip().append(alloc.concat(hints_lines))
            };

            let bad_double_rigid = |a: Lowercase, b: Lowercase| {
                if a.as_str() == WILDCARD && b.as_str() == WILDCARD {
                    return bad_double_wildcard();
                }
                let line = r#" as separate type variables. Your code seems to be saying they are the same though. Maybe they should be the same in your type annotation? Maybe your code uses them in a weird way?"#;

                alloc
                    .tip()
                    .append(alloc.reflow("Your type annotation uses "))
                    .append(alloc.type_variable(a))
                    .append(alloc.reflow(" and "))
                    .append(alloc.type_variable(b))
                    .append(alloc.reflow(line))
            };

            match tipe {
                Infinite | Error | FlexVar(_) => alloc.nil(),
                FlexAbleVar(_, abilities) => {
                    let mut abilities = abilities.into_sorted_iter();
                    let msg = if abilities.len() == 1 {
                        alloc.concat([
                            alloc.reflow("an instance of the ability "),
                            alloc.symbol_unqualified(abilities.next().unwrap()),
                        ])
                    } else {
                        alloc.concat([
                            alloc.reflow("an instance of the "),
                            alloc.intersperse(
                                abilities.map(|ab| alloc.symbol_unqualified(ab)),
                                alloc.reflow(", "),
                            ),
                            alloc.reflow(" abilities"),
                        ])
                    };
                    bad_rigid_var(x, msg)
                }
                RigidVar(y) | RigidAbleVar(y, _) => bad_double_rigid(x, y),
                Function(_, _, _) => bad_rigid_var(x, alloc.reflow("a function value")),
                Record(_, _) => bad_rigid_var(x, alloc.reflow("a record value")),
                TagUnion(_, _, _) | RecursiveTagUnion(_, _, _, _) => {
                    bad_rigid_var(x, alloc.reflow("a tag value"))
                }
                Alias(symbol, _, _, _) | Type(symbol, _) => bad_rigid_var(
                    x,
                    alloc.concat([
                        alloc.reflow("a "),
                        alloc.symbol_unqualified(symbol),
                        alloc.reflow(" value"),
                    ]),
                ),
                Range(..) => bad_rigid_var(x, alloc.reflow("a range")),
            }
        }

        (IntFloat, _) => alloc.tip().append(alloc.concat([
            alloc.reflow("You can convert between "),
            alloc.type_str("Int"),
            alloc.reflow(" and "),
            alloc.type_str("Frac"),
            alloc.reflow(" using functions like "),
            alloc.symbol_qualified(Symbol::NUM_TO_FRAC),
            alloc.reflow(" and "),
            alloc.symbol_qualified(Symbol::NUM_ROUND),
            alloc.reflow("."),
        ])),

        (TagsMissing(missing), ExpectationContext::WhenCondition) => match missing.split_last() {
            None => alloc.nil(),
            Some(split) => {
                let missing_tags = match split {
                    (f1, []) => alloc.tag_name(f1.clone()).append(alloc.reflow(" tag.")),
                    (last, init) => alloc
                        .intersperse(init.iter().map(|v| alloc.tag_name(v.clone())), ", ")
                        .append(alloc.reflow(" and "))
                        .append(alloc.tag_name(last.clone()))
                        .append(alloc.reflow(" tags.")),
                };

                let tip1 = alloc
                    .tip()
                    .append(alloc.reflow("Looks like the branches are missing coverage of the "))
                    .append(missing_tags);

                let tip2 = alloc
                    .tip()
                    .append(alloc.reflow("Maybe you need to add a catch-all branch, like "))
                    .append(alloc.keyword("_"))
                    .append(alloc.reflow("?"));

                alloc.stack([tip1, tip2])
            }
        },

        (TagsMissing(missing), _) => match missing.split_last() {
            None => alloc.nil(),
            Some((f1, [])) => {
                let tip1 = alloc
                    .tip()
                    .append(alloc.reflow("Looks like a closed tag union does not have the "))
                    .append(alloc.tag_name(f1.clone()))
                    .append(alloc.reflow(" tag."));

                let tip2 = alloc.tip().append(alloc.reflow(
                    "Closed tag unions can't grow, \
                    because that might change the size in memory. \
                    Can you use an open tag union?",
                ));

                alloc.stack([tip1, tip2])
            }

            Some((last, init)) => {
                let separator = alloc.reflow(", ");

                let tip1 = alloc
                    .tip()
                    .append(alloc.reflow("Looks like a closed tag union does not have the "))
                    .append(
                        alloc
                            .intersperse(init.iter().map(|v| alloc.tag_name(v.clone())), separator),
                    )
                    .append(alloc.reflow(" and "))
                    .append(alloc.tag_name(last.clone()))
                    .append(alloc.reflow(" tags."));

                let tip2 = alloc.tip().append(alloc.reflow(
                    "Closed tag unions can't grow, \
                    because that might change the size in memory. \
                    Can you use an open tag union?",
                ));

                alloc.stack([tip1, tip2])
            }
        },
        (OptionalRequiredMismatch(field), _) => alloc.tip().append(alloc.concat([
            alloc.reflow("To extract the "),
            alloc.record_field(field),
            alloc.reflow(
                " field it must be non-optional, but the type says this field is optional. ",
            ),
            alloc.reflow("Learn more about optional fields at TODO."),
        ])),

        (OpaqueComparedToNonOpaque, _) => alloc.tip().append(alloc.concat([
            alloc.reflow(
                "Type comparisons between an opaque type are only ever \
                equal if both types are the same opaque type. Did you mean \
                to create an opaque type by wrapping it? If I have an opaque type ",
            ),
            alloc.type_str("Age := U32"),
            alloc.reflow(" I can create an instance of this opaque type by doing "),
            alloc.type_str("@Age 23"),
            alloc.reflow("."),
        ])),

        (BoolVsBoolTag(tag), _) => alloc.tip().append(alloc.concat([
            alloc.reflow("Did you mean to use "),
            alloc.symbol_qualified(if tag.0.as_str() == "True" {
                Symbol::BOOL_TRUE
            } else {
                Symbol::BOOL_FALSE
            }),
            alloc.reflow(" rather than "),
            alloc.tag_name(tag),
            alloc.reflow("?"),
        ])),
    }
}

#[allow(clippy::too_many_arguments)]
fn report_record_field_typo<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    opt_sym: Option<Symbol>,
    field_prefix: &str,
    field: &Lowercase,
    field_suffix: &str,
    field_region: Region,
    actual_fields: SendMap<Lowercase, RecordField<ErrorType>>,
    ext: TypeExt,
) -> Report<'b> {
    let header = {
        let f_doc = alloc
            .text(field.as_str().to_string())
            .annotate(Annotation::Typo);

        let r_doc = match opt_sym {
            Some(symbol) => alloc.symbol_unqualified(symbol).append(" "),
            None => alloc.text(""),
        };

        alloc.concat([
            alloc.reflow("This "),
            r_doc,
            alloc.reflow("record doesn’t have a "),
            f_doc,
            alloc.reflow(" field:"),
        ])
    };

    let mut suggestions = suggest::sort(
        field.as_str(),
        actual_fields.into_iter().collect::<Vec<_>>(),
    );

    let doc = alloc.stack([
        header,
        alloc.region(lines.convert_region(field_region)),
        if suggestions.is_empty() {
            let r_doc = match opt_sym {
                Some(symbol) => alloc.symbol_unqualified(symbol).append(" is"),
                None => alloc.text("it’s"),
            };
            alloc.concat([
                alloc.reflow("In fact, "),
                r_doc,
                alloc.reflow(" a record with no fields at all!"),
            ])
        } else {
            let f = suggestions.remove(0);
            let fs = suggestions;
            let f_doc = alloc
                .text(format!("{}{}{}", field_prefix, field, field_suffix))
                .annotate(Annotation::Typo);

            let r_doc = match opt_sym {
                Some(symbol) => alloc.symbol_unqualified(symbol).append(" fields"),
                None => alloc.text("fields on the record"),
            };

            alloc.stack([
                alloc.concat([
                    alloc.reflow("There may be a typo. These "),
                    r_doc,
                    alloc.reflow(" are the most similar:"),
                ]),
                report_text::to_suggestion_record(alloc, f.clone(), fs, ext),
                alloc.concat([
                    alloc.reflow("Maybe "),
                    f_doc,
                    alloc.reflow(" should be "),
                    alloc
                        .text(format!("{}{}{}", field_prefix, f.0, field_suffix))
                        .annotate(Annotation::TypoSuggestion),
                    alloc.reflow(" instead?"),
                ]),
            ])
        },
    ]);

    Report {
        filename,
        title: "TYPE MISMATCH".to_string(),
        doc,
        severity: Severity::RuntimeError,
    }
}

fn exhaustive_problem<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    filename: PathBuf,
    problem: roc_exhaustive::Error,
) -> Report<'a> {
    use roc_exhaustive::Context::*;
    use roc_exhaustive::Error::*;

    match problem {
        Incomplete(region, context, missing) => match context {
            BadArg => {
                let doc = alloc.stack([
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(lines.convert_region(region)),
                    alloc.reflow("Other possibilities include:"),
                    unhandled_patterns_to_doc_block(alloc, missing),
                    alloc.concat([
                        alloc.reflow(
                            "I would have to crash if I saw one of those! \
                        So rather than pattern matching in function arguments, put a ",
                        ),
                        alloc.keyword("when"),
                        alloc.reflow(" in the function body to account for all possibilities."),
                    ]),
                ]);

                Report {
                    filename,
                    title: "UNSAFE PATTERN".to_string(),
                    doc,
                    severity: Severity::RuntimeError,
                }
            }
            BadDestruct => {
                let doc = alloc.stack([
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(lines.convert_region(region)),
                    alloc.reflow("Other possibilities include:"),
                    unhandled_patterns_to_doc_block(alloc, missing),
                    alloc.concat([
                        alloc.reflow(
                            "I would have to crash if I saw one of those! \
                       You can use a binding to deconstruct a value if there is only ONE possibility. \
                       Use a "
                        ),
                        alloc.keyword("when"),
                        alloc.reflow(" to account for all possibilities."),
                    ]),
                ]);

                Report {
                    filename,
                    title: "UNSAFE PATTERN".to_string(),
                    doc,
                    severity: Severity::RuntimeError,
                }
            }
            BadCase => {
                let doc = alloc.stack([
                    alloc.concat([
                        alloc.reflow("This "),
                        alloc.keyword("when"),
                        alloc.reflow(" does not cover all the possibilities:"),
                    ]),
                    alloc.region(lines.convert_region(region)),
                    alloc.reflow("Other possibilities include:"),
                    unhandled_patterns_to_doc_block(alloc, missing),
                    alloc.reflow(
                        "I would have to crash if I saw one of those! \
                        Add branches for them!",
                    ),
                    // alloc.hint().append(alloc.reflow("or use a hole.")),
                ]);

                Report {
                    filename,
                    title: "UNSAFE PATTERN".to_string(),
                    doc,
                    severity: Severity::RuntimeError,
                }
            }
        },
        Redundant {
            overall_region,
            branch_region,
            index,
        } => {
            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The "),
                    alloc.string(index.ordinal()),
                    alloc.reflow(" pattern is redundant:"),
                ]),
                alloc.region_with_subregion(
                    lines.convert_region(overall_region),
                    lines.convert_region(branch_region),
                ),
                alloc.reflow(
                    "Any value of this shape will be handled by \
                a previous pattern, so this one should be removed.",
                ),
            ]);

            Report {
                filename,
                title: "REDUNDANT PATTERN".to_string(),
                doc,
                severity: Severity::Warning,
            }
        }
        Unmatchable {
            overall_region,
            branch_region,
            index,
        } => {
            let doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The "),
                    alloc.string(index.ordinal()),
                    alloc.reflow(" pattern will never be matched:"),
                ]),
                alloc.region_with_subregion(
                    lines.convert_region(overall_region),
                    lines.convert_region(branch_region),
                ),
                alloc.reflow(
                    "It's impossible to create a value of this shape, \
                so this pattern can be safely removed!",
                ),
            ]);

            Report {
                filename,
                title: "UNMATCHABLE PATTERN".to_string(),
                doc,
                severity: Severity::Warning,
            }
        }
    }
}

pub fn unhandled_patterns_to_doc_block<'b>(
    alloc: &'b RocDocAllocator<'b>,
    patterns: Vec<roc_exhaustive::Pattern>,
) -> RocDocBuilder<'b> {
    alloc
        .vcat(
            patterns
                .into_iter()
                .map(|v| exhaustive_pattern_to_doc(alloc, v)),
        )
        .indent(4)
        .annotate(Annotation::TypeBlock)
}

fn exhaustive_pattern_to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: roc_exhaustive::Pattern,
) -> RocDocBuilder<'b> {
    pattern_to_doc_help(alloc, pattern, false)
}

const AFTER_TAG_INDENT: &str = "    ";

fn pattern_to_doc_help<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pattern: roc_exhaustive::Pattern,
    in_type_param: bool,
) -> RocDocBuilder<'b> {
    use roc_can::exhaustive::{GUARD_CTOR, NONEXHAUSIVE_CTOR};
    use roc_exhaustive::Literal::*;
    use roc_exhaustive::Pattern::*;
    use roc_exhaustive::RenderAs;

    match pattern {
        Anything => alloc.text("_"),
        Literal(l) => match l {
            Int(i) => alloc.text(i128::from_ne_bytes(i).to_string()),
            U128(i) => alloc.text(u128::from_ne_bytes(i).to_string()),
            Bit(true) => alloc.text("Bool.true"),
            Bit(false) => alloc.text("Bool.false"),
            Byte(b) => alloc.text(b.to_string()),
            Float(f) => alloc.text(f.to_string()),
            Decimal(d) => alloc.text(RocDec::from_ne_bytes(d).to_string()),
            Str(s) => alloc.string(s.into()),
        },
        List(arity, patterns) => {
            let inner = match arity {
                ListArity::Exact(_) => alloc.intersperse(
                    patterns
                        .into_iter()
                        .map(|p| pattern_to_doc_help(alloc, p, false)),
                    alloc.text(",").append(alloc.space()),
                ),
                ListArity::Slice(num_before, num_after) => {
                    let mut all_patterns = patterns
                        .into_iter()
                        .map(|p| pattern_to_doc_help(alloc, p, in_type_param));

                    let spread = alloc.text("..");
                    let comma_space = alloc.text(",").append(alloc.space());

                    let mut list = alloc.intersperse(
                        all_patterns.by_ref().take(num_before).chain([spread]),
                        comma_space.clone(),
                    );

                    if num_after > 0 {
                        let after = all_patterns;
                        list = alloc.intersperse([list].into_iter().chain(after), comma_space);
                    }

                    list
                }
            };
            alloc.concat([alloc.text("["), inner, alloc.text("]")])
        }
        Ctor(union, tag_id, args) => {
            match union.render_as {
                RenderAs::Guard => {
                    // #Guard <fake-condition-tag> <unexhausted-pattern>
                    debug_assert!(union.alternatives[tag_id.0 as usize]
                        .name
                        .is_tag(&TagName(GUARD_CTOR.into())));
                    debug_assert!(args.len() == 2);
                    let tag = pattern_to_doc_help(alloc, args[1].clone(), in_type_param);
                    alloc.concat([
                        tag,
                        alloc.text(AFTER_TAG_INDENT),
                        alloc.text("(note the lack of an "),
                        alloc.keyword("if"),
                        alloc.text(" clause)"),
                    ])
                }
                RenderAs::Record(field_names) => {
                    let mut arg_docs = Vec::with_capacity(args.len());

                    for (label, v) in field_names.into_iter().zip(args.into_iter()) {
                        match &v {
                            Anything => {
                                arg_docs.push(alloc.text(label.to_string()));
                            }
                            Literal(_) | Ctor(_, _, _) | List(..) => {
                                arg_docs.push(
                                    alloc
                                        .text(label.to_string())
                                        .append(alloc.reflow(": "))
                                        .append(pattern_to_doc_help(alloc, v, false)),
                                );
                            }
                        }
                    }

                    alloc
                        .text("{ ")
                        .append(alloc.intersperse(arg_docs, alloc.reflow(", ")))
                        .append(" }")
                }
                RenderAs::Tag | RenderAs::Opaque => {
                    let ctor = &union.alternatives[tag_id.0 as usize];
                    match &ctor.name {
                        CtorName::Tag(TagName(name)) if name.as_str() == NONEXHAUSIVE_CTOR => {
                            return pattern_to_doc_help(
                                alloc,
                                roc_exhaustive::Pattern::Anything,
                                in_type_param,
                            )
                        }
                        _ => {}
                    }

                    let tag_name = match (union.render_as, &ctor.name) {
                        (RenderAs::Tag, CtorName::Tag(tag)) => alloc.tag_name(tag.clone()),
                        (RenderAs::Opaque, CtorName::Opaque(opaque)) => {
                            alloc.wrapped_opaque_name(*opaque)
                        }
                        _ => unreachable!(),
                    };

                    let has_args = !args.is_empty();
                    let arg_docs = args
                        .into_iter()
                        .map(|v| pattern_to_doc_help(alloc, v, true));

                    // We assume the alternatives are sorted. If not, this assert will trigger
                    debug_assert!(tag_id == ctor.tag_id);

                    let docs = std::iter::once(tag_name).chain(arg_docs);

                    if in_type_param && has_args {
                        alloc
                            .text("(")
                            .append(alloc.intersperse(docs, alloc.space()))
                            .append(")")
                    } else {
                        alloc.intersperse(docs, alloc.space())
                    }
                }
            }
        }
    }
}
