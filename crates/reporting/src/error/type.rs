#![allow(clippy::too_many_arguments)]

use crate::error::canonicalize::{to_circular_def_doc, CIRCULAR_DEF};
use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder};
use itertools::EitherOrBoth;
use itertools::Itertools;
use roc_can::expected::{Expected, PExpected};
use roc_collections::all::{HumanIndex, MutSet, SendMap};
use roc_collections::VecMap;
use roc_error_macros::internal_error;
use roc_exhaustive::{CtorName, ListArity};
use roc_module::called_via::{BinOp, CalledVia};
use roc_module::ident::{IdentStr, Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_problem::Severity;
use roc_region::all::{LineInfo, Region};
use roc_solve_problem::{
    NotDerivableContext, NotDerivableEq, TypeError, UnderivableReason, Unfulfilled,
};
use roc_std::RocDec;
use roc_types::pretty_print::{Parens, WILDCARD};
use roc_types::types::{
    AbilitySet, AliasKind, Category, ErrorType, IndexOrField, PatternCategory, Polarity, Reason,
    RecordField, TypeExt,
};
use std::path::PathBuf;
use ven_pretty::{text, DocAllocator};

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

    let severity = problem.severity();

    let report =
        move |title: String, doc: RocDocBuilder<'b>, filename: PathBuf| -> Option<Report<'b>> {
            Some(Report {
                title,
                filename,
                doc,
                severity,
            })
        };

    match problem {
        BadExpr(region, category, found, expected) => Some(to_expr_report(
            alloc, lines, filename, severity, region, category, found, expected,
        )),
        BadPattern(region, category, found, expected) => Some(to_pattern_report(
            alloc, lines, filename, severity, region, category, found, expected,
        )),
        CircularType(region, symbol, overall_type) => Some(to_circular_report(
            alloc,
            lines,
            filename,
            severity,
            region,
            symbol,
            overall_type,
        )),
        UnexposedLookup(_, symbol) => {
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

            let doc = report_unfulfilled_ability(alloc, lines, incomplete, severity);

            report(title, doc, filename)
        }
        BadExprMissingAbility(region, _category, _found, incomplete) => {
            if region == roc_can::DERIVED_REGION {
                return None;
            }

            let incomplete = incomplete
                .into_iter()
                .map(|unfulfilled| report_unfulfilled_ability(alloc, lines, unfulfilled, severity));
            let note = alloc.stack(incomplete);
            let snippet = alloc.region(lines.convert_region(region), severity);
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
                severity,
            };
            Some(report)
        }
        BadPatternMissingAbility(region, _category, _found, incomplete) => {
            let incomplete = incomplete
                .into_iter()
                .map(|unfulfilled| report_unfulfilled_ability(alloc, lines, unfulfilled, severity));
            let note = alloc.stack(incomplete);
            let snippet = alloc.region(lines.convert_region(region), severity);
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
                severity,
            };
            Some(report)
        }
        Exhaustive(problem) => Some(exhaustive_problem(alloc, lines, filename, problem)),
        CircularDef(entries) => {
            let doc = to_circular_def_doc(alloc, lines, &entries, severity);
            let title = CIRCULAR_DEF.to_string();

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
                alloc.region(lines.convert_region(region), severity),
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
                severity,
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
                alloc.region(lines.convert_region(region), severity),
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
                severity,
            })
        }
        IngestedFileBadUtf8(file_path, utf8_err) => {
            let stack = [
                alloc.concat([
                    alloc.reflow("Failed to load "),
                    text!(alloc, "{:?}", file_path),
                    alloc.reflow(" as Str:"),
                ]),
                text!(alloc, "{}", utf8_err),
            ];
            Some(Report {
                title: "INVALID UTF-8".to_string(),
                filename,
                doc: alloc.stack(stack),
                severity,
            })
        }
        IngestedFileUnsupportedType(file_path, typ) => {
            let stack = [
                alloc.concat([
                    text!(alloc, "{:?}", file_path),
                    alloc.reflow(" is annotated to be a "),
                    alloc.inline_type_block(error_type_to_doc(alloc, typ)),
                    alloc.reflow("."),
                ]),
                alloc.concat([
                    alloc.reflow("Ingested files can only be of type "),
                    alloc.type_str("List U8"),
                    alloc.reflow(" or "),
                    alloc.type_str("Str"),
                    alloc.reflow("."),
                ]),
            ];
            Some(Report {
                title: "INVALID TYPE FOR INGESTED FILE".to_string(),
                filename,
                doc: alloc.stack(stack),
                severity,
            })
        }
    }
}

fn report_unfulfilled_ability<'a>(
    alloc: &'a RocDocAllocator<'a>,
    lines: &LineInfo,
    unfulfilled: Unfulfilled,
    severity: Severity,
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
                alloc.region(lines.convert_region(derive_region), severity),
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
                                alloc.keyword(roc_parse::keyword::IMPLEMENTS),
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
                alloc.reflow(". Consider adding an "),
                alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                alloc.reflow(" clause to bind the type variable, like "),
                alloc.inline_type_block(alloc.concat([
                    alloc.keyword(roc_parse::keyword::WHERE),
                    alloc.space(),
                    alloc.type_variable(v.clone()),
                    alloc.space(),
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.space(),
                    alloc.symbol_qualified(ability),
                ])),
            ])))
        }
        NotDerivableContext::DecodeOptionalRecordField(field) => {
            Some(alloc.note("").append(alloc.concat([
                alloc.reflow("I can't derive decoding for a record with an optional field, which in this case is "),
                alloc.record_field(field),
                alloc.reflow(". Default value record fields are polymorphic over records that may or may not contain them at compile time, "),
                alloc.reflow("but are not a concept that extends to runtime!"),
                alloc.hardline(),
                alloc.reflow("Maybe you wanted to use a "),
                alloc.symbol_unqualified(Symbol::RESULT_RESULT),
                alloc.reflow("?"),
            ])))
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
    severity: Severity,
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
            alloc.region(lines.convert_region(region), severity),
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
            alloc.region(lines.convert_region(region), severity),
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

fn report_mismatch<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    severity: Severity,
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
            severity,
        )
    } else {
        alloc.region(lines.convert_region(region), severity)
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
        severity,
    }
}

fn report_bad_type<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    severity: Severity,
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
            severity,
        )
    } else {
        alloc.region(lines.convert_region(region), severity)
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
        severity,
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
    severity: Severity,
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
                            severity,
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
                    alloc.region(lines.convert_region(expr_region), severity),
                    comparison,
                ]),
                severity,
            }
        }
        Expected::FromAnnotation(name, _arity, annotation_source, expected_type) => {
            use roc_can::pattern::Pattern;
            use roc_types::types::AnnotationSource::*;

            let is_suffixed = match &name.value {
                Pattern::Identifier(symbol) => symbol.as_str(alloc.interns).starts_with("#!"),
                _ => false,
            };

            let is_suffixed_stmt = match &name.value {
                Pattern::Identifier(symbol) => {
                    let ident = symbol.as_str(alloc.interns);
                    ident.starts_with("#!") && ident.ends_with("_stmt")
                }
                _ => false,
            };

            let (the_name_text, on_name_text) = match pattern_to_doc(alloc, &name.value) {
                _ if is_suffixed => (alloc.text("this suffixed"), alloc.nil()),
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
                    num_branches: 2,
                    ..
                } => alloc.concat([
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
                TypedBody { .. } if is_suffixed_stmt => alloc.concat([
                    alloc.text("body of "),
                    the_name_text,
                    alloc.text(" statement:"),
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
                TypedIfBranch { .. } => "This branch is".to_string(),
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
                    if is_suffixed_stmt {
                        // TODO: add a tip for using underscore
                        alloc.text(
                            "But a suffixed statement is expected to resolve to an empty record:",
                        )
                    } else {
                        alloc.concat([
                            alloc.text("But the type annotation"),
                            on_name_text,
                            alloc.text(" says it should be:"),
                        ])
                    },
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
                            severity,
                        )
                    },
                    comparison,
                ]),
                severity,
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
                    severity,
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
                    severity,
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
                    severity,
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
                    severity,
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
                    severity,
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
                severity,
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
                    severity,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.reflow("This list contains elements with different types:"),
                    alloc.string(format!("Its {ith} element is")),
                    alloc.reflow(prev_elems_msg),
                    Some(alloc.reflow("Every element in a list must have the same type!")),
                )
            }
            Reason::RecordUpdateValue(field) => report_mismatch(
                alloc,
                lines,
                filename,
                severity,
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
                                severity,
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
                                severity,
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
                        severity,
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
            Reason::FnCall {
                name,
                arity,
                called_via,
            } => match describe_wanted_function(&found) {
                DescribedFunction::NotAFunction(tag) => {
                    let this_value = match name {
                        None => alloc.text("This value"),
                        Some(symbol) => alloc.concat([
                            alloc.text("The "),
                            alloc.symbol_unqualified(symbol),
                            alloc.text(" value"),
                        ]),
                    };

                    use NotAFunctionTag::*;
                    let doc = match tag {
                        OpaqueNeedsUnwrap => alloc.stack([
                            alloc.concat([
                                this_value,
                                alloc.reflow(
                                    " is an opaque type, so it cannot be called with an argument:",
                                ),
                            ]),
                            alloc.region(lines.convert_region(expr_region), severity),
                            match called_via {
                                CalledVia::OldRecordBuilder => {
                                    alloc.hint("Did you mean to apply it to a function first?")
                                },
                                _ => {
                                    alloc.reflow("I can't call an opaque type because I don't know what it is! Maybe you meant to unwrap it first?")
                                }
                            }
                        ]),
                        Other => alloc.stack([
                            alloc.concat([
                                this_value,
                                alloc.string(format!(
                                    " is not a function, but it was given {}:",
                                    if arity == 1 {
                                        "1 argument".into()
                                    } else {
                                        format!("{arity} arguments")
                                    }
                                )),
                            ]),
                            alloc.region(lines.convert_region(expr_region), severity),
                            match called_via {
                                CalledVia::OldRecordBuilder => {
                                    alloc.concat([
                                        alloc.tip(),
                                        alloc.reflow("Remove "),
                                        alloc.keyword("<-"),
                                        alloc.reflow(" to assign the field directly.")
                                    ])
                                }
                                CalledVia::RecordBuilder => {
                                    alloc.concat([
                                        alloc.note(""),
                                        alloc.reflow("Record builders need a mapper function before the "),
                                        alloc.keyword("<-"),
                                        alloc.reflow(" to combine fields together with.")
                                    ])
                                }
                                _ => {
                                    alloc.reflow("Are there any missing commas? Or missing parentheses?")
                                }
                            }
                        ]),
                    };

                    Report {
                        filename,
                        title: "TOO MANY ARGS".to_string(),
                        doc,
                        severity,
                    }
                }
                DescribedFunction::Arguments(n) => {
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
                                        format!("{n} arguments")
                                    },
                                    arity
                                )),
                            ]),
                            alloc.region(lines.convert_region(expr_region), severity),
                            alloc.reflow("Are there any missing commas? Or missing parentheses?"),
                        ];

                        Report {
                            filename,
                            title: "TOO MANY ARGS".to_string(),
                            doc: alloc.stack(lines),
                            severity,
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
                                        format!("{n} arguments")
                                    },
                                    arity
                                )),
                            ]),
                            alloc.region(lines.convert_region(expr_region), severity),
                            alloc.reflow(
                                "Roc does not allow functions to be partially applied. \
                                Use a closure to make partial application explicit.",
                            ),
                        ];

                        Report {
                            filename,
                            title: "TOO FEW ARGS".to_string(),
                            doc: alloc.stack(lines),
                            severity,
                        }
                    }
                }
            },
            Reason::FnArg {
                name,
                arg_index,
                called_via,
            } => {
                let ith = arg_index.ordinal();

                let this_function = match (called_via, name) {
                    (CalledVia::Space, Some(symbole)) => alloc.symbol_unqualified(symbole),
                    (CalledVia::BinOp(op), _) => alloc.binop(op),
                    (CalledVia::UnaryOp(op), _) => alloc.unop(op),
                    (CalledVia::StringInterpolation, _) => alloc.text("this string interpolation"),
                    _ => alloc.text("this function"),
                };

                let argument = match called_via {
                    CalledVia::StringInterpolation => "argument".to_string(),
                    _ => format!("{ith} argument"),
                };

                report_mismatch(
                    alloc,
                    lines,
                    filename,
                    severity,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.concat([
                        alloc.string(format!("This {argument} to ")),
                        this_function.clone(),
                        alloc.text(" has an unexpected type:"),
                    ]),
                    alloc.text("The argument is"),
                    alloc.concat([
                        alloc.text("But "),
                        this_function,
                        alloc.string(format!(" needs its {argument} to be:")),
                    ]),
                    None,
                )
            }

            Reason::NumericLiteralSuffix => report_mismatch(
                alloc,
                lines,
                filename,
                severity,
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
                    severity,
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
                    severity,
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
                    severity,
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
                    severity,
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
                    alloc.region(lines.convert_region(region), severity),
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
                    severity,
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
                    alloc.region(lines.convert_region(region), severity),
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
                    severity,
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

enum DescribedFunction {
    Arguments(usize),
    NotAFunction(NotAFunctionTag),
}

enum NotAFunctionTag {
    OpaqueNeedsUnwrap,
    Other,
}

fn describe_wanted_function(tipe: &ErrorType) -> DescribedFunction {
    use ErrorType::*;

    match tipe {
        Function(args, _, _) => DescribedFunction::Arguments(args.len()),
        Alias(_, _, actual, AliasKind::Structural) => describe_wanted_function(actual),
        Alias(_, _, actual, AliasKind::Opaque) => {
            let tag = if matches!(
                describe_wanted_function(actual),
                DescribedFunction::Arguments(_)
            ) {
                NotAFunctionTag::OpaqueNeedsUnwrap
            } else {
                NotAFunctionTag::Other
            };

            DescribedFunction::NotAFunction(tag)
        }
        _ => DescribedFunction::NotAFunction(NotAFunctionTag::Other),
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
                text!(alloc, "{}his ", t),
                alloc.symbol_foreign_qualified(*name),
                alloc.text(" value"),
            ]),
            alloc.text(" is a:"),
        ),

        If => (
            alloc.concat([
                text!(alloc, "{}his ", t),
                alloc.keyword("if"),
                alloc.text(" expression"),
            ]),
            alloc.text(" produces:"),
        ),
        When => (
            alloc.concat([
                text!(alloc, "{}his ", t),
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
        IngestedFile(file_path) => (
            alloc.concat([this_is, text!(alloc, " an ingested file ({:?})", file_path)]),
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
                text!(alloc, "{}his ", t),
                alloc.opaque_name(*opaque),
                alloc.text(" opaque wrapping"),
            ]),
            alloc.text(" has the type:"),
        ),

        OpaqueArg => (
            alloc.concat([text!(alloc, "{}his argument to an opaque type", t)]),
            alloc.text(" has type:"),
        ),

        TagApply {
            tag_name: TagName(name),
            args_count: 0,
        } => (
            alloc.concat([
                text!(alloc, "{}his ", t),
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
                text!(alloc, "{}his ", t),
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
                text!(alloc, "{}his ", t),
                match field {
                    IndexOrField::Index(index) => alloc.tuple_field(*index),
                    IndexOrField::Field(field) => alloc.record_field(field.to_owned()),
                },
                alloc.text(" value"),
            ]),
            alloc.text(" is a:"),
        ),
        RecordAccess(field) => (
            alloc.concat([
                text!(alloc, "{}he value at ", t),
                alloc.record_field(field.to_owned()),
            ]),
            alloc.text(" is a:"),
        ),

        Tuple => (
            alloc.concat([this_is, alloc.text(" a tuple")]),
            alloc.text(" of type:"),
        ),

        TupleAccess(index) => (
            alloc.concat([text!(alloc, "{}he value at ", t), alloc.tuple_field(*index)]),
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
            text!(alloc, "{}his comparison", t),
            alloc.text(" produces:"),
        ),
        CallResult(Some(_), CalledVia::StringInterpolation) => (
            alloc.concat([this_is, alloc.text(" a string")]),
            alloc.text(" of type:"),
        ),
        CallResult(Some(symbol), _) => (
            alloc.concat([
                text!(alloc, "{}his ", t),
                alloc.symbol_foreign_qualified(*symbol),
                alloc.text(" call"),
            ]),
            alloc.text(" produces:"),
        ),
        CallResult(None, _) => (this_is, alloc.text(":")),
        LowLevelOpResult(op) => {
            panic!("Compiler bug: invalid return type from low-level op {op:?}");
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
    severity: Severity,
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
                alloc.region(lines.convert_region(expr_region), severity),
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
                severity,
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
                    alloc.region(lines.convert_region(region), severity),
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
                    severity,
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
                            severity,
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
                                severity,
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
                    severity,
                }
            }
            PReason::ListElem => {
                let doc = alloc.stack([
                    alloc.concat([alloc.reflow("This list element doesn't match the types of other elements in the pattern:")]),
                    alloc.region(lines.convert_region(region), severity),
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
                    severity,
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
        Tuple => alloc.reflow(" tuple values of type:"),
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
    severity: Severity,
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
                alloc.region(lines.convert_region(region), severity),
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
        severity,
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
                0, // zero fields omitted, since this isn't a diff
            )
        }

        Tuple(elems, ext) => {
            report_text::tuple(
                alloc,
                elems
                    .into_iter()
                    .map(|(_, value)| {
                        to_doc_help(ctx, gen_usages, alloc, Parens::Unnecessary, value)
                    })
                    .collect(),
                record_ext_to_doc(alloc, ext),
                0, // zero elems omitted, since this isn't a diff
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
                0, // zero tags omitted, since this isn't a diff
                None,
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

            let rec_doc = to_doc_help(ctx, gen_usages, alloc, Parens::Unnecessary, *rec_var);

            report_text::tag_union(
                alloc,
                tags.into_iter()
                    .map(|(k, v)| (alloc.tag_name(k), v))
                    .collect(),
                tag_ext_to_doc(alloc, pol, gen_usages, ext),
                0, // zero tags omitted, since this isn't a diff
                Some(rec_doc),
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
            Tuple(elems, ext) => {
                stack.extend(elems.iter().map(|(_, f)| (f, only_unseen)));
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
        if i == 0 {
            doc.push(alloc.space());
            doc.push(alloc.keyword(roc_parse::keyword::WHERE));
        } else {
            doc.push(alloc.string(",".to_string()));
        }
        doc.push(alloc.space());
        doc.push(alloc.type_variable(var));
        doc.push(alloc.space());
        doc.push(alloc.keyword(roc_parse::keyword::IMPLEMENTS));

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
                let arg_diff = diff_args(alloc, Parens::InFn, args1, args2);
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
            let args_diff = diff_args(alloc, Parens::InTypeParam, args1, args2);
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
            let args_diff = diff_args(alloc, Parens::InTypeParam, args1, args2);
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
            diff_tag_union(alloc, pol, tags1, ext1, None, tags2, ext2, None)
        }

        (RecursiveTagUnion(rec1, tags1, ext1, pol), RecursiveTagUnion(rec2, tags2, ext2, _)) => {
            diff_tag_union(alloc, pol, tags1, ext1, Some(*rec1), tags2, ext2, Some(*rec2))
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
                        &args.first(),
                        Some(ErrorType::Type(Symbol::NUM_INTEGER, _))
                            | Some(ErrorType::Alias(Symbol::NUM_INTEGER, _, _, _))
                    )
                }
                ErrorType::Alias(Symbol::NUM_NUM, args, _, _) => {
                    matches!(
                        &args.first(),
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
                        &args.first(),
                        Some(ErrorType::Type(Symbol::NUM_FLOATINGPOINT, _))
                            | Some(ErrorType::Alias(Symbol::NUM_FLOATINGPOINT, _, _, _))
                    )
                }

                ErrorType::Alias(Symbol::NUM_NUM, args, _, _) => {
                    matches!(
                        &args.first(),
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

fn diff_args<'b, I>(
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
    mut fields2: SendMap<Lowercase, RecordField<ErrorType>>,
    ext2: TypeExt,
) -> Diff<RocDocBuilder<'b>> {
    let to_overlap_docs =
        |(field, (t1, t2)): (Lowercase, (RecordField<ErrorType>, RecordField<ErrorType>))| {
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
                                Status::Different(vec![Problem::OptionalRequiredMismatch(field)])
                            }
                            Status::Different(mut problems) => {
                                problems.push(Problem::OptionalRequiredMismatch(field));

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
    let mut same_fields_different_types = VecMap::default();
    let mut fields_in_left_only = Vec::default();
    let mut same_fields_same_types = 0;

    for (k1, v1) in fields1.into_iter() {
        match fields2.remove(&k1) {
            Some(v2) if should_show_field_diff(&v1, &v2) => {
                // The field names are the same but the types are different
                // (or at least should be rendered as different)
                same_fields_different_types.insert(k1, (v1, v2));
            }
            Some(_) => {
                // They're both the same fields and the same types
                same_fields_same_types += 1;
            }
            None => {
                // Only fields1 has this field.
                fields_in_left_only.push((k1, v1));
            }
        }
    }

    // We've removed all the fields that they had in common, so the remaining entries in fields2
    // are ones that appear on the right only.
    let fields_in_right_only = fields2;

    let both = same_fields_different_types.into_iter().map(to_overlap_docs);
    let mut left = fields_in_left_only
        .iter()
        .map(|(k, v)| to_unknown_docs((k, v)))
        .peekable();
    let mut right = fields_in_right_only.iter().map(to_unknown_docs).peekable();

    let all_fields_shared = left.peek().is_none() && right.peek().is_none();

    let status = match (ext_has_fixed_fields(&ext1), ext_has_fixed_fields(&ext2)) {
        (true, true) => match left.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::FieldTypo(
                f.clone(),
                fields_in_right_only.keys().cloned().collect(),
            )]),
            None => {
                if right.peek().is_none() {
                    Status::Similar
                } else {
                    let result = Status::Different(vec![Problem::FieldsMissing(
                        right.map(|v| v.0).collect(),
                    )]);
                    // we just used the values in `right`.  in
                    right = fields_in_right_only.iter().map(to_unknown_docs).peekable();
                    result
                }
            }
        },
        (false, true) => match left.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::FieldTypo(
                f.clone(),
                fields_in_right_only.keys().cloned().collect(),
            )]),
            None => Status::Similar,
        },
        (true, false) => match right.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::FieldTypo(
                f.clone(),
                fields_in_left_only
                    .iter()
                    .map(|(field, _)| field.clone())
                    .collect(),
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
        same_fields_same_types,
    );
    let doc2 = report_text::record(
        alloc,
        fields_diff
            .right
            .into_iter()
            .map(|(_, b, c)| (b, c))
            .collect(),
        ext_diff.right,
        same_fields_same_types,
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

/// This is a helper for should_show_field_diff - see its doc comment for details.
fn should_show_diff(t1: &ErrorType, t2: &ErrorType) -> bool {
    use ErrorType::*;

    match (t1, t2) {
        (Type(sym1, types1), Type(sym2, types2)) => {
            if sym1 != sym2 || types1.len() != types2.len() {
                return true;
            }

            types1
                .iter()
                .zip(types2.iter())
                .any(|(t1, t2)| should_show_diff(t1, t2))
        }
        (Infinite, Infinite) | (Error, Error) => false,
        (RigidVar(v1), RigidVar(v2)) => v1 != v2,
        (FlexVar(_), _) | (_, FlexVar(_)) => {
            // If either is flex, it will unify to the other type; no diff is needed.
            false
        }
        (FlexAbleVar(v1, _set1), FlexAbleVar(v2, _set2))
        | (RigidAbleVar(v1, _set1), RigidAbleVar(v2, _set2)) => {
            #[cfg(debug_assertions)]
            {
                if v1 == v2 {
                    // If v1 == v2, then the sets should be equal too!
                    debug_assert_eq!(_set1.len(), _set2.len());
                    debug_assert!(_set1
                        .sorted_iter()
                        .zip(_set2.sorted_iter())
                        .all(|(t1, t2)| t1 == t2));
                }
            }

            v1 != v2
        }
        (Record(fields1, ext1), Record(fields2, ext2)) => {
            let is_1_open = matches!(ext1, TypeExt::FlexOpen(_));
            let is_2_open = matches!(ext2, TypeExt::FlexOpen(_));

            if !is_1_open && !is_2_open && fields1.len() != fields2.len() {
                return true;
            }

            // Check for diffs in any of the fields1 fields.
            for (name, f1) in fields1.iter() {
                match fields2.get(name) {
                    Some(f2) => {
                        // If the field is on both records, and the diff should be
                        // shown, then we should show a diff for the whole record.
                        if should_show_field_diff(f1, f2) {
                            return true;
                        }

                        // (If the field is on both records, but no diff should be
                        // shown between those fields, continue checking other fields.)
                    }
                    None => {
                        // It's fine for 1 to have a field that the other doesn't have,
                        // so long as the other one is open.
                        if !is_2_open {
                            return true;
                        }
                    }
                }
            }

            // At this point we've checked all the fields that are in both records,
            // as well as all the fields that are in 1 but not 2.
            // All that remains is to check the fields that are in 2 but not 1,
            // which we don't care about if 1 is open (because then it's fine
            // for the other record to have fields it doesn't).
            if !is_1_open {
                for name in fields2.keys() {
                    if !fields1.contains_key(name) {
                        // fields1 is missing this field, and fields1 is not open,
                        // therefore this is a relevant diff.
                        return true;
                    }
                }
            }

            // We haven't early-returned true yet, so we didn't find any relevant diffs!
            false
        }
        (Tuple(elems1, ext1), Tuple(elems2, ext2)) => {
            if elems1.len() != elems2.len() || ext1 != ext2 {
                return true;
            }

            elems1
                .iter()
                .zip(elems2.iter())
                .any(|((i1, e1), (i2, e2))| i1 != i2 || should_show_diff(e1, e2))
        }
        (TagUnion(tags1, ext1, polarity1), TagUnion(tags2, ext2, polarity2)) => {
            debug_assert_eq!(
                polarity1, polarity2,
                "Any two tag unions we're comparing should have the same polarity!"
            );

            if tags1.len() != tags2.len() || ext1 != ext2 {
                return true;
            }

            tags1
                .iter()
                .zip(tags2.iter())
                .any(|((name1, payload1), (name2, payload2))| {
                    if name1 != name2 || payload1.len() != payload2.len() {
                        return true;
                    }

                    payload1
                        .iter()
                        .zip(payload2.iter())
                        .any(|(p1, p2)| should_show_diff(p1, p2))
                })
        }
        (
            RecursiveTagUnion(rec1, tags1, ext1, _polarity1),
            RecursiveTagUnion(rec2, tags2, ext2, _polarity2),
        ) => {
            // If two tag unions differ only in polarity, don't show that as a diff;
            // polarity is invisible to the reader!

            if tags1.len() != tags2.len() || ext1 != ext2 || should_show_diff(rec1, rec2) {
                return true;
            }

            tags1
                .iter()
                .zip(tags2.iter())
                .any(|((name1, payload1), (name2, payload2))| {
                    if name1 != name2 || payload1.len() != payload2.len() {
                        return true;
                    }

                    payload1
                        .iter()
                        .zip(payload2.iter())
                        .any(|(p1, p2)| should_show_diff(p1, p2))
                })
        }
        (Function(params1, ret1, l1), Function(params2, ret2, l2)) => {
            if params1.len() != params2.len()
                || should_show_diff(ret1, ret2)
                || should_show_diff(l1, l2)
            {
                return true;
            }

            params1
                .iter()
                .zip(params2.iter())
                .any(|(p1, p2)| should_show_diff(p1, p2))
        }
        (Alias(sym1, params1, t1, kind1), Alias(sym2, params2, t2, kind2)) => {
            if sym1 != sym2
                || kind1 != kind2
                || params1.len() != params2.len()
                || should_show_diff(t1, t2)
            {
                return true;
            }

            params1
                .iter()
                .zip(params2.iter())
                .any(|(p1, p2)| should_show_diff(p1, p2))
        }
        (Range(types1), Range(types2)) => {
            if types1.len() != types2.len() {
                return true;
            }

            types1
                .iter()
                .zip(types2.iter())
                .any(|(t1, t2)| should_show_diff(t1, t2))
        }
        (Alias(_sym, _params, aliased, AliasKind::Structural), other)
        | (other, Alias(_sym, _params, aliased, AliasKind::Structural)) => {
            // Check to see if we should show the diff after unwrapping the alias
            should_show_diff(aliased, other)
        }
        (Alias(_, _, _, AliasKind::Opaque), _)
        | (_, Alias(_, _, _, AliasKind::Opaque))
        | (Infinite, _)
        | (_, Infinite)
        | (Error, _)
        | (_, Error)
        | (Type(_, _), _)
        | (_, Type(_, _))
        | (RigidVar(_), _)
        | (_, RigidVar(_))
        | (FlexAbleVar(_, _), _)
        | (_, FlexAbleVar(_, _))
        | (RigidAbleVar(_, _), _)
        | (_, RigidAbleVar(_, _))
        | (Record(_, _), _)
        | (_, Record(_, _))
        | (Tuple(_, _), _)
        | (_, Tuple(_, _))
        | (TagUnion(_, _, _), _)
        | (_, TagUnion(_, _, _))
        | (RecursiveTagUnion(_, _, _, _), _)
        | (_, RecursiveTagUnion(_, _, _, _))
        | (Function(_, _, _), _)
        | (_, Function(_, _, _)) => true,
    }
}

/// If these are equivalent, we shouldn't bother showing them in a diff.
/// (For example, if one is Required and the other is Demanded, showing
/// them in a diff will be unhelpful; they'll both be rendered using : and will look
/// exactly the same to the reader!)
fn should_show_field_diff(
    field1: &RecordField<ErrorType>,
    field2: &RecordField<ErrorType>,
) -> bool {
    use RecordField::*;

    match (field1, field2) {
        // If they're both the same, they don't need a diff
        (Demanded(t1), Demanded(t2))
        | (Required(t1), Required(t2))
        | (RigidRequired(t1), RigidRequired(t2))
        | (RigidOptional(t1), RigidOptional(t2))
        | (Optional(t1), Optional(t2))
        // Demanded and Required don't need a diff
        | (Demanded(t1), Required(t2))
        | (Required(t1), Demanded(t2))
        // Demanded and RigidRequired don't need a diff
        | (Demanded(t1), RigidRequired(t2))
        | (RigidRequired(t1), Demanded(t2))
        => should_show_diff(t1, t2),
        // Everything else needs a diff
        (Demanded(_), Optional(_))
        | (Demanded(_), RigidOptional(_))
        | (Required(_), RigidRequired(_))
        | (Required(_), Optional(_))
        | (Optional(_), Demanded(_))
        | (Optional(_), RigidRequired(_))
        | (Optional(_), RigidOptional(_))
        | (Optional(_), Required(_))
        | (RigidRequired(_), Required(_))
        | (RigidRequired(_), Optional(_))
        | (RigidRequired(_), RigidOptional(_))
        | (Required(_), RigidOptional(_))
        | (RigidOptional(_), Demanded(_))
        | (RigidOptional(_), Required(_))
        | (RigidOptional(_), Optional(_))
        | (RigidOptional(_), RigidRequired(_)) => true,
    }
}

fn same_tag_name_overlap_diff<'b>(
    alloc: &'b RocDocAllocator<'b>,
    field: TagName,
    payload_vals1: Vec<ErrorType>,
    payload_vals2: Vec<ErrorType>,
) -> Diff<(TagName, RocDocBuilder<'b>, Vec<RocDocBuilder<'b>>)> {
    // Render ellipses wherever the payload slots have the same type.
    let mut left_doc = Vec::with_capacity(payload_vals1.len());
    let mut left_able = Vec::new();
    let mut right_doc = Vec::with_capacity(payload_vals2.len());
    let mut right_able = Vec::new();

    // itertools::zip_longest is a zip that can continue past the end of one Vec.
    // If they both have payload values in a given slot, and both are the same type,
    // we render ellipsis instead of the actual type - since there's no diff between them.
    // If one of them doesn't have a payload value in that slot, we always render its type.
    for either_or_both in payload_vals1
        .into_iter()
        .zip_longest(payload_vals2.into_iter())
    {
        match either_or_both {
            // Both tag unions have a payload value in this slot
            EitherOrBoth::Both(t1, t2) => {
                if should_show_diff(&t1, &t2) {
                    {
                        let (doc, able) = to_doc(alloc, Parens::InTypeParam, t1);
                        left_doc.push(doc);
                        left_able.extend(able);
                    }

                    {
                        let (doc, able) = to_doc(alloc, Parens::InTypeParam, t2);
                        right_doc.push(doc);
                        right_able.extend(able);
                    }
                } else {
                    left_doc.push(alloc.ellipsis());
                    right_doc.push(alloc.ellipsis());
                }
            }
            // Only the left tag union has a payload value in this slot
            EitherOrBoth::Left(t1) => {
                let (doc, able) = to_doc(alloc, Parens::InTypeParam, t1);
                left_doc.push(doc);
                left_able.extend(able);
            }
            // Only the right tag union has a payload value in this slot
            EitherOrBoth::Right(t2) => {
                let (doc, able) = to_doc(alloc, Parens::InTypeParam, t2);
                right_doc.push(doc);
                right_able.extend(able);
            }
        }
    }

    Diff {
        left: (field.clone(), alloc.tag_name(field.clone()), left_doc),
        right: (field.clone(), alloc.tag_name(field), right_doc),
        status: Status::Similar,
        left_able,
        right_able,
    }
}

fn diff_tag_union<'b>(
    alloc: &'b RocDocAllocator<'b>,
    pol: Polarity,
    tags1: SendMap<TagName, Vec<ErrorType>>,
    ext1: TypeExt,
    rec1: Option<ErrorType>,
    mut tags2: SendMap<TagName, Vec<ErrorType>>,
    ext2: TypeExt,
    rec2: Option<ErrorType>,
) -> Diff<RocDocBuilder<'b>> {
    let gen_usages1 = {
        let mut usages = VecMap::default();
        count_generated_name_usages(&mut usages, tags1.values().flatten());
        count_generated_name_usages_in_exts(&mut usages, [(&ext1, false)]);
        usages
    };
    let gen_usages2 = {
        let mut usages = VecMap::default();
        count_generated_name_usages(&mut usages, tags2.values().flatten());
        count_generated_name_usages_in_exts(&mut usages, [(&ext2, false)]);
        usages
    };

    let to_overlap_docs = |(tag_name, (t1, t2)): (TagName, (Vec<ErrorType>, Vec<ErrorType>))| {
        same_tag_name_overlap_diff(alloc, tag_name, t1, t2)
    };
    let to_unknown_docs = |(tag_name, args): (&TagName, &Vec<ErrorType>)| -> (
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
            tag_name.clone(),
            alloc.tag_name(tag_name.clone()),
            args,
            able.into_iter().flatten().collect(),
        )
    };
    let mut same_tags_different_payloads = VecMap::default();
    let mut tags_in_left_only = Vec::default();
    let mut same_tags_same_payloads = 0;

    for (k1, v1) in tags1.into_iter() {
        match tags2.remove(&k1) {
            Some(v2) if should_show_payload_diff(&v1, &v2) => {
                // The tag names are the same but the payload types are different
                // (or at least should be rendered as different)
                same_tags_different_payloads.insert(k1.clone(), (v1.clone(), v2));
            }
            Some(_) => {
                // They both have the same tag name as well as the same payload types
                same_tags_same_payloads += 1;
            }
            None => {
                // Only tags1 has this tag.
                tags_in_left_only.push((k1, v1));
            }
        }
    }

    // We've removed all the tags that they had in common, so the remaining entries in tags2
    // are ones that appear on the right only.
    let tags_in_right_only = tags2;
    let no_tags_in_common = same_tags_different_payloads.is_empty() && same_tags_same_payloads == 0;
    let both = same_tags_different_payloads
        .into_iter()
        .map(to_overlap_docs);

    let any_tags_on_one_side_only = !tags_in_left_only.is_empty() || !tags_in_right_only.is_empty();

    let mut left = tags_in_left_only
        .iter()
        .map(|(k, v)| to_unknown_docs((k, v)))
        .peekable();
    let mut right = tags_in_right_only.iter().map(to_unknown_docs).peekable();

    let status = match (ext_has_fixed_fields(&ext1), ext_has_fixed_fields(&ext2)) {
        (false, false) => Status::Similar,
        _ => match (left.peek(), right.peek()) {
            // At least one tag appeared only on the left, and also
            // at least one tag appeared only on the right. There's a chance this is
            // because of a typo, so we'll suggest that as a hint.
            (Some((f, _, _, _)), Some(_)) => Status::Different(vec![Problem::TagTypo(
                f.clone(),
                tags_in_right_only.keys().cloned().collect(),
            )]),
            // At least one tag appeared only on the left, but all of the tags
            // on the right also appeared on the left. So at least one tag is missing.
            (Some(_), None) => Status::Different(vec![Problem::TagsMissing(
                left.clone().map(|v| v.0).collect(),
            )]),
            // At least one tag appeared only on the right, but all of the tags
            // on the left also appeared on the right. So at least one tag is missing.
            (None, Some(_)) => {
                let status =
                    Status::Different(vec![Problem::TagsMissing(right.map(|v| v.0).collect())]);
                right = tags_in_right_only.iter().map(to_unknown_docs).peekable();
                status
            }
            // Left and right have the same set of tag names (but may have different payloads).
            (None, None) => Status::Similar,
        },
    };

    let ext1_is_open = matches!(&ext1, TypeExt::FlexOpen(_));
    let ext2_is_open = matches!(&ext2, TypeExt::FlexOpen(_));
    let ext_diff = tag_ext_to_diff(alloc, pol, ext1, ext2, &gen_usages1, &gen_usages2);

    let mut tags_diff: Diff<Vec<(TagName, RocDocBuilder<'b>, Vec<RocDocBuilder<'b>>)>> = Diff {
        status: Status::Similar,
        left: Vec::new(),
        right: Vec::new(),
        left_able: Vec::new(),
        right_able: Vec::new(),
    };

    for diff in both {
        tags_diff.status.merge(diff.status);
        tags_diff.left.push(diff.left);
        tags_diff.right.push(diff.right);
        tags_diff.left_able.extend(diff.left_able);
        tags_diff.right_able.extend(diff.right_able);
    }

    let left_tags_omitted;
    let right_tags_omitted;

    if no_tags_in_common {
        // If they have no tags in common, we shouldn't omit any tags,
        // because that would result in an unhelpful diff of
        // […] on one side and another […] on the other side!

        left_tags_omitted = 0;
        right_tags_omitted = 0;

        for (tag, tag_doc, payload_vals, able) in left {
            tags_diff.left.push((tag, tag_doc, payload_vals));
            tags_diff.left_able.extend(able);
        }

        for (tag, tag_doc, payload_vals, able) in right {
            tags_diff.right.push((tag, tag_doc, payload_vals));
            tags_diff.right_able.extend(able);
        }

        tags_diff.status.merge(Status::Different(Vec::new()));
    } else if any_tags_on_one_side_only {
        // If either tag union is open but the other is not, then omit the tags in the other.
        //
        // In other words, if one tag union is a pattern match which has _ ->,
        // don't list the tags which fall under that catch-all pattern because
        // they won't be helpful. By omitting them, we'll only show the tags that
        // are actually matched.
        //
        // We shouldn't do this if they're both open though,
        // because that would result in an unhelpful diff of
        // […] on one side and another […] on the other side!
        if ext2_is_open && !ext1_is_open {
            left_tags_omitted = same_tags_same_payloads + left.len();
        } else {
            left_tags_omitted = same_tags_same_payloads;

            for (tag, tag_doc, args, able) in left {
                tags_diff.left.push((tag, tag_doc, args));
                tags_diff.left_able.extend(able);
            }
        }

        if ext1_is_open && !ext2_is_open {
            right_tags_omitted = same_tags_same_payloads + right.len();
        } else {
            right_tags_omitted = same_tags_same_payloads;

            for (tag, tag_doc, args, able) in right {
                tags_diff.right.push((tag, tag_doc, args));
                tags_diff.right_able.extend(able);
            }
        }

        tags_diff.status.merge(Status::Different(Vec::new()));
    } else {
        left_tags_omitted = same_tags_same_payloads;
        right_tags_omitted = same_tags_same_payloads;
    }

    tags_diff.left.sort_by(|a, b| a.0.cmp(&b.0));
    tags_diff.right.sort_by(|a, b| a.0.cmp(&b.0));

    let lefts = tags_diff.left.into_iter().map(|(_, a, b)| (a, b)).collect();
    let rights = tags_diff
        .right
        .into_iter()
        .map(|(_, a, b)| (a, b))
        .collect();

    let doc1 = match rec1 {
        None => report_text::tag_union(alloc, lefts, ext_diff.left, left_tags_omitted, None),
        Some(rec) => {
            let (rec_doc, able) = to_doc(alloc, Parens::Unnecessary, rec);

            tags_diff.left_able.extend(able);

            report_text::tag_union(
                alloc,
                lefts,
                ext_diff.left,
                left_tags_omitted,
                Some(rec_doc),
            )
        }
    };
    let doc2 = match rec2 {
        None => report_text::tag_union(alloc, rights, ext_diff.right, right_tags_omitted, None),
        Some(rec) => {
            let (rec_doc, able) = to_doc(alloc, Parens::Unnecessary, rec);

            tags_diff.right_able.extend(able);

            report_text::tag_union(
                alloc,
                rights,
                ext_diff.right,
                right_tags_omitted,
                Some(rec_doc),
            )
        }
    };

    tags_diff.status.merge(status);

    Diff {
        left: doc1,
        right: doc2,
        status: tags_diff.status,
        left_able: tags_diff.left_able,
        right_able: tags_diff.right_able,
    }
}

fn should_show_payload_diff(errs1: &[ErrorType], errs2: &[ErrorType]) -> bool {
    if errs1.len() == errs2.len() {
        errs1
            .iter()
            .zip(errs2.iter())
            .any(|(err1, err2)| should_show_diff(err1, err2))
    } else {
        true
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
            left_able: Vec::new(),
            right_able: Vec::new(),
        },
        Status::Different(_) => Diff {
            left: ext_doc_1,
            right: ext_doc_2,
            status,
            left_able: Vec::new(),
            right_able: Vec::new(),
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
        fields_omitted: usize,
    ) -> RocDocBuilder<'b> {
        let ext_doc = if let Some(t) = opt_ext {
            t
        } else {
            alloc.nil()
        };

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

        if entries.is_empty() {
            if fields_omitted == 0 {
                alloc.text("{}")
            } else {
                alloc
                    .text("{ ")
                    .append(alloc.ellipsis().append(alloc.text(" }")))
            }
            .append(ext_doc)
        } else if entries.len() == 1 {
            // Single-field records get printed on one line; multi-field records get multiple lines
            alloc
                .text("{ ")
                .append(entry_to_doc(entries.into_iter().next().unwrap()))
                .append(if fields_omitted == 0 {
                    alloc.text("")
                } else {
                    alloc.text(", ").append(alloc.ellipsis())
                })
                .append(alloc.text(" }"))
                .append(ext_doc)
        } else {
            let ending = if fields_omitted == 0 {
                alloc.reflow("}")
            } else {
                alloc.vcat([
                    alloc.ellipsis().indent(super::RECORD_FIELD_INDENT),
                    alloc.reflow("}"),
                ])
            };

            // Multi-field records get printed on multiple lines, as do records with fields omitted.
            alloc
                .vcat(
                    std::iter::once(alloc.reflow("{")).chain(
                        entries
                            .into_iter()
                            .map(|entry| {
                                entry_to_doc(entry)
                                    .indent(super::RECORD_FIELD_INDENT)
                                    .append(alloc.reflow(","))
                            })
                            .chain(std::iter::once(ending)),
                    ),
                )
                .append(ext_doc)
        }
    }

    pub fn tuple<'b>(
        alloc: &'b RocDocAllocator<'b>,
        entries: Vec<RocDocBuilder<'b>>,
        opt_ext: Option<RocDocBuilder<'b>>,
        fields_omitted: usize,
    ) -> RocDocBuilder<'b> {
        let ext_doc = if let Some(t) = opt_ext {
            t
        } else {
            alloc.nil()
        };

        if entries.is_empty() {
            if fields_omitted == 0 {
                alloc.text("()")
            } else {
                alloc
                    .text("(")
                    .append(alloc.ellipsis().append(alloc.text(")")))
            }
            .append(ext_doc)
        } else if entries.len() == 1 {
            // Single-field records get printed on one line; multi-field records get multiple lines
            alloc
                .text("(")
                .append(entries.into_iter().next().unwrap())
                .append(if fields_omitted == 0 {
                    alloc.text("")
                } else {
                    alloc.text(", ").append(alloc.ellipsis())
                })
                .append(alloc.text(")"))
                .append(ext_doc)
        } else {
            let ending = if fields_omitted == 0 {
                alloc.reflow(")")
            } else {
                alloc.vcat([
                    alloc.ellipsis().indent(super::RECORD_FIELD_INDENT),
                    alloc.reflow(")"),
                ])
            }
            .append(ext_doc);

            // Multi-elem tuples get printed on multiple lines
            alloc.vcat(
                std::iter::once(alloc.reflow("(")).chain(
                    entries
                        .into_iter()
                        .map(|entry| {
                            entry
                                .indent(super::RECORD_FIELD_INDENT)
                                .append(alloc.reflow(","))
                        })
                        .chain(std::iter::once(ending)),
                ),
            )
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
        tags_omitted: usize,
        opt_rec: Option<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        let ext_doc = if let Some(t) = opt_ext {
            t
        } else {
            alloc.nil()
        };

        let entry_to_doc = |(tag_name, arguments): (RocDocBuilder<'b>, Vec<_>)| {
            if arguments.is_empty() {
                tag_name
            } else {
                tag_name
                    .append(alloc.space())
                    .append(alloc.intersperse(arguments, alloc.space()))
            }
        };

        let without_rec = if entries.is_empty() {
            if tags_omitted == 0 {
                alloc.text("[]")
            } else {
                alloc
                    .text("[")
                    .append(alloc.ellipsis().append(alloc.text("]")))
            }
            .append(ext_doc)
        } else if entries.len() == 1 {
            // Single-tag unions get printed on one line; multi-tag unions get multiple lines
            alloc
                .text("[")
                .append(entry_to_doc(entries.into_iter().next().unwrap()))
                .append(if tags_omitted == 0 {
                    alloc.text("")
                } else {
                    alloc.text(", ").append(alloc.ellipsis())
                })
                .append(alloc.text("]"))
                .append(ext_doc)
        } else {
            let ending = if tags_omitted == 0 {
                alloc.reflow("]")
            } else {
                alloc.vcat([
                    alloc.ellipsis().indent(super::TAG_INDENT),
                    alloc.reflow("]"),
                ])
            };

            // Multi-tag unions get printed on multiple lines, as do tag unions with tags omitted.
            alloc
                .vcat(
                    std::iter::once(alloc.reflow("[")).chain(
                        entries
                            .into_iter()
                            .map(|entry| {
                                entry_to_doc(entry)
                                    .indent(super::TAG_INDENT)
                                    .append(alloc.reflow(","))
                            })
                            .chain(std::iter::once(ending)),
                    ),
                )
                .append(ext_doc)
        };

        match opt_rec {
            Some(rec) => without_rec.append(alloc.text(" as ")).append(rec),
            None => without_rec,
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

            match suggestions.first() {
                None => alloc.nil(),
                Some(nearest) => {
                    let typo_str = format!("{typo}");
                    let nearest_str = format!("{nearest}");

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

            match suggestions.first() {
                None => alloc.nil(),
                Some(nearest) => {
                    let nearest_str = format!("{nearest}");

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
                        .append(alloc.reflow(" says it can take on any value that implements the "))
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
                            .append(
                                alloc.reflow(" can take on any value that implements only the "),
                            )
                            .append(list_abilities(alloc, &abilities))
                            .append(alloc.reflow(".")),
                        alloc.concat([
                            alloc.reflow("But, I see that it's also used as if it implements the "),
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
                            alloc.keyword(roc_parse::keyword::IMPLEMENTS),
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
                Tuple(_, _) => rigid_able_vs_concrete(x, alloc.reflow("a tuple value")),
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
                Tuple(_, _) => bad_rigid_var(x, alloc.reflow("a tuple value")),
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

        (IntFloat, _) => {
            alloc.tip().append(alloc.concat(
                [
                    alloc.reflow(
                        "You can convert between integers and fractions using functions like ",
                    ),
                    alloc.symbol_qualified(Symbol::NUM_TO_FRAC),
                    alloc.reflow(" and "),
                    alloc.symbol_qualified(Symbol::NUM_ROUND),
                    alloc.reflow("."),
                ],
            ))
        }

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

fn report_record_field_typo<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    severity: Severity,
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
        alloc.region(lines.convert_region(field_region), severity),
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
            let f_doc = text!(alloc, "{}{}{}", field_prefix, field, field_suffix)
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
                    text!(alloc, "{}{}{}", field_prefix, f.0, field_suffix)
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
        severity,
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

    let severity = problem.severity();

    match problem {
        Incomplete(region, context, missing) => match context {
            BadArg => {
                let doc = alloc.stack([
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(lines.convert_region(region), severity),
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
                    severity,
                }
            }
            BadDestruct => {
                let doc = alloc.stack([
                    alloc.reflow("This pattern does not cover all the possibilities:"),
                    alloc.region(lines.convert_region(region), severity),
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
                    severity,
                }
            }
            BadCase => {
                let doc = alloc.stack([
                    alloc.concat([
                        alloc.reflow("This "),
                        alloc.keyword("when"),
                        alloc.reflow(" does not cover all the possibilities:"),
                    ]),
                    alloc.region(lines.convert_region(region), severity),
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
                    severity,
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
                    severity,
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
                severity,
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
                    severity,
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
                severity,
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
const TAG_INDENT: usize = 4;
const RECORD_FIELD_INDENT: usize = 4;

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
                RenderAs::Tuple => {
                    let mut arg_docs = Vec::with_capacity(args.len());

                    for v in args.into_iter() {
                        arg_docs.push(pattern_to_doc_help(alloc, v, false));
                    }

                    alloc
                        .text("( ")
                        .append(alloc.intersperse(arg_docs, alloc.reflow(", ")))
                        .append(" )")
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
