use roc_can::expected::{Expected, PExpected};
use roc_collections::all::{Index, MutSet, SendMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_solve::solve;
use roc_types::pretty_print::Parens;
use roc_types::types::{Category, ErrorType, PatternCategory, Reason, TypeExt};
use std::path::PathBuf;

use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder};
use ven_pretty::DocAllocator;

const ADD_ANNOTATIONS: &str = r#"Can more type annotations be added? Type annotations always help me give more specific messages, and I think they could help a lot in this case"#;

pub fn type_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    problem: solve::TypeError,
) -> Report<'b> {
    use solve::TypeError::*;

    match problem {
        BadExpr(region, category, found, expected) => {
            to_expr_report(alloc, filename, region, category, found, expected)
        }
        BadPattern(region, category, found, expected) => {
            to_pattern_report(alloc, filename, region, category, found, expected)
        }
        CircularType(region, symbol, overall_type) => {
            to_circular_report(alloc, filename, region, symbol, overall_type)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn report_mismatch<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    category: &Category,
    found: ErrorType,
    expected_type: ErrorType,
    region: roc_region::all::Region,
    _opt_highlight: Option<roc_region::all::Region>,
    problem: RocDocBuilder<'b>,
    this_is: RocDocBuilder<'b>,
    instead_of: RocDocBuilder<'b>,
    further_details: Option<RocDocBuilder<'b>>,
) -> Report<'b> {
    let lines = vec![
        problem,
        alloc.region(region),
        type_comparison(
            alloc,
            found,
            expected_type,
            add_category(alloc, this_is, category),
            instead_of,
            further_details,
        ),
    ];

    Report {
        title: "TYPE MISMATCH".to_string(),
        filename,
        doc: alloc.stack(lines),
    }
}

#[allow(clippy::too_many_arguments)]
fn report_bad_type<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    category: &Category,
    found: ErrorType,
    expected_type: ErrorType,
    region: roc_region::all::Region,
    _opt_highlight: Option<roc_region::all::Region>,
    problem: RocDocBuilder<'b>,
    this_is: RocDocBuilder<'b>,
    further_details: RocDocBuilder<'b>,
) -> Report<'b> {
    let lines = vec![
        problem,
        alloc.region(region),
        lone_type(
            alloc,
            found,
            expected_type,
            add_category(alloc, this_is, &category),
            further_details,
        ),
    ];

    Report {
        title: "TYPE MISMATCH".to_string(),
        filename,
        doc: alloc.stack(lines),
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

fn to_expr_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    expr_region: roc_region::all::Region,
    category: Category,
    found: ErrorType,
    expected: Expected<ErrorType>,
) -> Report<'b> {
    match expected {
        Expected::NoExpectation(expected_type) => {
            let comparison = type_comparison(
                alloc,
                found,
                expected_type,
                add_category(alloc, alloc.text("It is"), &category),
                alloc.text("But you are trying to use it as:"),
                None,
            );

            Report {
                filename,
                title: "TYPE MISMATCH".to_string(),
                doc: alloc.stack(vec![
                    alloc.text("This expression is used in an unexpected way:"),
                    alloc.region(expr_region),
                    comparison,
                ]),
            }
        }
        Expected::FromAnnotation(name, _arity, annotation_source, expected_type) => {
            use roc_types::types::AnnotationSource::*;

            let (the_name_text, on_name_text) = match pattern_to_doc(alloc, &name.value) {
                Some(doc) => (
                    alloc.concat(vec![alloc.reflow("the "), doc.clone()]),
                    alloc.concat(vec![alloc.reflow(" on "), doc]),
                ),
                None => (alloc.text("this"), alloc.nil()),
            };

            let thing = match annotation_source {
                TypedIfBranch {
                    index,
                    num_branches,
                } if num_branches == 2 => alloc.concat(vec![
                    alloc.keyword(if index == Index::FIRST {
                        "then"
                    } else {
                        "else"
                    }),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("if"),
                    alloc.text(" expression:"),
                ]),
                TypedIfBranch { index, .. } => alloc.concat(vec![
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("if"),
                    alloc.text(" expression:"),
                ]),
                TypedWhenBranch(index) => alloc.concat(vec![
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("when"),
                    alloc.text(" expression:"),
                ]),
                TypedBody => alloc.concat(vec![
                    alloc.text("body of "),
                    the_name_text,
                    alloc.text(" definition:"),
                ]),
            };

            let it_is = match annotation_source {
                TypedIfBranch { index, .. } => format!("The {} branch is", index.ordinal()),
                TypedWhenBranch(index) => format!("The {} branch is", index.ordinal()),
                TypedBody => "The body is".into(),
            };

            let comparison = type_comparison(
                alloc,
                found,
                expected_type,
                add_category(alloc, alloc.text(it_is), &category),
                alloc.concat(vec![
                    alloc.text("But the type annotation"),
                    on_name_text,
                    alloc.text(" says it should be:"),
                ]),
                None,
            );

            Report {
                title: "TYPE MISMATCH".to_string(),
                filename,
                doc: alloc.stack(vec![
                    alloc.text("Something is off with the ").append(thing),
                    alloc.region(expr_region),
                    comparison,
                ]),
            }
        }
        Expected::ForReason(reason, expected_type, region) => match reason {
            Reason::IfCondition => {
                let problem = alloc.concat(vec![
                    alloc.text("This "),
                    alloc.keyword("if"),
                    alloc.text(" condition needs to be a "),
                    alloc.type_str("Bool"),
                    alloc.text(":"),
                ]);

                report_bad_type(
                    alloc,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    alloc.text("Right now it’s"),
                    alloc.concat(vec![
                        alloc.reflow("But I need every "),
                        alloc.keyword("if"),
                        alloc.reflow(" condition to evaluate to a "),
                        alloc.type_str("Bool"),
                        alloc.reflow("—either "),
                        alloc.global_tag_name("True".into()),
                        alloc.reflow(" or "),
                        alloc.global_tag_name("False".into()),
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
                let problem = alloc.concat(vec![
                    alloc.text("This "),
                    alloc.keyword("if"),
                    alloc.text(" guard condition needs to be a "),
                    alloc.type_str("Bool"),
                    alloc.text(":"),
                ]);
                report_bad_type(
                    alloc,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    alloc.text("Right now it’s"),
                    alloc.concat(vec![
                        alloc.reflow("But I need every "),
                        alloc.keyword("if"),
                        alloc.reflow(" guard condition to evaluate to a "),
                        alloc.type_str("Bool"),
                        alloc.reflow("—either "),
                        alloc.global_tag_name("True".into()),
                        alloc.reflow(" or "),
                        alloc.global_tag_name("False".into()),
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
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.concat(vec![
                        alloc.text("This "),
                        alloc.keyword("if"),
                        alloc.text(" has an "),
                        alloc.keyword("else"),
                        alloc.text(" branch with a different type from its "),
                        alloc.keyword("then"),
                        alloc.text(" branch:"),
                    ]),
                    alloc.concat(vec![
                        alloc.text("The "),
                        alloc.keyword("else"),
                        alloc.text(" branch is"),
                    ]),
                    alloc.concat(vec![
                        alloc.text("but the "),
                        alloc.keyword("then"),
                        alloc.text(" branch has the type:"),
                    ]),
                    Some(alloc.concat(vec![
                        alloc.text("I need all branches in an "),
                        alloc.keyword("if"),
                        alloc.text(" to have the same type!"),
                    ])),
                ),
                _ => report_mismatch(
                    alloc,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.concat(vec![
                        alloc.reflow("The "),
                        alloc.string(index.ordinal()),
                        alloc.reflow(" branch of this "),
                        alloc.keyword("if"),
                        alloc.reflow(" does not match all the previous branches:"),
                    ]),
                    alloc.string(format!("The {} branch is", index.ordinal())),
                    alloc.reflow("But all the previous branches have type:"),
                    Some(alloc.concat(vec![
                        alloc.reflow("I need all branches in an "),
                        alloc.keyword("if"),
                        alloc.reflow(" to have the same type!"),
                    ])),
                ),
            },
            Reason::WhenBranch { index } => report_mismatch(
                alloc,
                filename,
                &category,
                found,
                expected_type,
                region,
                Some(expr_region),
                alloc.concat(vec![
                    alloc.reflow("The "),
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch of this "),
                    alloc.keyword("when"),
                    alloc.reflow(" does not match all the previous branches:"),
                ]),
                alloc.concat(vec![
                    alloc.reflow("The "),
                    alloc.string(index.ordinal()),
                    alloc.reflow(" branch is"),
                ]),
                alloc.reflow("But all the previous branches have type:"),
                Some(alloc.concat(vec![
                    alloc.reflow("I need all branches of a "),
                    alloc.keyword("when"),
                    alloc.reflow(" to have the same type!"),
                ])),
            ),
            Reason::ElemInList { index } => {
                let ith = index.ordinal();

                report_mismatch(
                    alloc,
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.string(format!(
                        "The {} element of this list does not match all the previous elements:",
                        ith
                    )),
                    alloc.string(format!("The {} element is", ith)),
                    alloc.reflow("But all the previous elements in the list have type:"),
                    Some(alloc.reflow("I need all elements of a list to have the same type!")),
                )
            }
            Reason::RecordUpdateValue(field) => report_mismatch(
                alloc,
                filename,
                &category,
                found,
                expected_type,
                region,
                Some(expr_region),
                alloc.concat(vec![
                    alloc.text("I cannot update the "),
                    alloc.record_field(field.to_owned()),
                    alloc.text(" field like this:"),
                ]),
                alloc.concat(vec![
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
            Reason::RecordUpdateKeys(symbol, expected_fields) => match found.clone().unwrap_alias()
            {
                ErrorType::Record(actual_fields, ext) => {
                    let expected_set: MutSet<_> = expected_fields.keys().cloned().collect();
                    let actual_set: MutSet<_> = actual_fields.keys().cloned().collect();

                    let mut diff = expected_set.difference(&actual_set);

                    match diff.next().and_then(|k| Some((k, expected_fields.get(k)?))) {
                        None => report_mismatch(
                            alloc,
                            filename,
                            &category,
                            found,
                            expected_type,
                            region,
                            Some(expr_region),
                            alloc.reflow("Something is off with this record update:"),
                            alloc.concat(vec![
                                alloc.reflow("The"),
                                alloc.symbol_unqualified(symbol),
                                alloc.reflow(" record is"),
                            ]),
                            alloc.reflow("But this update needs it to be compatible with:"),
                            None,
                        ),
                        Some((field, field_region)) => {
                            let r_doc = alloc.symbol_unqualified(symbol);
                            let f_doc = alloc.record_field(field.clone());

                            let header = alloc.concat(vec![
                                alloc.reflow("The "),
                                r_doc.clone(),
                                alloc.reflow(" record does not have a "),
                                f_doc.clone(),
                                alloc.reflow(" field:"),
                            ]);

                            let mut suggestions = suggest::sort(
                                field.as_str(),
                                actual_fields.into_iter().collect::<Vec<_>>(),
                            );

                            let doc = alloc.stack(vec![
                                header,
                                alloc.region(*field_region),
                                if suggestions.is_empty() {
                                    alloc.concat(vec![
                                        alloc.reflow("In fact, "),
                                        r_doc,
                                        alloc.reflow(" is a record with NO fields!"),
                                    ])
                                } else {
                                    let f = suggestions.remove(0);
                                    let fs = suggestions;

                                    alloc.stack(vec![
                                        alloc.concat(vec![
                                            alloc.reflow("This is usually a typo. Here are the "),
                                            r_doc,
                                            alloc.reflow(" fields that are most similar:"),
                                        ]),
                                        report_text::to_suggestion_record(
                                            alloc,
                                            f.clone(),
                                            fs,
                                            ext,
                                        ),
                                        alloc.concat(vec![
                                            alloc.reflow("So maybe "),
                                            f_doc,
                                            alloc.reflow(" should be "),
                                            alloc.record_field(f.0),
                                            alloc.reflow("?"),
                                        ]),
                                    ])
                                },
                            ]);

                            Report {
                                filename,
                                title: "TYPE MISMATCH".to_string(),
                                doc,
                            }
                        }
                    }
                }
                _ => report_bad_type(
                    alloc,
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
            },
            Reason::FnCall { name, arity } => match count_arguments(&found) {
                0 => {
                    let this_value = match name {
                        None => alloc.text("This value"),
                        Some(symbol) => alloc.concat(vec![
                            alloc.text("The "),
                            alloc.symbol_unqualified(symbol),
                            alloc.text(" value"),
                        ]),
                    };

                    let lines = vec![
                        alloc.concat(vec![
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
                        alloc.region(expr_region),
                        alloc.reflow("Are there any missing commas? Or missing parentheses?"),
                    ];

                    Report {
                        filename,
                        title: "TOO MANY ARGS".to_string(),
                        doc: alloc.stack(lines),
                    }
                }
                n => {
                    let this_function = match name {
                        None => alloc.text("This function"),
                        Some(symbol) => alloc.concat(vec![
                            alloc.text("The "),
                            alloc.symbol_unqualified(symbol),
                            alloc.text(" function"),
                        ]),
                    };

                    if n < arity as usize {
                        let lines = vec![
                            alloc.concat(vec![
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
                            alloc.region(expr_region),
                            alloc.reflow("Are there any missing commas? Or missing parentheses?"),
                        ];

                        Report {
                            filename,
                            title: "TOO MANY ARGS".to_string(),
                            doc: alloc.stack(lines),
                        }
                    } else {
                        let lines = vec![
                            alloc.concat(vec![
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
                            alloc.region(expr_region),
                            alloc.reflow(
                                "Roc does not allow functions to be partially applied. \
                                Use a closure to make partial application explicit.",
                            ),
                        ];

                        Report {
                            filename,
                            title: "TOO FEW ARGS".to_string(),
                            doc: alloc.stack(lines),
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
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    alloc.concat(vec![
                        alloc.string(format!("The {} argument to ", ith)),
                        this_function.clone(),
                        alloc.text(" is not what I expect:"),
                    ]),
                    alloc.text("This argument is"),
                    alloc.concat(vec![
                        alloc.text("But "),
                        this_function,
                        alloc.string(format!(" needs the {} argument to be:", ith)),
                    ]),
                    None,
                )
            }
            Reason::FloatLiteral | Reason::IntLiteral | Reason::NumLiteral => {
                unreachable!("I don't think these can be reached")
            }

            Reason::InterpolatedStringVar => {
                unimplemented!("string interpolation is not implemented yet")
            }
        },
    }
}

fn count_arguments(tipe: &ErrorType) -> usize {
    use ErrorType::*;

    match tipe {
        Function(args, _) => args.len(),
        Type(Symbol::ATTR_ATTR, args) => count_arguments(&args[1]),
        Alias(_, _, actual) => count_arguments(actual),
        _ => 0,
    }
}

fn type_comparison<'b>(
    alloc: &'b RocDocAllocator<'b>,
    actual: ErrorType,
    expected: ErrorType,
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

    lines.extend(problems_to_hint(alloc, comparison.problems));

    alloc.stack(lines)
}

fn lone_type<'b>(
    alloc: &'b RocDocAllocator<'b>,
    actual: ErrorType,
    expected: ErrorType,
    i_am_seeing: RocDocBuilder<'b>,
    further_details: RocDocBuilder<'b>,
) -> RocDocBuilder<'b> {
    let comparison = to_comparison(alloc, actual, expected);

    let mut lines = vec![i_am_seeing, comparison.actual, further_details];

    lines.extend(problems_to_hint(alloc, comparison.problems));

    alloc.stack(lines)
}

fn add_category<'b>(
    alloc: &'b RocDocAllocator<'b>,
    this_is: RocDocBuilder<'b>,
    category: &Category,
) -> RocDocBuilder<'b> {
    use Category::*;

    match category {
        Lookup(name) => alloc.concat(vec![
            alloc.text("This "),
            alloc.symbol_foreign_qualified(*name),
            alloc.text(" value is a:"),
        ]),

        If => alloc.concat(vec![
            alloc.text("This "),
            alloc.keyword("if"),
            alloc.text("expression produces:"),
        ]),
        When => alloc.concat(vec![
            alloc.text("This "),
            alloc.keyword("when"),
            alloc.text("expression produces:"),
        ]),

        List => alloc.concat(vec![this_is, alloc.text(" a list of type:")]),
        Num => alloc.concat(vec![this_is, alloc.text(" a number of type:")]),
        Int => alloc.concat(vec![this_is, alloc.text(" an integer of type:")]),
        Float => alloc.concat(vec![this_is, alloc.text(" a float of type:")]),
        Str => alloc.concat(vec![this_is, alloc.text(" a string of type:")]),

        Lambda => alloc.concat(vec![this_is, alloc.text(" an anonymous function of type:")]),

        TagApply(TagName::Global(name)) => alloc.concat(vec![
            alloc.text("This "),
            alloc.global_tag_name(name.to_owned()),
            alloc.text(" global tag application has the type:"),
        ]),
        TagApply(TagName::Private(name)) => alloc.concat(vec![
            alloc.text("This "),
            alloc.private_tag_name(*name),
            alloc.text(" private tag application has the type:"),
        ]),

        Record => alloc.concat(vec![this_is, alloc.text(" a record of type:")]),

        Accessor(field) => alloc.concat(vec![
            alloc.text("This "),
            alloc.record_field(field.to_owned()),
            alloc.text(" value is a:"),
        ]),
        Access(field) => alloc.concat(vec![
            alloc.text("The value at "),
            alloc.record_field(field.to_owned()),
            alloc.text(" is a:"),
        ]),

        CallResult(Some(symbol)) => alloc.concat(vec![
            alloc.text("This "),
            alloc.symbol_foreign_qualified(*symbol),
            alloc.text(" call produces:"),
        ]),
        CallResult(None) => alloc.concat(vec![this_is, alloc.text(":")]),

        Uniqueness => alloc.concat(vec![
            this_is,
            alloc.text(" an uniqueness attribute of type:"),
        ]),
        Storage => alloc.concat(vec![this_is, alloc.text(" a value of type:")]),
    }
}

fn to_pattern_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    expr_region: roc_region::all::Region,
    category: PatternCategory,
    found: ErrorType,
    expected: PExpected<ErrorType>,
) -> Report<'b> {
    use roc_types::types::PReason;

    match expected {
        PExpected::NoExpectation(expected_type) => {
            let doc = alloc.stack(vec![
                alloc.text("This pattern is being used in an unexpected way:"),
                alloc.region(expr_region),
                pattern_type_comparision(
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
            }
        }

        PExpected::ForReason(reason, expected_type, region) => match reason {
            PReason::WhenMatch { index } => {
                if index == Index::FIRST {
                    let doc = alloc.stack(vec![
                        alloc
                            .text("The 1st pattern in this ")
                            .append(alloc.keyword("when"))
                            .append(alloc.text(" is causing a mismatch:")),
                        alloc.region(region),
                        pattern_type_comparision(
                            alloc,
                            found,
                            expected_type,
                            add_pattern_category(
                                alloc,
                                alloc.text("The first pattern is trying to match"),
                                &category,
                            ),
                            alloc.concat(vec![
                                alloc.text("But the expression between "),
                                alloc.keyword("when"),
                                alloc.text(" and "),
                                alloc.keyword("is"),
                                alloc.text(" has the type:"),
                            ]),
                            vec![],
                        ),
                    ]);

                    Report {
                        filename,
                        title: "TYPE MISMATCH".to_string(),
                        doc,
                    }
                } else {
                    let doc = alloc.stack(vec![
                        alloc
                            .string(format!("The {} pattern in this ", index.ordinal()))
                            .append(alloc.keyword("when"))
                            .append(alloc.text(" does not match the previous ones:")),
                        alloc.region(region),
                        pattern_type_comparision(
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
                    ]);

                    Report {
                        filename,
                        title: "TYPE MISMATCH".to_string(),
                        doc,
                    }
                }
            }
            PReason::TagArg { .. } | PReason::PatternGuard => {
                unreachable!("I didn't think this could trigger. Please tell Folkert about it!")
            }
        },
    }
}

fn pattern_type_comparision<'b>(
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

    lines.extend(problems_to_hint(alloc, comparison.problems));
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
        Set => alloc.reflow(" sets of type:"),
        Map => alloc.reflow(" maps of type:"),
        Ctor(tag_name) => alloc.concat(vec![
            alloc.tag_name(tag_name.clone()),
            alloc.reflow(" values of type:"),
        ]),
        Str => alloc.reflow(" strings:"),
        Num => alloc.reflow(" numbers:"),
        Int => alloc.reflow(" integers:"),
        Float => alloc.reflow(" floats"),
    };

    alloc.concat(vec![i_am_trying_to_match, rest])
}

fn to_circular_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    region: roc_region::all::Region,
    symbol: Symbol,
    overall_type: ErrorType,
) -> Report<'b> {
    Report {
        title: "CIRCULAR TYPE".to_string(),
        filename,
        doc: {
            alloc.stack(vec![
                alloc
                    .reflow("I'm inferring a weird self-referential type for ")
                    .append(alloc.symbol_unqualified(symbol))
                    .append(alloc.text(":")),
                alloc.region(region),
                alloc.stack(vec![
                    alloc.reflow(
                        "Here is my best effort at writing down the type. \
                        You will see ∞ for parts of the type that repeat \
                        something already printed out infinitely.",
                    ),
                    alloc.type_block(to_doc(alloc, Parens::Unnecessary, overall_type)),
                ]),
            ])
        },
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
    BadRigidVar(Lowercase, ErrorType),
}

fn problems_to_hint<'b>(
    alloc: &'b RocDocAllocator<'b>,
    mut problems: Vec<Problem>,
) -> Option<RocDocBuilder<'b>> {
    if problems.is_empty() {
        None
    } else {
        let problem = problems.remove(problems.len() - 1);
        Some(type_problem_to_pretty(alloc, problem))
    }
}

pub mod suggest {
    use core::convert::AsRef;
    use distance;
    use inlinable_string::InlinableString;
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

    impl ToStr for InlinableString {
        fn to_str(&self) -> &str {
            self.as_ref()
        }
    }

    impl ToStr for &str {
        fn to_str(&self) -> &str {
            self
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

    pub fn sort<'a, T>(typo: &'a str, mut options: Vec<T>) -> Vec<T>
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

    Comparison {
        actual: alloc.type_block(diff.left),
        expected: alloc.type_block(diff.right),
        problems: match diff.status {
            Status::Similar => vec![],
            Status::Different(problems) => problems,
        },
    }
}

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
}

fn ext_to_doc<'b>(alloc: &'b RocDocAllocator<'b>, ext: TypeExt) -> Option<RocDocBuilder<'b>> {
    use TypeExt::*;

    match ext {
        Closed => None,
        FlexOpen(lowercase) | RigidOpen(lowercase) => Some(alloc.type_variable(lowercase)),
    }
}

pub fn to_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    tipe: ErrorType,
) -> RocDocBuilder<'b> {
    use ErrorType::*;

    match tipe {
        Function(args, ret) => report_text::function(
            alloc,
            parens,
            args.into_iter()
                .map(|arg| to_doc(alloc, Parens::InFn, arg))
                .collect(),
            to_doc(alloc, Parens::InFn, *ret),
        ),
        Infinite => alloc.text("∞"),
        Error => alloc.text("?"),

        FlexVar(lowercase) => alloc.type_variable(lowercase),
        RigidVar(lowercase) => alloc.type_variable(lowercase),

        Type(symbol, args) => report_text::apply(
            alloc,
            parens,
            alloc.symbol_foreign_qualified(symbol),
            args.into_iter()
                .map(|arg| to_doc(alloc, Parens::InTypeParam, arg))
                .collect(),
        ),

        Alias(symbol, args, _) => report_text::apply(
            alloc,
            parens,
            alloc.symbol_foreign_qualified(symbol),
            args.into_iter()
                .map(|(_, arg)| to_doc(alloc, Parens::InTypeParam, arg))
                .collect(),
        ),

        Record(fields_map, ext) => {
            let mut fields = fields_map.into_iter().collect::<Vec<_>>();
            fields.sort_by(|(a, _), (b, _)| a.cmp(&b));

            report_text::record(
                alloc,
                fields
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            alloc.string(k.as_str().to_string()),
                            to_doc(alloc, Parens::Unnecessary, v),
                        )
                    })
                    .collect(),
                ext_to_doc(alloc, ext),
            )
        }

        TagUnion(tags_map, ext) => {
            let mut tags = tags_map
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.into_iter()
                            .map(|arg| to_doc(alloc, Parens::InTypeParam, arg))
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>();
            tags.sort_by(|(a, _), (b, _)| a.cmp(&b));

            report_text::tag_union(
                alloc,
                tags.into_iter()
                    .map(|(k, v)| (alloc.tag_name(k), v))
                    .collect(),
                ext_to_doc(alloc, ext),
            )
        }

        RecursiveTagUnion(rec_var, tags_map, ext) => {
            let mut tags = tags_map
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.into_iter()
                            .map(|arg| to_doc(alloc, Parens::InTypeParam, arg))
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>();
            tags.sort_by(|(a, _), (b, _)| a.cmp(&b));

            report_text::recursive_tag_union(
                alloc,
                to_doc(alloc, Parens::Unnecessary, *rec_var),
                tags.into_iter()
                    .map(|(k, v)| (alloc.tag_name(k), v))
                    .collect(),
                ext_to_doc(alloc, ext),
            )
        }
        Boolean(b) => alloc.string(format!("{:?}", b)),
    }
}

fn same<'b>(
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    tipe: ErrorType,
) -> Diff<RocDocBuilder<'b>> {
    let doc = to_doc(alloc, parens, tipe);

    Diff {
        left: doc.clone(),
        right: doc,
        status: Status::Similar,
    }
}

fn to_diff<'b>(
    alloc: &'b RocDocAllocator<'b>,
    parens: Parens,
    type1: ErrorType,
    type2: ErrorType,
) -> Diff<RocDocBuilder<'b>> {
    use ErrorType::*;

    // TODO remove clone
    match (type1.clone(), type2.clone()) {
        (Error, Error) | (Infinite, Infinite) => same(alloc, parens, type1),

        (FlexVar(x), FlexVar(y)) if x == y => same(alloc, parens, type1),
        (RigidVar(x), RigidVar(y)) if x == y => same(alloc, parens, type1),

        (Function(args1, ret1), Function(args2, ret2)) => {
            if args1.len() == args2.len() {
                let mut status = Status::Similar;
                let arg_diff = traverse(alloc, Parens::InFn, args1, args2);
                let ret_diff = to_diff(alloc, Parens::InFn, *ret1, *ret2);
                status.merge(arg_diff.status);
                status.merge(ret_diff.status);

                let left = report_text::function(alloc, parens, arg_diff.left, ret_diff.left);
                let right = report_text::function(alloc, parens, arg_diff.right, ret_diff.right);

                Diff {
                    left,
                    right,
                    status,
                }
            } else {
                let left = to_doc(alloc, Parens::InFn, type1);
                let right = to_doc(alloc, Parens::InFn, type2);

                Diff {
                    left,
                    right,
                    status: Status::Different(vec![Problem::ArityMismatch(
                        args1.len(),
                        args2.len(),
                    )]),
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
            }
        }

        (Alias(symbol1, args1, _), Alias(symbol2, args2, _)) if symbol1 == symbol2 => {
            // TODO remove collects
            let a1 = args1.into_iter().map(|(_, v)| v).collect::<Vec<_>>();
            let a2 = args2.into_iter().map(|(_, v)| v).collect::<Vec<_>>();
            let args_diff = traverse(alloc, Parens::InTypeParam, a1, a2);
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
            }
        }

        (Record(fields1, ext1), Record(fields2, ext2)) => {
            diff_record(alloc, fields1, ext1, fields2, ext2)
        }

        (TagUnion(tags1, ext1), TagUnion(tags2, ext2)) => {
            diff_tag_union(alloc, &tags1, ext1, &tags2, ext2)
        }

        (RecursiveTagUnion(_rec1, _tags1, _ext1), RecursiveTagUnion(_rec2, _tags2, _ext2)) => {
            // TODO do a better job here
            let left = to_doc(alloc, Parens::Unnecessary, type1);
            let right = to_doc(alloc, Parens::Unnecessary, type2);

            Diff {
                left,
                right,
                status: Status::Similar,
            }
        }

        pair => {
            // We hit none of the specific cases where we give more detailed information
            let left = to_doc(alloc, Parens::Unnecessary, type1);
            let right = to_doc(alloc, Parens::Unnecessary, type2);

            let is_int = |t: &ErrorType| match t {
                ErrorType::Type(Symbol::INT_INT, _) => true,
                ErrorType::Alias(Symbol::INT_INT, _, _) => true,

                ErrorType::Type(Symbol::NUM_NUM, args) => match &args.get(0) {
                    Some(ErrorType::Type(Symbol::INT_INTEGER, _)) => true,
                    Some(ErrorType::Alias(Symbol::INT_INTEGER, _, _)) => true,
                    _ => false,
                },
                ErrorType::Alias(Symbol::NUM_NUM, args, _) => match &args.get(0) {
                    Some((_, ErrorType::Type(Symbol::INT_INTEGER, _))) => true,
                    Some((_, ErrorType::Alias(Symbol::INT_INTEGER, _, _))) => true,
                    _ => false,
                },
                _ => false,
            };
            let is_float = |t: &ErrorType| match t {
                ErrorType::Type(Symbol::FLOAT_FLOAT, _) => true,
                ErrorType::Alias(Symbol::FLOAT_FLOAT, _, _) => true,

                _ => false,
            };

            let problems = match pair {
                (RigidVar(x), other) | (other, RigidVar(x)) => vec![Problem::BadRigidVar(x, other)],
                (a, b) if (is_int(&a) && is_float(&b)) || (is_float(&a) && is_int(&b)) => {
                    vec![Problem::IntFloat]
                }
                _ => vec![],
            };

            Diff {
                left,
                right,
                status: Status::Different(problems),
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

    for (arg1, arg2) in args1.into_iter().zip(args2.into_iter()) {
        let diff = to_diff(alloc, parens, arg1, arg2);

        left.push(diff.left);
        right.push(diff.right);
        status.merge(diff.status);
    }

    Diff {
        left,
        right,
        status,
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
    fields1: SendMap<Lowercase, ErrorType>,
    ext1: TypeExt,
    fields2: SendMap<Lowercase, ErrorType>,
    ext2: TypeExt,
) -> Diff<RocDocBuilder<'b>> {
    let to_overlap_docs = |(field, (t1, t2)): &(Lowercase, (ErrorType, ErrorType))| {
        let diff = to_diff(alloc, Parens::Unnecessary, t1.clone(), t2.clone());

        Diff {
            left: (
                field.clone(),
                alloc.string(field.as_str().to_string()),
                diff.left,
            ),
            right: (
                field.clone(),
                alloc.string(field.as_str().to_string()),
                diff.right,
            ),
            status: diff.status,
        }
    };
    let to_unknown_docs = |(field, tipe): &(Lowercase, ErrorType)| {
        (
            field.clone(),
            alloc.string(field.as_str().to_string()),
            to_doc(alloc, Parens::Unnecessary, tipe.clone()),
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

    let ext_diff = ext_to_diff(alloc, ext1, ext2);

    let mut fields_diff: Diff<Vec<(Lowercase, RocDocBuilder<'b>, RocDocBuilder<'b>)>> = Diff {
        left: vec![],
        right: vec![],
        status: Status::Similar,
    };

    for diff in both {
        fields_diff.left.push(diff.left);
        fields_diff.right.push(diff.right);
        fields_diff.status.merge(diff.status);
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
    }
}

fn diff_tag_union<'b>(
    alloc: &'b RocDocAllocator<'b>,
    fields1: &SendMap<TagName, Vec<ErrorType>>,
    ext1: TypeExt,
    fields2: &SendMap<TagName, Vec<ErrorType>>,
    ext2: TypeExt,
) -> Diff<RocDocBuilder<'b>> {
    let to_overlap_docs = |(field, (t1, t2)): (TagName, (Vec<ErrorType>, Vec<ErrorType>))| {
        let diff = traverse(alloc, Parens::Unnecessary, t1, t2);

        Diff {
            left: (field.clone(), alloc.tag_name(field.clone()), diff.left),
            right: (field.clone(), alloc.tag_name(field), diff.right),
            status: diff.status,
        }
    };
    let to_unknown_docs = |(field, args): &(TagName, Vec<ErrorType>)| {
        (
            field.clone(),
            alloc.tag_name(field.clone()),
            // TODO add spaces between args
            args.iter()
                .map(|arg| to_doc(alloc, Parens::Unnecessary, arg.clone()))
                .collect(),
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
        (true, true) => match left.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::TagTypo(
                f.clone(),
                fields2.keys().cloned().collect(),
            )]),
            None => {
                if right.peek().is_none() {
                    Status::Similar
                } else {
                    let result =
                        Status::Different(vec![Problem::TagsMissing(right.map(|v| v.0).collect())]);
                    // we just used the values in `right`.  in
                    right = right_keys.iter().map(to_unknown_docs).peekable();
                    result
                }
            }
        },
        (false, true) => match left.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::TagTypo(
                f.clone(),
                fields2.keys().cloned().collect(),
            )]),
            None => Status::Similar,
        },
        (true, false) => match right.peek() {
            Some((f, _, _)) => Status::Different(vec![Problem::TagTypo(
                f.clone(),
                fields1.keys().cloned().collect(),
            )]),
            None => Status::Similar,
        },
        (false, false) => Status::Similar,
    };

    let ext_diff = ext_to_diff(alloc, ext1, ext2);

    let mut fields_diff: Diff<Vec<(TagName, RocDocBuilder<'b>, Vec<RocDocBuilder<'b>>)>> = Diff {
        left: vec![],
        right: vec![],
        status: Status::Similar,
    };

    for diff in both {
        fields_diff.left.push(diff.left);
        fields_diff.right.push(diff.right);
        fields_diff.status.merge(diff.status);
    }

    if !all_fields_shared {
        fields_diff.left.extend(left);
        fields_diff.right.extend(right);
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
    }
}

fn ext_to_diff<'b>(
    alloc: &'b RocDocAllocator<'b>,
    ext1: TypeExt,
    ext2: TypeExt,
) -> Diff<Option<RocDocBuilder<'b>>> {
    let status = ext_to_status(&ext1, &ext2);
    let ext_doc_1 = ext_to_doc(alloc, ext1);
    let ext_doc_2 = ext_to_doc(alloc, ext2);

    match &status {
        Status::Similar => Diff {
            left: ext_doc_1,
            right: ext_doc_2,
            status,
        },
        Status::Different(_) => Diff {
            // NOTE elm colors these differently at this point
            left: ext_doc_1,
            right: ext_doc_2,
            status,
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
    use roc_types::types::{ErrorType, TypeExt};
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
        let function_doc = alloc.concat(vec![
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
            let apply_doc = alloc.concat(vec![
                name,
                alloc.space(),
                alloc.intersperse(args, alloc.space()),
            ]);

            match parens {
                Parens::Unnecessary | Parens::InFn => apply_doc,
                Parens::InTypeParam => with_parens(alloc, apply_doc),
            }
        }
    }

    pub fn record<'b>(
        alloc: &'b RocDocAllocator<'b>,
        entries: Vec<(RocDocBuilder<'b>, RocDocBuilder<'b>)>,
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
                |(field_name, field_type): (RocDocBuilder<'b>, RocDocBuilder<'b>)| {
                    field_name.append(alloc.text(" : ")).append(field_type)
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
        f: (Lowercase, ErrorType),
        fs: Vec<(Lowercase, ErrorType)>,
        ext: TypeExt,
    ) -> RocDocBuilder<'b> {
        use crate::type_error::{ext_to_doc, to_doc};

        let entry_to_doc = |(name, tipe): (Lowercase, ErrorType)| {
            (
                alloc.string(name.as_str().to_string()),
                to_doc(alloc, Parens::Unnecessary, tipe),
            )
        };

        if fs.len() <= 3 {
            let mut selection = vec![f];
            selection.extend(fs);

            let fields = selection.into_iter().map(entry_to_doc).collect();

            vertical_record(alloc, fields, ext_to_doc(alloc, ext))
                .annotate(Annotation::TypeBlock)
                .indent(4)
        } else {
            let fields = fs.into_iter().take(3).map(entry_to_doc).collect();

            vertical_record_snippet(alloc, entry_to_doc(f), fields)
                .annotate(Annotation::TypeBlock)
                .indent(4)
        }
    }

    fn vertical_record<'b>(
        alloc: &'b RocDocAllocator<'b>,
        entries: Vec<(RocDocBuilder<'b>, RocDocBuilder<'b>)>,
        opt_ext: Option<RocDocBuilder<'b>>,
    ) -> RocDocBuilder<'b> {
        let entry_to_doc = |(field_name, field_type): (RocDocBuilder<'b>, RocDocBuilder<'b>)| {
            field_name
                .append(alloc.text(" : "))
                .hang(4)
                .append(field_type)
        };

        match opt_ext {
            None => {
                if entries.is_empty() {
                    alloc.text("{}")
                } else {
                    let start = std::iter::once(alloc.reflow("{ "))
                        .chain(std::iter::repeat(alloc.reflow(", ")));
                    let entry_docs = start
                        .zip(entries.into_iter().map(entry_to_doc))
                        .map(|(a, b)| a.append(b));
                    alloc.vcat(entry_docs.chain(std::iter::once(alloc.text("}"))))
                }
            }
            Some(ext) => {
                let start = std::iter::once(alloc.reflow("{ "))
                    .chain(std::iter::repeat(alloc.reflow(", ")));
                let entry_docs = start
                    .zip(entries.into_iter().map(entry_to_doc))
                    .map(|(a, b)| a.append(b));
                alloc
                    .vcat(entry_docs.chain(std::iter::once(alloc.text("}"))))
                    .append(ext)
            }
        }
    }

    fn vertical_record_snippet<'b>(
        alloc: &'b RocDocAllocator<'b>,
        entry: (RocDocBuilder<'b>, RocDocBuilder<'b>),
        entries: Vec<(RocDocBuilder<'b>, RocDocBuilder<'b>)>,
    ) -> RocDocBuilder<'b> {
        let entry_to_doc = |(field_name, field_type): (RocDocBuilder<'b>, RocDocBuilder<'b>)| {
            field_name
                .append(alloc.text(" : "))
                .hang(4)
                .append(field_type)
        };

        let field = alloc.reflow("{ ").append(entry_to_doc(entry));
        let fields = std::iter::repeat(alloc.reflow(", "))
            .zip(
                entries
                    .into_iter()
                    .map(entry_to_doc)
                    .chain(std::iter::once(alloc.text("..."))),
            )
            .map(|(a, b)| a.append(b));

        alloc.vcat(
            std::iter::once(field)
                .chain(fields)
                .chain(std::iter::once(alloc.text("}"))),
        )
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
                std::iter::once(alloc.reflow("[ ")).chain(std::iter::repeat(alloc.reflow(", ")));

            let entries_doc = alloc.concat(
                entries
                    .into_iter()
                    .zip(starts)
                    .map(|(entry, start)| start.append(entry_to_doc(entry))),
            );

            entries_doc.append(alloc.reflow(" ]")).append(ext_doc)
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
                std::iter::once(alloc.reflow("[ ")).chain(std::iter::repeat(alloc.reflow(", ")));

            let entries_doc = alloc.concat(
                entries
                    .into_iter()
                    .zip(starts)
                    .map(|(entry, start)| start.append(entry_to_doc(entry))),
            );

            entries_doc
                .append(alloc.reflow(" ]"))
                .append(ext_doc)
                .append(alloc.text(" as "))
                .append(rec_var)
        }
    }
}

fn type_problem_to_pretty<'b>(
    alloc: &'b RocDocAllocator<'b>,
    problem: crate::type_error::Problem,
) -> RocDocBuilder<'b> {
    use crate::type_error::Problem::*;

    match problem {
        FieldTypo(typo, possibilities) => {
            let suggestions = suggest::sort(typo.as_str(), possibilities);

            match suggestions.get(0) {
                None => alloc.nil(),
                Some(nearest) => {
                    let typo_str = format!("{}", typo);
                    let nearest_str = format!("{}", nearest);

                    let found = alloc.text(typo_str).annotate(Annotation::Typo);
                    let suggestion = alloc.text(nearest_str).annotate(Annotation::TypoSuggestion);

                    let hint1 = alloc
                        .hint()
                        .append(alloc.reflow("Seems like a record field typo. Maybe "))
                        .append(found)
                        .append(alloc.reflow(" should be "))
                        .append(suggestion)
                        .append(alloc.text("?"));

                    let hint2 = alloc.hint().append(alloc.reflow(ADD_ANNOTATIONS));

                    hint1
                        .append(alloc.line())
                        .append(alloc.line())
                        .append(hint2)
                }
            }
        }
        FieldsMissing(missing) => match missing.split_last() {
            None => alloc.nil(),
            Some((f1, [])) => alloc
                .hint()
                .append(alloc.reflow("Looks like the "))
                .append(f1.as_str().to_owned())
                .append(alloc.reflow(" field is missing.")),
            Some((last, init)) => {
                let separator = alloc.reflow(", ");

                alloc
                    .hint()
                    .append(alloc.reflow("Looks like the "))
                    .append(
                        alloc.intersperse(init.iter().map(|v| v.as_str().to_owned()), separator),
                    )
                    .append(alloc.reflow(" and "))
                    .append(alloc.text(last.as_str().to_owned()))
                    .append(alloc.reflow(" fields are missing."))
            }
        },
        TagTypo(typo, possibilities_tn) => {
            let possibilities = possibilities_tn
                .into_iter()
                .map(|tag_name| tag_name.into_string(alloc.interns, alloc.home))
                .collect();
            let typo_str = format!("{}", typo.into_string(alloc.interns, alloc.home));
            let suggestions = suggest::sort(&typo_str, possibilities);

            match suggestions.get(0) {
                None => alloc.nil(),
                Some(nearest) => {
                    let nearest_str = format!("{}", nearest);

                    let found = alloc.text(typo_str).annotate(Annotation::Typo);
                    let suggestion = alloc.text(nearest_str).annotate(Annotation::TypoSuggestion);

                    let hint1 = alloc
                        .hint()
                        .append(alloc.reflow("Seems like a tag typo. Maybe "))
                        .append(found)
                        .append(" should be ")
                        .append(suggestion)
                        .append(alloc.text("?"));

                    let hint2 = alloc.hint().append(alloc.reflow(ADD_ANNOTATIONS));

                    hint1
                        .append(alloc.line())
                        .append(alloc.line())
                        .append(hint2)
                }
            }
        }
        ArityMismatch(found, expected) => {
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

            alloc.hint().append(line)
        }

        BadRigidVar(x, tipe) => {
            use ErrorType::*;

            let bad_rigid_var = |name: Lowercase, a_thing| {
                alloc
                    .hint()
                    .append(alloc.reflow("The type annotation uses the type variable "))
                    .append(alloc.type_variable(name))
                    .append(alloc.reflow(" to say that this definition can produce any type of value. But in the body I see that it will only produce "))
                    .append(a_thing)
                    .append(alloc.reflow(" of a single specific type. Maybe change the type annotation to be more specific? Maybe change the code to be more general?"))
            };

            let bad_double_rigid = |a, b| {
                let line = r#" as separate type variables. Your code seems to be saying they are the same though. Maybe they should be the same your type annotation? Maybe your code uses them in a weird way?"#;

                alloc
                    .hint()
                    .append(alloc.reflow("Your type annotation uses "))
                    .append(alloc.type_variable(a))
                    .append(alloc.reflow(" and "))
                    .append(alloc.type_variable(b))
                    .append(alloc.reflow(line))
            };

            match tipe {
                Infinite | Error | FlexVar(_) => alloc.nil(),
                RigidVar(y) => bad_double_rigid(x, y),
                Function(_, _) => bad_rigid_var(x, alloc.reflow("a function value")),
                Record(_, _) => bad_rigid_var(x, alloc.reflow("a record value")),
                TagUnion(_, _) | RecursiveTagUnion(_, _, _) => {
                    bad_rigid_var(x, alloc.reflow("a tag value"))
                }
                Alias(symbol, _, _) | Type(symbol, _) => bad_rigid_var(
                    x,
                    alloc.concat(vec![
                        alloc.reflow("a "),
                        alloc.symbol_unqualified(symbol),
                        alloc.reflow(" value"),
                    ]),
                ),
                Boolean(_) => bad_rigid_var(x, alloc.reflow("a uniqueness attribute value")),
            }
        }
        IntFloat => alloc.hint().append(alloc.concat(vec![
            alloc.reflow("Convert between "),
            alloc.type_str("Int"),
            alloc.reflow(" and "),
            alloc.type_str("Float"),
            alloc.reflow(" with "),
            alloc.symbol_qualified(Symbol::NUM_TO_FLOAT),
            alloc.reflow(" and "),
            alloc.symbol_qualified(Symbol::FLOAT_ROUND),
            alloc.reflow("."),
        ])),

        TagsMissing(missing) => match missing.split_last() {
            None => alloc.nil(),
            Some((f1, [])) => {
                let hint1 = alloc
                    .hint()
                    .append(alloc.reflow("Looks like a closed tag union does not have the "))
                    .append(alloc.tag_name(f1.clone()))
                    .append(alloc.reflow(" tag."));

                let hint2 = alloc.hint().append(alloc.reflow(
                    "Closed tag unions can't grow, \
                    because that might change the size in memory. \
                    Can you use an open tag union?",
                ));

                alloc.stack(vec![hint1, hint2])
            }

            Some((last, init)) => {
                let separator = alloc.reflow(", ");

                let hint1 = alloc
                    .hint()
                    .append(alloc.reflow("Looks like a closed tag union does not have the "))
                    .append(
                        alloc
                            .intersperse(init.iter().map(|v| alloc.tag_name(v.clone())), separator),
                    )
                    .append(alloc.reflow(" and "))
                    .append(alloc.tag_name(last.clone()))
                    .append(alloc.reflow(" tags."));

                let hint2 = alloc.hint().append(alloc.reflow(
                    "Closed tag unions can't grow, \
                    because that might change the size in memory. \
                    Can you use an open tag union?",
                ));

                alloc.stack(vec![hint1, hint2])
            }
        },
    }
}
