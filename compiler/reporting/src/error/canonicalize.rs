use roc_collections::all::MutSet;
use roc_problem::can::PrecedenceProblem::BothNonAssociative;
use roc_problem::can::{FloatErrorKind, IntErrorKind, Problem, RuntimeError};
use roc_region::all::Region;
use std::path::PathBuf;

use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder};
use ven_pretty::DocAllocator;

pub fn can_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    filename: PathBuf,
    problem: Problem,
) -> Report<'b> {
    let doc = match problem {
        Problem::UnusedDef(symbol, region) => {
            let line =
                r#" then remove it so future readers of your code don't wonder why it is there."#;

            alloc.stack(vec![
                alloc
                    .symbol_unqualified(symbol)
                    .append(alloc.reflow(" is not used anywhere in your code.")),
                alloc.region(region),
                alloc
                    .reflow("If you didn't intend on using ")
                    .append(alloc.symbol_unqualified(symbol))
                    .append(alloc.reflow(line)),
            ])
        }
        Problem::UnusedImport(module_id, region) => alloc.concat(vec![
            alloc.reflow("Nothing from "),
            alloc.module(module_id),
            alloc.reflow(" is used in this module."),
            alloc.region(region),
            alloc.reflow("Since "),
            alloc.module(module_id),
            alloc.reflow(" isn't used, you don't need to import it."),
        ]),
        Problem::UnusedArgument(closure_symbol, argument_symbol, region) => {
            let line = "\". Adding an underscore at the start of a variable name is a way of saying that the variable is not used.";

            alloc.concat(vec![
                alloc.symbol_unqualified(closure_symbol),
                alloc.reflow(" doesn't use "),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow("."),
                alloc.region(region),
                alloc.reflow("If you don't need "),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow(", then you can just remove it. However, if you really do need "),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow(" as an argument of "),
                alloc.symbol_unqualified(closure_symbol),
                alloc.reflow(", prefix it with an underscore, like this: \"_"),
                alloc.symbol_unqualified(argument_symbol),
                alloc.reflow(line),
            ])
        }
        Problem::PrecedenceProblem(BothNonAssociative(region, left_bin_op, right_bin_op)) => alloc
            .stack(vec![
                if left_bin_op.value == right_bin_op.value {
                    alloc.concat(vec![
                        alloc.reflow("Using more than one "),
                        alloc.binop(left_bin_op.value),
                        alloc.reflow(concat!(
                            " like this requires parentheses,",
                            " to clarify how things should be grouped.",
                        )),
                    ])
                } else {
                    alloc.concat(vec![
                        alloc.reflow("Using "),
                        alloc.binop(left_bin_op.value),
                        alloc.reflow(" and "),
                        alloc.binop(right_bin_op.value),
                        alloc.reflow(concat!(
                            " together requires parentheses, ",
                            "to clarify how they should be grouped."
                        )),
                    ])
                },
                alloc.region(region),
            ]),
        Problem::UnsupportedPattern(pattern_type, region) => {
            use roc_parse::pattern::PatternType::*;

            let this_thing = match pattern_type {
                TopLevelDef => "a top-level definition:",
                DefExpr => "a value definition:",
                FunctionArg => "function arguments:",
                WhenBranch => unreachable!("all patterns are allowed in a When"),
            };

            let suggestion = vec![
                alloc.reflow(
                    "Patterns like this don't cover all possible shapes of the input type. Use a ",
                ),
                alloc.keyword("when"),
                alloc.reflow(" ... "),
                alloc.keyword("is"),
                alloc.reflow(" instead."),
            ];

            alloc.stack(vec![
                alloc
                    .reflow("This pattern is not allowed in ")
                    .append(alloc.reflow(this_thing)),
                alloc.region(region),
                alloc.concat(suggestion),
            ])
        }
        Problem::ShadowingInAnnotation {
            original_region,
            shadow,
        } => pretty_runtime_error(
            alloc,
            RuntimeError::Shadowing {
                original_region,
                shadow,
            },
        ),
        Problem::CyclicAlias(symbol, region, others) => {
            let (doc, title) = crate::error::r#type::cyclic_alias(alloc, symbol, region, others);

            return Report {
                filename,
                title,
                doc,
            };
        }
        Problem::PhantomTypeArgument {
            alias,
            variable_region,
            variable_name,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("The "),
                alloc.type_variable(variable_name),
                alloc.reflow(" type variable is not used in the "),
                alloc.symbol_unqualified(alias),
                alloc.reflow(" alias definition:"),
            ]),
            alloc.region(variable_region),
            alloc.reflow("Roc does not allow unused type parameters!"),
            // TODO add link to this guide section
            alloc.hint().append(alloc.reflow(
                "If you want an unused type parameter (a so-called \"phantom type\"), \
                read the guide section on phantom data.",
            )),
        ]),
        Problem::DuplicateRecordFieldValue {
            field_name,
            field_region,
            record_region,
            replaced_region,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("This record defines the "),
                alloc.record_field(field_name.clone()),
                alloc.reflow(" field twice!"),
            ]),
            alloc.region_all_the_things(
                record_region,
                replaced_region,
                field_region,
                Annotation::Error,
            ),
            alloc.reflow("In the rest of the program, I will only use the latter definition:"),
            alloc.region_all_the_things(
                record_region,
                field_region,
                field_region,
                Annotation::TypoSuggestion,
            ),
            alloc.concat(vec![
                alloc.reflow("For clarity, remove the previous "),
                alloc.record_field(field_name),
                alloc.reflow(" definitions from this record."),
            ]),
        ]),
        Problem::DuplicateRecordFieldType {
            field_name,
            field_region,
            record_region,
            replaced_region,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("This record type defines the "),
                alloc.record_field(field_name.clone()),
                alloc.reflow(" field twice!"),
            ]),
            alloc.region_all_the_things(
                record_region,
                replaced_region,
                field_region,
                Annotation::Error,
            ),
            alloc.reflow("In the rest of the program, I will only use the latter definition:"),
            alloc.region_all_the_things(
                record_region,
                field_region,
                field_region,
                Annotation::TypoSuggestion,
            ),
            alloc.concat(vec![
                alloc.reflow("For clarity, remove the previous "),
                alloc.record_field(field_name),
                alloc.reflow(" definitions from this record type."),
            ]),
        ]),
        Problem::DuplicateTag {
            tag_name,
            tag_union_region,
            tag_region,
            replaced_region,
        } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("This tag union type defines the "),
                alloc.tag_name(tag_name.clone()),
                alloc.reflow(" tag twice!"),
            ]),
            alloc.region_all_the_things(
                tag_union_region,
                replaced_region,
                tag_region,
                Annotation::Error,
            ),
            alloc.reflow("In the rest of the program, I will only use the latter definition:"),
            alloc.region_all_the_things(
                tag_union_region,
                tag_region,
                tag_region,
                Annotation::TypoSuggestion,
            ),
            alloc.concat(vec![
                alloc.reflow("For clarity, remove the previous "),
                alloc.tag_name(tag_name),
                alloc.reflow(" definitions from this tag union type."),
            ]),
        ]),
        Problem::SignatureDefMismatch {
            ref annotation_pattern,
            ref def_pattern,
        } => alloc.stack(vec![
            alloc.reflow("This annotation does not match the definition immediately following it:"),
            alloc.region(Region::span_across(annotation_pattern, def_pattern)),
            alloc.reflow("Is it a typo? If not, put either a newline or comment between them."),
        ]),
        Problem::InvalidAliasRigid { alias_name, region } => alloc.stack(vec![
            alloc.concat(vec![
                alloc.reflow("This pattern in the definition of "),
                alloc.symbol_unqualified(alias_name),
                alloc.reflow(" is not what I expect:"),
            ]),
            alloc.region(region),
            alloc.concat(vec![
                alloc.reflow("Only type variables like "),
                alloc.type_variable("a".into()),
                alloc.reflow(" or "),
                alloc.type_variable("value".into()),
                alloc.reflow(" can occur in this position."),
            ]),
        ]),
        Problem::RuntimeError(runtime_error) => pretty_runtime_error(alloc, runtime_error),
    };

    Report {
        title: "SYNTAX PROBLEM".to_string(),
        filename,
        doc,
    }
}

fn pretty_runtime_error<'b>(
    alloc: &'b RocDocAllocator<'b>,
    runtime_error: RuntimeError,
) -> RocDocBuilder<'b> {
    match runtime_error {
        RuntimeError::Shadowing {
            original_region,
            shadow,
        } => {
            let line = r#"Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."#;

            alloc.stack(vec![
                alloc
                    .text("The ")
                    .append(alloc.ident(shadow.value))
                    .append(alloc.reflow(" name is first defined here:")),
                alloc.region(original_region),
                alloc.reflow("But then it's defined a second time here:"),
                alloc.region(shadow.region),
                alloc.reflow(line),
            ])
        }

        RuntimeError::LookupNotInScope(loc_name, options) => {
            not_found(alloc, loc_name.region, &loc_name.value, "value", options)
        }
        RuntimeError::CircularDef(mut symbols, regions) => {
            let first = symbols.remove(0);

            if symbols.is_empty() {
                alloc
                    .reflow("The ")
                    .append(alloc.symbol_unqualified(first))
                    .append(alloc.reflow(
                        " value is defined directly in terms of itself, causing an infinite loop.",
                    ))
            // TODO "are you trying to mutate a variable?
            // TODO hint?
            } else {
                alloc.stack(vec![
                    alloc
                        .reflow("The ")
                        .append(alloc.symbol_unqualified(first))
                        .append(
                            alloc.reflow(" definition is causing a very tricky infinite loop:"),
                        ),
                    alloc.region(regions[0].0),
                    alloc
                        .reflow("The ")
                        .append(alloc.symbol_unqualified(first))
                        .append(alloc.reflow(
                            " value depends on itself through the following chain of definitions:",
                        )),
                    crate::report::cycle(
                        alloc,
                        4,
                        alloc.symbol_unqualified(first),
                        symbols
                            .into_iter()
                            .map(|s| alloc.symbol_unqualified(s))
                            .collect::<Vec<_>>(),
                    ),
                    // TODO hint?
                ])
            }
        }
        RuntimeError::MalformedPattern(problem, region) => {
            use roc_parse::ast::Base;
            use roc_problem::can::MalformedPatternProblem::*;

            let name = match problem {
                MalformedInt => " integer ",
                MalformedFloat => " float ",
                MalformedBase(Base::Hex) => " hex integer ",
                MalformedBase(Base::Binary) => " binary integer ",
                MalformedBase(Base::Octal) => " octal integer ",
                MalformedBase(Base::Decimal) => " integer ",
                Unknown => " ",
                QualifiedIdentifier => " qualified ",
            };

            let hint = match problem {
                MalformedInt | MalformedFloat | MalformedBase(_) => alloc
                    .hint()
                    .append(alloc.reflow("Learn more about number literals at TODO")),
                Unknown => alloc.nil(),
                QualifiedIdentifier => alloc.hint().append(
                    alloc.reflow("In patterns, only private and global tags can be qualified"),
                ),
            };

            alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This"),
                    alloc.text(name),
                    alloc.reflow("pattern is malformed:"),
                ]),
                alloc.region(region),
                hint,
            ])
        }
        RuntimeError::UnsupportedPattern(_) => {
            todo!("unsupported patterns are currently not parsed!")
        }
        RuntimeError::ValueNotExposed { .. } => todo!("value not exposed"),
        RuntimeError::ModuleNotImported { .. } => todo!("module not imported"),
        RuntimeError::InvalidPrecedence(_, _) => {
            // do nothing, reported with PrecedenceProblem
            unreachable!()
        }
        RuntimeError::MalformedIdentifier(_, _) => {
            todo!("malformed identifier, currently gives a parse error and thus is unreachable")
        }
        RuntimeError::MalformedClosure(_) => todo!(""),
        RuntimeError::InvalidFloat(sign @ FloatErrorKind::PositiveInfinity, region, _raw_str)
        | RuntimeError::InvalidFloat(sign @ FloatErrorKind::NegativeInfinity, region, _raw_str) => {
            let hint = alloc
                .hint()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            let big_or_small = if let FloatErrorKind::PositiveInfinity = sign {
                "big"
            } else {
                "small"
            };

            alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This float literal is too "),
                    alloc.text(big_or_small),
                    alloc.reflow(":"),
                ]),
                alloc.region(region),
                alloc.concat(vec![
                    alloc.reflow("Roc uses signed 64-bit floating points, allowing values between"),
                    alloc.text(format!("{:e}", f64::MIN)),
                    alloc.reflow(" and "),
                    alloc.text(format!("{:e}", f64::MAX)),
                ]),
                hint,
            ])
        }
        RuntimeError::InvalidFloat(FloatErrorKind::Error, region, _raw_str) => {
            let hint = alloc
                .hint()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This float literal contains an invalid digit:"),
                ]),
                alloc.region(region),
                alloc.concat(vec![
                    alloc.reflow("Floating point literals can only contain the digits 0-9, or use scientific notation 10e4"),
                ]),
                hint,
            ])
        }
        RuntimeError::InvalidInt(IntErrorKind::Empty, _base, _region, _raw_str) => {
            unreachable!("would never parse an empty int literal")
        }
        RuntimeError::InvalidInt(IntErrorKind::InvalidDigit, base, region, _raw_str) => {
            use roc_parse::ast::Base::*;

            let name = match base {
                Decimal => "integer",
                Octal => "octal integer",
                Hex => "hex integer",
                Binary => "binary integer",
            };

            let plurals = match base {
                Decimal => "Integer literals",
                Octal => "Octal (base-8) integer literals",
                Hex => "Hexadecimal (base-16) integer literals",
                Binary => "Binary (base-2) integer literals",
            };

            let charset = match base {
                Decimal => "0-9",
                Octal => "0-7",
                Hex => "0-9, a-f and A-F",
                Binary => "0 and 1",
            };

            let hint = alloc
                .hint()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This "),
                    alloc.text(name),
                    alloc.reflow(" literal contains an invalid digit:"),
                ]),
                alloc.region(region),
                alloc.concat(vec![
                    alloc.text(plurals),
                    alloc.reflow(" can only contain the digits "),
                    alloc.text(charset),
                    alloc.text("."),
                ]),
                hint,
            ])
        }
        RuntimeError::InvalidInt(error_kind @ IntErrorKind::Underflow, _base, region, _raw_str)
        | RuntimeError::InvalidInt(error_kind @ IntErrorKind::Overflow, _base, region, _raw_str) => {
            let big_or_small = if let IntErrorKind::Underflow = error_kind {
                "small"
            } else {
                "big"
            };

            let hint = alloc
                .hint()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This integer literal is too "),
                    alloc.text(big_or_small),
                    alloc.reflow(":"),
                ]),
                alloc.region(region),
                alloc.reflow("Roc uses signed 64-bit integers, allowing values between −9_223_372_036_854_775_808 and 9_223_372_036_854_775_807."),
                hint,
            ])
        }
        RuntimeError::NoImplementation => todo!("no implementation, unreachable"),
    }
}

fn not_found<'b>(
    alloc: &'b RocDocAllocator<'b>,
    region: roc_region::all::Region,
    name: &str,
    thing: &'b str,
    options: MutSet<Box<str>>,
) -> RocDocBuilder<'b> {
    use crate::error::r#type::suggest;

    let mut suggestions = suggest::sort(name, options.iter().map(|v| v.as_ref()).collect());
    suggestions.truncate(4);

    let default_no = alloc.concat(vec![
        alloc.reflow("Is there an "),
        alloc.keyword("import"),
        alloc.reflow(" or "),
        alloc.keyword("exposing"),
        alloc.reflow(" missing up-top"),
    ]);

    let default_yes = alloc.reflow("these names seem close though:");

    let to_details = |no_suggestion_details, yes_suggestion_details| {
        if suggestions.is_empty() {
            no_suggestion_details
        } else {
            alloc.stack(vec![
                yes_suggestion_details,
                alloc
                    .vcat(suggestions.into_iter().map(|v| alloc.string(v.to_string())))
                    .indent(4),
            ])
        }
    };

    alloc.stack(vec![
        alloc.concat(vec![
            alloc.reflow("I cannot find a `"),
            alloc.string(name.to_string()),
            alloc.reflow("` "),
            alloc.reflow(thing),
        ]),
        alloc.region(region),
        to_details(default_no, default_yes),
    ])
}
