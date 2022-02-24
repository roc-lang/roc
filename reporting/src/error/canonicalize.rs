use roc_collections::all::MutSet;
use roc_module::ident::{Ident, Lowercase, ModuleName};
use roc_problem::can::PrecedenceProblem::BothNonAssociative;
use roc_problem::can::{BadPattern, FloatErrorKind, IntErrorKind, Problem, RuntimeError};
use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Loc, Region};
use std::path::PathBuf;

use crate::error::r#type::suggest;
use crate::report::{Annotation, Report, RocDocAllocator, RocDocBuilder, Severity};
use ven_pretty::DocAllocator;

const SYNTAX_PROBLEM: &str = "SYNTAX PROBLEM";
const NAMING_PROBLEM: &str = "NAMING PROBLEM";
const UNRECOGNIZED_NAME: &str = "UNRECOGNIZED NAME";
const UNUSED_DEF: &str = "UNUSED DEFINITION";
const UNUSED_IMPORT: &str = "UNUSED IMPORT";
const UNUSED_ALIAS_PARAM: &str = "UNUSED TYPE ALIAS PARAMETER";
const UNUSED_ARG: &str = "UNUSED ARGUMENT";
const MISSING_DEFINITION: &str = "MISSING DEFINITION";
const UNKNOWN_GENERATES_WITH: &str = "UNKNOWN GENERATES FUNCTION";
const DUPLICATE_FIELD_NAME: &str = "DUPLICATE FIELD NAME";
const DUPLICATE_TAG_NAME: &str = "DUPLICATE TAG NAME";
const INVALID_UNICODE: &str = "INVALID UNICODE";
const CIRCULAR_DEF: &str = "CIRCULAR DEFINITION";
const DUPLICATE_NAME: &str = "DUPLICATE NAME";
const VALUE_NOT_EXPOSED: &str = "NOT EXPOSED";
const MODULE_NOT_IMPORTED: &str = "MODULE NOT IMPORTED";
const NESTED_DATATYPE: &str = "NESTED DATATYPE";
const CONFLICTING_NUMBER_SUFFIX: &str = "CONFLICTING NUMBER SUFFIX";
const NUMBER_OVERFLOWS_SUFFIX: &str = "NUMBER OVERFLOWS SUFFIX";
const NUMBER_UNDERFLOWS_SUFFIX: &str = "NUMBER UNDERFLOWS SUFFIX";
const OPAQUE_NOT_DEFINED: &str = "OPAQUE NOT DEFINED";
const OPAQUE_DECLARED_OUTSIDE_SCOPE: &str = "OPAQUE DECLARED OUTSIDE SCOPE";

pub fn can_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    problem: Problem,
) -> Report<'b> {
    let doc;
    let title;
    let severity;

    match problem {
        Problem::UnusedDef(symbol, region) => {
            let line =
                r#" then remove it so future readers of your code don't wonder why it is there."#;

            doc = alloc.stack(vec![
                alloc
                    .symbol_unqualified(symbol)
                    .append(alloc.reflow(" is not used anywhere in your code.")),
                alloc.region(lines.convert_region(region)),
                alloc
                    .reflow("If you didn't intend on using ")
                    .append(alloc.symbol_unqualified(symbol))
                    .append(alloc.reflow(line)),
            ]);

            title = UNUSED_DEF.to_string();
            severity = Severity::Warning;
        }
        Problem::UnusedImport(module_id, region) => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("Nothing from "),
                    alloc.module(module_id),
                    alloc.reflow(" is used in this module."),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc.reflow("Since "),
                    alloc.module(module_id),
                    alloc.reflow(" isn't used, you don't need to import it."),
                ]),
            ]);

            title = UNUSED_IMPORT.to_string();
            severity = Severity::Warning;
        }
        Problem::ExposedButNotDefined(symbol) => {
            doc = alloc.stack(vec![
                alloc.symbol_unqualified(symbol).append(
                    alloc.reflow(" is listed as exposed, but it isn't defined in this module."),
                ),
                alloc
                    .reflow("You can fix this by adding a definition for ")
                    .append(alloc.symbol_unqualified(symbol))
                    .append(alloc.reflow(", or by removing it from "))
                    .append(alloc.keyword("exposes"))
                    .append(alloc.reflow(".")),
            ]);

            title = MISSING_DEFINITION.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::UnknownGeneratesWith(loc_ident) => {
            doc = alloc.stack(vec![
                alloc
                    .reflow("I don't know how to generate the ")
                    .append(alloc.ident(loc_ident.value))
                    .append(alloc.reflow(" function.")),
                alloc.region(lines.convert_region(loc_ident.region)),
                alloc
                    .reflow("Only specific functions like `after` and `map` can be generated.")
                    .append(alloc.reflow("Learn more about hosted modules at TODO.")),
            ]);

            title = UNKNOWN_GENERATES_WITH.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::UnusedArgument(closure_symbol, argument_symbol, region) => {
            let line = "\". Adding an underscore at the start of a variable name is a way of saying that the variable is not used.";

            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.symbol_unqualified(closure_symbol),
                    alloc.reflow(" doesn't use "),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.text("."),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc.reflow("If you don't need "),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.reflow(", then you can just remove it. However, if you really do need "),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.reflow(" as an argument of "),
                    alloc.symbol_unqualified(closure_symbol),
                    alloc.reflow(", prefix it with an underscore, like this: \"_"),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.reflow(line),
                ]),
            ]);

            title = UNUSED_ARG.to_string();
            severity = Severity::Warning;
        }
        Problem::PrecedenceProblem(BothNonAssociative(region, left_bin_op, right_bin_op)) => {
            doc = alloc.stack(vec![
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
                alloc.region(lines.convert_region(region)),
            ]);

            title = SYNTAX_PROBLEM.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::UnsupportedPattern(BadPattern::UnderscoreInDef, region) => {
            doc = alloc.stack(vec![
                alloc.reflow("Underscore patterns are not allowed in definitions"),
                alloc.region(lines.convert_region(region)),
            ]);

            title = SYNTAX_PROBLEM.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::UnsupportedPattern(BadPattern::Unsupported(pattern_type), region) => {
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

            doc = alloc.stack(vec![
                alloc
                    .reflow("This pattern is not allowed in ")
                    .append(alloc.reflow(this_thing)),
                alloc.region(lines.convert_region(region)),
                alloc.concat(suggestion),
            ]);

            title = SYNTAX_PROBLEM.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::ShadowingInAnnotation {
            original_region,
            shadow,
        } => {
            doc = report_shadowing(alloc, lines, original_region, shadow);

            title = DUPLICATE_NAME.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::CyclicAlias(symbol, region, others) => {
            let answer = crate::error::r#type::cyclic_alias(alloc, lines, symbol, region, others);

            doc = answer.0;
            title = answer.1;
            severity = Severity::RuntimeError;
        }
        Problem::PhantomTypeArgument {
            typ: alias,
            variable_region,
            variable_name,
        } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("The "),
                    alloc.type_variable(variable_name),
                    alloc.reflow(" type parameter is not used in the "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" alias definition:"),
                ]),
                alloc.region(lines.convert_region(variable_region)),
                alloc.reflow("Roc does not allow unused type alias parameters!"),
                // TODO add link to this guide section
                alloc.tip().append(alloc.reflow(
                    "If you want an unused type parameter (a so-called \"phantom type\"), \
                read the guide section on phantom values.",
                )),
            ]);

            title = UNUSED_ALIAS_PARAM.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::BadRecursion(entries) => {
            doc = to_circular_def_doc(alloc, lines, &entries);
            title = CIRCULAR_DEF.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::DuplicateRecordFieldValue {
            field_name,
            field_region,
            record_region,
            replaced_region,
        } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This record defines the "),
                    alloc.record_field(field_name.clone()),
                    alloc.reflow(" field twice!"),
                ]),
                alloc.region_all_the_things(
                    lines.convert_region(record_region),
                    lines.convert_region(replaced_region),
                    lines.convert_region(field_region),
                    Annotation::Error,
                ),
                alloc.reflow(r"In the rest of the program, I will only use the latter definition:"),
                alloc.region_all_the_things(
                    lines.convert_region(record_region),
                    lines.convert_region(field_region),
                    lines.convert_region(field_region),
                    Annotation::TypoSuggestion,
                ),
                alloc.concat(vec![
                    alloc.reflow("For clarity, remove the previous "),
                    alloc.record_field(field_name),
                    alloc.reflow(" definitions from this record."),
                ]),
            ]);

            title = DUPLICATE_FIELD_NAME.to_string();
            severity = Severity::Warning;
        }
        Problem::InvalidOptionalValue {
            field_name,
            field_region,
            record_region,
        } => {
            return to_invalid_optional_value_report(
                alloc,
                lines,
                filename,
                field_name,
                field_region,
                record_region,
            );
        }
        Problem::DuplicateRecordFieldType {
            field_name,
            field_region,
            record_region,
            replaced_region,
        } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This record type defines the "),
                    alloc.record_field(field_name.clone()),
                    alloc.reflow(" field twice!"),
                ]),
                alloc.region_all_the_things(
                    lines.convert_region(record_region),
                    lines.convert_region(replaced_region),
                    lines.convert_region(field_region),
                    Annotation::Error,
                ),
                alloc.reflow("In the rest of the program, I will only use the latter definition:"),
                alloc.region_all_the_things(
                    lines.convert_region(record_region),
                    lines.convert_region(field_region),
                    lines.convert_region(field_region),
                    Annotation::TypoSuggestion,
                ),
                alloc.concat(vec![
                    alloc.reflow("For clarity, remove the previous "),
                    alloc.record_field(field_name),
                    alloc.reflow(" definitions from this record type."),
                ]),
            ]);

            title = DUPLICATE_FIELD_NAME.to_string();
            severity = Severity::Warning;
        }
        Problem::DuplicateTag {
            tag_name,
            tag_union_region,
            tag_region,
            replaced_region,
        } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This tag union type defines the "),
                    alloc.tag_name(tag_name.clone()),
                    alloc.reflow(" tag twice!"),
                ]),
                alloc.region_all_the_things(
                    lines.convert_region(tag_union_region),
                    lines.convert_region(replaced_region),
                    lines.convert_region(tag_region),
                    Annotation::Error,
                ),
                alloc.reflow("In the rest of the program, I will only use the latter definition:"),
                alloc.region_all_the_things(
                    lines.convert_region(tag_union_region),
                    lines.convert_region(tag_region),
                    lines.convert_region(tag_region),
                    Annotation::TypoSuggestion,
                ),
                alloc.concat(vec![
                    alloc.reflow("For clarity, remove the previous "),
                    alloc.tag_name(tag_name),
                    alloc.reflow(" definitions from this tag union type."),
                ]),
            ]);

            title = DUPLICATE_TAG_NAME.to_string();
            severity = Severity::Warning;
        }
        Problem::SignatureDefMismatch {
            ref annotation_pattern,
            ref def_pattern,
        } => {
            doc = alloc.stack(vec![
                alloc.reflow(
                    "This annotation does not match the definition immediately following it:",
                ),
                alloc.region(
                    lines.convert_region(Region::span_across(annotation_pattern, def_pattern)),
                ),
                alloc.reflow("Is it a typo? If not, put either a newline or comment between them."),
            ]);

            title = NAMING_PROBLEM.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::InvalidAliasRigid {
            alias_name: type_name,
            region,
        } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This pattern in the definition of "),
                    alloc.symbol_unqualified(type_name),
                    alloc.reflow(" is not what I expect:"),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc.reflow("Only type variables like "),
                    alloc.type_variable("a".into()),
                    alloc.reflow(" or "),
                    alloc.type_variable("value".into()),
                    alloc.reflow(" can occur in this position."),
                ]),
            ]);

            title = SYNTAX_PROBLEM.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::InvalidHexadecimal(region) => {
            doc = alloc.stack(vec![
                alloc.reflow("This unicode code point is invalid:"),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc.reflow(r"I was expecting a hexadecimal number, like "),
                    alloc.parser_suggestion("\\u(1100)"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("\\u(00FF)"),
                    alloc.text("."),
                ]),
                alloc.reflow(r"Learn more about working with unicode in roc at TODO"),
            ]);

            title = INVALID_UNICODE.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::InvalidUnicodeCodePt(region) => {
            doc = alloc.stack(vec![
                alloc.reflow("This unicode code point is invalid:"),
                alloc.region(lines.convert_region(region)),
                alloc.reflow("Learn more about working with unicode in roc at TODO"),
            ]);

            title = INVALID_UNICODE.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::InvalidInterpolation(region) => {
            doc = alloc.stack(vec![
                alloc.reflow("This string interpolation is invalid:"),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc.reflow(r"I was expecting an identifier, like "),
                    alloc.parser_suggestion("\\u(message)"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("\\u(LoremIpsum.text)"),
                    alloc.text("."),
                ]),
                alloc.reflow(r"Learn more about string interpolation at TODO"),
            ]);

            title = SYNTAX_PROBLEM.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::RuntimeError(runtime_error) => {
            let answer = pretty_runtime_error(alloc, lines, runtime_error);

            doc = answer.0;
            title = answer.1.to_string();
            severity = Severity::RuntimeError;
        }
        Problem::NestedDatatype {
            alias,
            def_region,
            differing_recursion_region,
        } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" is a nested datatype. Here is one recursive usage of it:"),
                ]),
                alloc.region(lines.convert_region(differing_recursion_region)),
                alloc.concat(vec![
                    alloc.reflow("But recursive usages of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" must match its definition:"),
                ]),
                alloc.region(lines.convert_region(def_region)),
                alloc.reflow("Nested datatypes are not supported in Roc."),
                alloc.concat(vec![
                    alloc.hint("Consider rewriting the definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.text(" to use the recursive type with the same arguments."),
                ]),
            ]);

            title = NESTED_DATATYPE.to_string();
            severity = Severity::RuntimeError;
        }
    };

    Report {
        title,
        filename,
        doc,
        severity,
    }
}

fn to_invalid_optional_value_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    field_name: Lowercase,
    field_region: Region,
    record_region: Region,
) -> Report<'b> {
    let doc = to_invalid_optional_value_report_help(
        alloc,
        lines,
        field_name,
        field_region,
        record_region,
    );

    Report {
        title: "BAD OPTIONAL VALUE".to_string(),
        filename,
        doc,
        severity: Severity::RuntimeError,
    }
}

fn to_invalid_optional_value_report_help<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    field_name: Lowercase,
    field_region: Region,
    record_region: Region,
) -> RocDocBuilder<'b> {
    alloc.stack(vec![
        alloc.concat(vec![
            alloc.reflow("This record uses an optional value for the "),
            alloc.record_field(field_name),
            alloc.reflow(" field in an incorrect context!"),
        ]),
        alloc.region_all_the_things(
            lines.convert_region(record_region),
            lines.convert_region(field_region),
            lines.convert_region(field_region),
            Annotation::Error,
        ),
        alloc.reflow(r"You can only use optional values in record destructuring, like:"),
        alloc
            .reflow(r"{ answer ? 42, otherField } = myRecord")
            .indent(4),
    ])
}

fn to_bad_ident_expr_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    bad_ident: roc_parse::ident::BadIdent,
    surroundings: Region,
) -> RocDocBuilder<'b> {
    use roc_parse::ident::BadIdent::*;

    match bad_ident {
        Start(_) | Space(_, _) => unreachable!("these are handled in the parser"),
        WeirdDotAccess(pos) | StrayDot(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack(vec![
                alloc.reflow(r"I trying to parse a record field access here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region),
                alloc.concat(vec![
                    alloc.reflow("So I expect to see a lowercase letter next, like "),
                    alloc.parser_suggestion(".name"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion(".height"),
                    alloc.reflow("."),
                ]),
            ])
        }

        WeirdAccessor(_pos) => alloc.stack(vec![
            alloc.reflow("I am very confused by this field access"),
            alloc.region(lines.convert_region(surroundings)),
            alloc.concat(vec![
                alloc.reflow("It looks like a field access on an accessor. I parse"),
                alloc.parser_suggestion(".client.name"),
                alloc.reflow(" as "),
                alloc.parser_suggestion("(.client).name"),
                alloc.reflow(". Maybe use an anonymous function like "),
                alloc.parser_suggestion("(\\r -> r.client.name)"),
                alloc.reflow(" instead"),
                alloc.reflow("?"),
            ]),
        ]),

        WeirdDotQualified(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack(vec![
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region),
                alloc.concat(vec![
                    alloc.reflow("I was expecting to see an identifier next, like "),
                    alloc.parser_suggestion("height"),
                    alloc.reflow(". A complete qualified name looks something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("."),
                ]),
            ])
        }
        QualifiedTag(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack(vec![
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region),
                alloc.concat(vec![
                    alloc.reflow(r"This looks like a qualified tag name to me, "),
                    alloc.reflow(r"but tags cannot be qualified! "),
                    alloc.reflow(r"Maybe you wanted a qualified name, something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("?"),
                ]),
            ])
        }

        Underscore(pos) => {
            let region = Region::new(surroundings.start(), pos);
            alloc.stack(vec![
                alloc.reflow("Underscores are not allowed in identifier names:"),
                alloc.region_with_subregion(
                    lines.convert_region(surroundings),
                    lines.convert_region(region),
                ),
                alloc.concat(vec![alloc.reflow(
                    r"I recommend using camelCase, it is the standard in the Roc ecosystem.",
                )]),
            ])
        }

        BadPrivateTag(pos) | BadOpaqueRef(pos) => {
            use BadIdentNext::*;
            let kind = if matches!(bad_ident, BadPrivateTag(..)) {
                "a private tag"
            } else {
                "an opaque reference"
            };
            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                LowercaseAccess(width) => {
                    let region = Region::new(pos, pos.bump_column(width));
                    alloc.stack(vec![
                        alloc.reflow("I am very confused by this field access:"),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            lines.convert_region(region),
                        ),
                        alloc.concat(vec![
                            alloc.reflow(r"It looks like a record field access on "),
                            alloc.reflow(kind),
                            alloc.text("."),
                        ]),
                    ])
                }
                UppercaseAccess(width) => {
                    let region = Region::new(pos, pos.bump_column(width));
                    alloc.stack(vec![
                        alloc.reflow("I am very confused by this expression:"),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            lines.convert_region(region),
                        ),
                        alloc.concat(vec![
                            alloc.reflow(r"Looks like "),
                            alloc.reflow(kind),
                            alloc.reflow(" is treated like a module name. "),
                            alloc.reflow(r"Maybe you wanted a qualified name, like "),
                            alloc.parser_suggestion("Json.Decode.string"),
                            alloc.text("?"),
                        ]),
                    ])
                }
                Other(Some(c)) if c.is_lowercase() => {
                    let region =
                        Region::new(surroundings.start().bump_column(1), pos.bump_column(1));
                    alloc.stack(vec![
                        alloc.concat(vec![
                            alloc.reflow("I am trying to parse "),
                            alloc.reflow(kind),
                            alloc.reflow(" here:"),
                        ]),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            lines.convert_region(region),
                        ),
                        alloc.concat(vec![
                            alloc.reflow(r"But after the "),
                            alloc.keyword("@"),
                            alloc.reflow(r" symbol I found a lowercase letter. "),
                            alloc.reflow(r"All tag names (global and private)"),
                            alloc.reflow(r" must start with an uppercase letter, like "),
                            alloc.parser_suggestion("@UUID"),
                            alloc.reflow(" or "),
                            alloc.parser_suggestion("@Secrets"),
                            alloc.reflow("."),
                        ]),
                    ])
                }
                other => todo!("{:?}", other),
            }
        }
    }
}

fn to_bad_ident_pattern_report<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    bad_ident: roc_parse::ident::BadIdent,
    surroundings: Region,
) -> RocDocBuilder<'b> {
    use roc_parse::ident::BadIdent::*;

    match bad_ident {
        Start(_) | Space(_, _) => unreachable!("these are handled in the parser"),
        WeirdDotAccess(pos) | StrayDot(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack(vec![
                alloc.reflow(r"I trying to parse a record field accessor here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region),
                alloc.concat(vec![
                    alloc.reflow("Something like "),
                    alloc.parser_suggestion(".name"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion(".height"),
                    alloc.reflow(" that accesses a value from a record."),
                ]),
            ])
        }

        WeirdAccessor(_pos) => alloc.stack(vec![
            alloc.reflow("I am very confused by this field access"),
            alloc.region(lines.convert_region(surroundings)),
            alloc.concat(vec![
                alloc.reflow("It looks like a field access on an accessor. I parse"),
                alloc.parser_suggestion(".client.name"),
                alloc.reflow(" as "),
                alloc.parser_suggestion("(.client).name"),
                alloc.reflow(". Maybe use an anonymous function like "),
                alloc.parser_suggestion("(\\r -> r.client.name)"),
                alloc.reflow(" instead"),
                alloc.reflow("?"),
            ]),
        ]),

        WeirdDotQualified(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack(vec![
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region),
                alloc.concat(vec![
                    alloc.reflow("I was expecting to see an identifier next, like "),
                    alloc.parser_suggestion("height"),
                    alloc.reflow(". A complete qualified name looks something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("."),
                ]),
            ])
        }
        QualifiedTag(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack(vec![
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region),
                alloc.concat(vec![
                    alloc.reflow(r"This looks like a qualified tag name to me, "),
                    alloc.reflow(r"but tags cannot be qualified! "),
                    alloc.reflow(r"Maybe you wanted a qualified name, something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("?"),
                ]),
            ])
        }

        Underscore(pos) => {
            let region = Region::from_pos(pos.sub(1));

            alloc.stack(vec![
                alloc.reflow("I am trying to parse an identifier here:"),
                alloc.region_with_subregion(
                    lines.convert_region(surroundings),
                    lines.convert_region(region),
                ),
                alloc.concat(vec![alloc.reflow(
                    r"Underscores are not allowed in identifiers. Use camelCase instead!",
                )]),
            ])
        }

        _ => todo!(),
    }
}

#[derive(Debug)]
enum BadIdentNext<'a> {
    LowercaseAccess(u32),
    UppercaseAccess(u32),
    NumberAccess(u32),
    Keyword(&'a str),
    DanglingDot,
    Other(Option<char>),
}

fn what_is_next<'a>(source_lines: &'a [&'a str], pos: LineColumn) -> BadIdentNext<'a> {
    let row_index = pos.line as usize;
    let col_index = pos.column as usize;
    match source_lines.get(row_index) {
        None => BadIdentNext::Other(None),
        Some(line) => {
            let chars = &line[col_index..];
            let mut it = chars.chars();

            match roc_parse::keyword::KEYWORDS
                .iter()
                .find(|keyword| crate::error::parse::starts_with_keyword(chars, keyword))
            {
                Some(keyword) => BadIdentNext::Keyword(keyword),
                None => match it.next() {
                    None => BadIdentNext::Other(None),
                    Some('.') => match it.next() {
                        Some(c) if c.is_lowercase() => {
                            BadIdentNext::LowercaseAccess(2 + till_whitespace(it) as u32)
                        }
                        Some(c) if c.is_uppercase() => {
                            BadIdentNext::UppercaseAccess(2 + till_whitespace(it) as u32)
                        }
                        Some(c) if c.is_ascii_digit() => {
                            BadIdentNext::NumberAccess(2 + till_whitespace(it) as u32)
                        }
                        _ => BadIdentNext::DanglingDot,
                    },
                    Some(c) => BadIdentNext::Other(Some(c)),
                },
            }
        }
    }
}

fn till_whitespace<I>(it: I) -> usize
where
    I: Iterator<Item = char>,
{
    let mut chomped = 0;

    for c in it {
        if c.is_ascii_whitespace() || c == '#' {
            break;
        } else {
            chomped += 1;
            continue;
        }
    }

    chomped
}

fn report_shadowing<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    original_region: Region,
    shadow: Loc<Ident>,
) -> RocDocBuilder<'b> {
    let line = r#"Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."#;

    alloc.stack(vec![
        alloc
            .text("The ")
            .append(alloc.ident(shadow.value))
            .append(alloc.reflow(" name is first defined here:")),
        alloc.region(lines.convert_region(original_region)),
        alloc.reflow("But then it's defined a second time here:"),
        alloc.region(lines.convert_region(shadow.region)),
        alloc.reflow(line),
    ])
}

fn pretty_runtime_error<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    runtime_error: RuntimeError,
) -> (RocDocBuilder<'b>, &'static str) {
    let doc;
    let title;

    match runtime_error {
        RuntimeError::VoidValue => {
            // is used to communicate to the compiler that
            // a branch is unreachable; this should never reach a user
            unreachable!("");
        }

        RuntimeError::UnresolvedTypeVar | RuntimeError::ErroneousType => {
            // only generated during layout generation
            unreachable!("");
        }

        RuntimeError::Shadowing {
            original_region,
            shadow,
        } => {
            doc = report_shadowing(alloc, lines, original_region, shadow);
            title = DUPLICATE_NAME;
        }

        RuntimeError::LookupNotInScope(loc_name, options) => {
            doc = not_found(
                alloc,
                lines,
                loc_name.region,
                &loc_name.value,
                "value",
                options,
            );
            title = UNRECOGNIZED_NAME;
        }
        RuntimeError::CircularDef(entries) => {
            doc = to_circular_def_doc(alloc, lines, &entries);
            title = CIRCULAR_DEF;
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
                BadIdent(bad_ident) => {
                    title = NAMING_PROBLEM;
                    doc = to_bad_ident_pattern_report(alloc, lines, bad_ident, region);

                    return (doc, title);
                }
                Unknown => " ",
                QualifiedIdentifier => " qualified ",
            };

            let tip = match problem {
                MalformedInt | MalformedFloat | MalformedBase(_) => alloc
                    .tip()
                    .append(alloc.reflow("Learn more about number literals at TODO")),
                Unknown | BadIdent(_) => alloc.nil(),
                QualifiedIdentifier => alloc.tip().append(
                    alloc.reflow("In patterns, only private and global tags can be qualified"),
                ),
            };

            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This"),
                    alloc.text(name),
                    alloc.reflow("pattern is malformed:"),
                ]),
                alloc.region(lines.convert_region(region)),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::UnsupportedPattern(_) => {
            todo!("unsupported patterns are currently not parsed!")
        }
        RuntimeError::ValueNotExposed {
            module_name,
            ident,
            region,
            exposed_values,
        } => {
            let mut suggestions = suggest::sort(ident.as_ref(), exposed_values);
            suggestions.truncate(4);

            let did_you_mean = if suggestions.is_empty() {
                alloc.concat(vec![
                    alloc.reflow("In fact, it looks like "),
                    alloc.module_name(module_name.clone()),
                    alloc.reflow(" doesn't expose any values!"),
                ])
            } else {
                let qualified_suggestions = suggestions
                    .into_iter()
                    .map(|v| alloc.string(module_name.to_string() + "." + v.as_str()));
                alloc.stack(vec![
                    alloc.reflow("Did you mean one of these?"),
                    alloc.vcat(qualified_suggestions).indent(4),
                ])
            };
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("The "),
                    alloc.module_name(module_name),
                    alloc.reflow(" module does not expose `"),
                    alloc.string(ident.to_string()),
                    alloc.reflow("`:"),
                ]),
                alloc.region(lines.convert_region(region)),
                did_you_mean,
            ]);

            title = VALUE_NOT_EXPOSED;
        }

        RuntimeError::ModuleNotImported {
            module_name,
            imported_modules,
            region,
        } => {
            doc = module_not_found(alloc, lines, region, &module_name, imported_modules);

            title = MODULE_NOT_IMPORTED;
        }
        RuntimeError::InvalidPrecedence(_, _) => {
            // do nothing, reported with PrecedenceProblem
            unreachable!();
        }
        RuntimeError::MalformedIdentifier(_box_str, bad_ident, surroundings) => {
            doc = to_bad_ident_expr_report(alloc, lines, bad_ident, surroundings);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::MalformedTypeName(_box_str, surroundings) => {
            doc = alloc.stack(vec![
                alloc.reflow(r"I am confused by this type name:"),
                alloc.region(lines.convert_region(surroundings)),
                alloc.concat(vec![
                    alloc.reflow("Type names start with an uppercase letter, "),
                    alloc.reflow("and can optionally be qualified by a module name, like "),
                    alloc.parser_suggestion("Bool"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("Http.Request.Request"),
                    alloc.reflow("."),
                ]),
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::MalformedClosure(_) => {
            todo!("");
        }
        RuntimeError::InvalidFloat(sign @ FloatErrorKind::PositiveInfinity, region, _raw_str)
        | RuntimeError::InvalidFloat(sign @ FloatErrorKind::NegativeInfinity, region, _raw_str) => {
            let tip = alloc
                .tip()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            let big_or_small = if let FloatErrorKind::PositiveInfinity = sign {
                "big"
            } else {
                "small"
            };

            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This float literal is too "),
                    alloc.text(big_or_small),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc
                        .reflow("Roc uses signed 64-bit floating points, allowing values between "),
                    alloc.text(format!("{:e}", f64::MIN)),
                    alloc.reflow(" and "),
                    alloc.text(format!("{:e}", f64::MAX)),
                ]),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidFloat(FloatErrorKind::Error, region, _raw_str) => {
            let tip = alloc
                .tip()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This float literal contains an invalid digit:"),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc.reflow("Floating point literals can only contain the digits 0-9, or use scientific notation 10e4, or have a float suffix."),
                ]),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidFloat(FloatErrorKind::IntSuffix, region, _raw_str) => {
            doc = alloc.stack(vec![
                alloc.concat(vec![alloc.reflow(
                    "This number literal is a float, but it has an integer suffix:",
                )]),
                alloc.region(lines.convert_region(region)),
            ]);

            title = CONFLICTING_NUMBER_SUFFIX;
        }
        RuntimeError::InvalidInt(error @ IntErrorKind::InvalidDigit, base, region, _raw_str)
        | RuntimeError::InvalidInt(error @ IntErrorKind::Empty, base, region, _raw_str) => {
            use roc_parse::ast::Base::*;

            let (problem, contains) = if let IntErrorKind::InvalidDigit = error {
                (
                    "an invalid digit",
                    alloc.reflow(" can only contain the digits "),
                )
            } else {
                (
                    "no digits",
                    alloc.reflow(" must contain at least one of the digits "),
                )
            };

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

            let tip = alloc
                .tip()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This "),
                    alloc.text(name),
                    alloc.reflow(" literal contains "),
                    alloc.text(problem),
                    alloc.text(":"),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.concat(vec![
                    alloc.text(plurals),
                    contains,
                    alloc.text(charset),
                    alloc.text(", or have an integer suffix."),
                ]),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidInt(error_kind @ IntErrorKind::Underflow, _base, region, _raw_str)
        | RuntimeError::InvalidInt(error_kind @ IntErrorKind::Overflow, _base, region, _raw_str) => {
            let (big_or_small, info) = if let IntErrorKind::Underflow = error_kind {
                (
                    "small",
                    alloc.concat(vec![
                        alloc.reflow(
                            "The smallest number representable in Roc is the minimum I128 value, ",
                        ),
                        alloc.int_literal(i128::MIN),
                        alloc.text("."),
                    ]),
                )
            } else {
                (
                    "big",
                    alloc.concat(vec![
                        alloc.reflow(
                            "The largest number representable in Roc is the maximum U128 value, ",
                        ),
                        alloc.int_literal(u128::MAX),
                        alloc.text("."),
                    ]),
                )
            };

            let tip = alloc
                .tip()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This integer literal is too "),
                    alloc.text(big_or_small),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region)),
                info,
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidInt(IntErrorKind::FloatSuffix, _base, region, _raw_str) => {
            doc = alloc.stack(vec![
                alloc.concat(vec![alloc.reflow(
                    "This number literal is an integer, but it has a float suffix:",
                )]),
                alloc.region(lines.convert_region(region)),
            ]);

            title = CONFLICTING_NUMBER_SUFFIX;
        }
        RuntimeError::InvalidInt(
            IntErrorKind::OverflowsSuffix {
                suffix_type,
                max_value,
            },
            _base,
            region,
            _raw_str,
        ) => {
            doc = alloc.stack(vec![
                alloc.concat(vec![alloc.reflow(
                    "This integer literal overflows the type indicated by its suffix:",
                )]),
                alloc.region(lines.convert_region(region)),
                alloc.tip().append(alloc.concat(vec![
                    alloc.reflow("The suffix indicates this integer is a "),
                    alloc.type_str(suffix_type),
                    alloc.reflow(", whose maximum value is "),
                    alloc.int_literal(max_value),
                    alloc.reflow("."),
                ])),
            ]);

            title = NUMBER_OVERFLOWS_SUFFIX;
        }
        RuntimeError::InvalidInt(
            IntErrorKind::UnderflowsSuffix {
                suffix_type,
                min_value,
            },
            _base,
            region,
            _raw_str,
        ) => {
            doc = alloc.stack(vec![
                alloc.concat(vec![alloc.reflow(
                    "This integer literal underflows the type indicated by its suffix:",
                )]),
                alloc.region(lines.convert_region(region)),
                alloc.tip().append(alloc.concat(vec![
                    alloc.reflow("The suffix indicates this integer is a "),
                    alloc.type_str(suffix_type),
                    alloc.reflow(", whose minimum value is "),
                    alloc.int_literal(min_value),
                    alloc.reflow("."),
                ])),
            ]);

            title = NUMBER_UNDERFLOWS_SUFFIX;
        }
        RuntimeError::InvalidOptionalValue {
            field_name,
            field_region,
            record_region,
        } => {
            doc = to_invalid_optional_value_report_help(
                alloc,
                lines,
                field_name,
                field_region,
                record_region,
            );

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidRecordUpdate { region } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("This expression cannot be updated"),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region)),
                alloc.reflow("Only variables can be updated with record update syntax."),
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidHexadecimal(region) => {
            todo!(
                "TODO runtime error for an invalid hexadecimal number in a \\u(...) code point at region {:?}",
                region
            );
        }
        RuntimeError::InvalidUnicodeCodePt(region) => {
            todo!(
                "TODO runtime error for an invalid \\u(...) code point at region {:?}",
                region
            );
        }
        RuntimeError::InvalidInterpolation(region) => {
            todo!(
                "TODO runtime error for an invalid string interpolation at region {:?}",
                region
            );
        }
        RuntimeError::NoImplementation | RuntimeError::NoImplementationNamed { .. } => {
            todo!("no implementation, unreachable")
        }
        RuntimeError::NonExhaustivePattern => {
            unreachable!("not currently reported (but can blow up at runtime)")
        }
        RuntimeError::ExposedButNotDefined(symbol) => {
            doc = alloc.stack(vec![alloc
                .symbol_unqualified(symbol)
                .append(alloc.reflow(" was listed as exposed in "))
                .append(alloc.module(symbol.module_id()))
                .append(
                    alloc.reflow(", but it was not defined anywhere in that module."),
                )]);

            title = MISSING_DEFINITION;
        }
        RuntimeError::OpaqueNotDefined {
            usage:
                Loc {
                    region: used_region,
                    value: opaque,
                },
            opaques_in_scope,
            opt_defined_alias,
        } => {
            let mut suggestions = suggest::sort(
                opaque.as_inline_str().as_str(),
                opaques_in_scope.iter().map(|v| v.as_ref()).collect(),
            );
            suggestions.truncate(4);

            let details = if suggestions.is_empty() {
                alloc.note("It looks like there are no opaque types declared in this scope yet!")
            } else {
                let qualified_suggestions =
                    suggestions.into_iter().map(|v| alloc.string(v.to_string()));
                alloc.stack(vec![
                    alloc
                        .tip()
                        .append(alloc.reflow("Did you mean one of these opaque types?")),
                    alloc.vcat(qualified_suggestions).indent(4),
                ])
            };

            let mut stack = vec![
                alloc.concat(vec![
                    alloc.reflow("The opaque type "),
                    alloc.type_str(opaque.as_inline_str().as_str()),
                    alloc.reflow(" referenced here is not defined:"),
                ]),
                alloc.region(lines.convert_region(used_region)),
            ];

            if let Some(defined_alias_region) = opt_defined_alias {
                stack.push(alloc.stack(vec![
                    alloc.note("There is an alias of the same name:"),
                    alloc.region(lines.convert_region(defined_alias_region)),
                ]));
            }

            stack.push(details);

            doc = alloc.stack(stack);

            title = OPAQUE_NOT_DEFINED;
        }
        RuntimeError::OpaqueOutsideScope {
            opaque,
            referenced_region,
            imported_region,
        } => {
            doc = alloc.stack(vec![
                alloc.concat(vec![
                    alloc.reflow("The unwrapped opaque type "),
                    alloc.type_str(opaque.as_inline_str().as_str()),
                    alloc.reflow(" referenced here:"),
                ]),
                alloc.region(lines.convert_region(referenced_region)),
                alloc.reflow("is imported from another module:"),
                alloc.region(lines.convert_region(imported_region)),
                alloc.note(
                    "Opaque types can only be wrapped and unwrapped in the module they are defined in!",
                ),
            ]);

            title = OPAQUE_DECLARED_OUTSIDE_SCOPE;
        }
    }

    (doc, title)
}

fn to_circular_def_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    entries: &[roc_problem::can::CycleEntry],
) -> RocDocBuilder<'b> {
    // TODO "are you trying to mutate a variable?
    // TODO tip?
    match entries {
        [] => unreachable!(),
        [first] => alloc
            .reflow("The ")
            .append(alloc.symbol_unqualified(first.symbol))
            .append(alloc.reflow(
                " value is defined directly in terms of itself, causing an infinite loop.",
            )),
        [first, others @ ..] => {
            alloc.stack(vec![
                alloc
                    .reflow("The ")
                    .append(alloc.symbol_unqualified(first.symbol))
                    .append(alloc.reflow(" definition is causing a very tricky infinite loop:")),
                alloc.region(lines.convert_region(first.symbol_region)),
                alloc
                    .reflow("The ")
                    .append(alloc.symbol_unqualified(first.symbol))
                    .append(alloc.reflow(
                        " value depends on itself through the following chain of definitions:",
                    )),
                crate::report::cycle(
                    alloc,
                    4,
                    alloc.symbol_unqualified(first.symbol),
                    others
                        .iter()
                        .map(|s| alloc.symbol_unqualified(s.symbol))
                        .collect::<Vec<_>>(),
                ),
                // TODO tip?
            ])
        }
    }
}

fn not_found<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    region: roc_region::all::Region,
    name: &Ident,
    thing: &'b str,
    options: MutSet<Box<str>>,
) -> RocDocBuilder<'b> {
    let mut suggestions = suggest::sort(
        name.as_inline_str().as_str(),
        options.iter().map(|v| v.as_ref()).collect(),
    );
    suggestions.truncate(4);

    let default_no = alloc.concat(vec![
        alloc.reflow("Is there an "),
        alloc.keyword("import"),
        alloc.reflow(" or "),
        alloc.keyword("exposing"),
        alloc.reflow(" missing up-top"),
    ]);

    let default_yes = alloc.reflow("Did you mean one of these?");

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
        alloc.region(lines.convert_region(region)),
        to_details(default_no, default_yes),
    ])
}

fn module_not_found<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    region: roc_region::all::Region,
    name: &ModuleName,
    options: MutSet<Box<str>>,
) -> RocDocBuilder<'b> {
    let mut suggestions =
        suggest::sort(name.as_str(), options.iter().map(|v| v.as_ref()).collect());
    suggestions.truncate(4);

    let default_no = alloc.concat(vec![
        alloc.reflow("Is there an "),
        alloc.keyword("import"),
        alloc.reflow(" or "),
        alloc.keyword("exposing"),
        alloc.reflow(" missing up-top"),
    ]);

    let default_yes = alloc
        .reflow("Is there an import missing? Perhaps there is a typo. Did you mean one of these?");

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
            alloc.reflow("The `"),
            alloc.string(name.to_string()),
            alloc.reflow("` module is not imported:"),
        ]),
        alloc.region(lines.convert_region(region)),
        to_details(default_no, default_yes),
    ])
}
