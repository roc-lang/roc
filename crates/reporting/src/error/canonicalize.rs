use roc_collections::all::MutSet;
use roc_module::ident::{Ident, Lowercase, ModuleName};
use roc_module::symbol::DERIVABLE_ABILITIES;
use roc_problem::can::PrecedenceProblem::BothNonAssociative;
use roc_problem::can::{
    BadPattern, CycleEntry, ExtensionTypeKind, FloatErrorKind, IntErrorKind, Problem, RuntimeError,
    ScopeModuleSource, ShadowKind,
};
use roc_problem::Severity;
use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Loc, Region};
use roc_types::types::{AliasKind, EarlyReturnKind};
use std::path::PathBuf;

use crate::error::r#type::suggest;
use crate::report::{to_file_problem_report, Annotation, Report, RocDocAllocator, RocDocBuilder};
use ven_pretty::{text, DocAllocator};

const SYNTAX_PROBLEM: &str = "SYNTAX PROBLEM";
const NAMING_PROBLEM: &str = "NAMING PROBLEM";
const UNRECOGNIZED_NAME: &str = "UNRECOGNIZED NAME";
const UNUSED_DEF: &str = "UNUSED DEFINITION";
const UNUSED_IMPORT: &str = "UNUSED IMPORT";
const IMPORT_NAME_CONFLICT: &str = "IMPORT NAME CONFLICT";
const EXPLICIT_BUILTIN_IMPORT: &str = "EXPLICIT BUILTIN IMPORT";
const UNUSED_ALIAS_PARAM: &str = "UNUSED TYPE ALIAS PARAMETER";
const UNDECLARED_TYPE_VARIABLE: &str = "UNDECLARED TYPE VARIABLE";
const WILDCARD_NOT_ALLOWED: &str = "WILDCARD NOT ALLOWED HERE";
const UNDERSCORE_NOT_ALLOWED: &str = "UNDERSCORE NOT ALLOWED HERE";
const UNUSED_ARG: &str = "UNUSED ARGUMENT";
const MISSING_DEFINITION: &str = "MISSING DEFINITION";
const DUPLICATE_FIELD_NAME: &str = "DUPLICATE FIELD NAME";
const DUPLICATE_TAG_NAME: &str = "DUPLICATE TAG NAME";
const INVALID_UNICODE: &str = "INVALID UNICODE";
pub const CIRCULAR_DEF: &str = "CIRCULAR DEFINITION";
const DUPLICATE_NAME: &str = "DUPLICATE NAME";
const VALUE_NOT_EXPOSED: &str = "NOT EXPOSED";
const MODULE_NOT_IMPORTED: &str = "MODULE NOT IMPORTED";
const INGESTED_FILE_ERROR: &str = "INGESTED FILE ERROR";
const NESTED_DATATYPE: &str = "NESTED DATATYPE";
const CONFLICTING_NUMBER_SUFFIX: &str = "CONFLICTING NUMBER SUFFIX";
const NUMBER_OVERFLOWS_SUFFIX: &str = "NUMBER OVERFLOWS SUFFIX";
const NUMBER_UNDERFLOWS_SUFFIX: &str = "NUMBER UNDERFLOWS SUFFIX";
const OPAQUE_NOT_DEFINED: &str = "OPAQUE TYPE NOT DEFINED";
const OPAQUE_DECLARED_OUTSIDE_SCOPE: &str = "OPAQUE TYPE DECLARED OUTSIDE SCOPE";
const OPAQUE_NOT_APPLIED: &str = "OPAQUE TYPE NOT APPLIED";
const OPAQUE_OVER_APPLIED: &str = "OPAQUE TYPE APPLIED TO TOO MANY ARGS";
const INVALID_EXTENSION_TYPE: &str = "INVALID_EXTENSION_TYPE";
const ABILITY_HAS_TYPE_VARIABLES: &str = "ABILITY HAS TYPE VARIABLES";
const IMPLEMENTS_CLAUSE_IS_NOT_AN_ABILITY: &str = "IMPLEMENTS CLAUSE IS NOT AN ABILITY";
const ILLEGAL_IMPLEMENTS_CLAUSE: &str = "ILLEGAL IMPLEMENTS CLAUSE";
const ABILITY_MEMBER_MISSING_IMPLEMENTS_CLAUSE: &str = "ABILITY MEMBER MISSING IMPLEMENTS CLAUSE";
const ABILITY_MEMBER_BINDS_MULTIPLE_VARIABLES: &str = "ABILITY MEMBER BINDS MULTIPLE VARIABLES";
const ABILITY_NOT_ON_TOPLEVEL: &str = "ABILITY NOT ON TOP-LEVEL";
const SPECIALIZATION_NOT_ON_TOPLEVEL: &str = "SPECIALIZATION NOT ON TOP-LEVEL";
const ABILITY_USED_AS_TYPE: &str = "ABILITY USED AS TYPE";
const ILLEGAL_DERIVE: &str = "ILLEGAL DERIVE";
const IMPLEMENTATION_NOT_FOUND: &str = "IMPLEMENTATION NOT FOUND";
const NOT_AN_ABILITY_MEMBER: &str = "NOT AN ABILITY MEMBER";
const NOT_AN_ABILITY: &str = "NOT AN ABILITY";
const OPTIONAL_ABILITY_IMPLEMENTATION: &str = "OPTIONAL ABILITY IMPLEMENTATION";
const QUALIFIED_ABILITY_IMPLEMENTATION: &str = "QUALIFIED ABILITY IMPLEMENTATION";
const ABILITY_IMPLEMENTATION_NOT_IDENTIFIER: &str = "ABILITY IMPLEMENTATION NOT IDENTIFIER";
const DUPLICATE_IMPLEMENTATION: &str = "DUPLICATE IMPLEMENTATION";
const UNNECESSARY_IMPLEMENTATIONS: &str = "UNNECESSARY IMPLEMENTATIONS";
const INCOMPLETE_ABILITY_IMPLEMENTATION: &str = "INCOMPLETE ABILITY IMPLEMENTATION";
const STATEMENT_AFTER_EXPRESSION: &str = "STATEMENT AFTER EXPRESSION";
const MISSING_EXCLAMATION: &str = "MISSING EXCLAMATION";
const UNNECESSARY_EXCLAMATION: &str = "UNNECESSARY EXCLAMATION";
const EMPTY_TUPLE_TYPE: &str = "EMPTY TUPLE TYPE";
const UNBOUND_TYPE_VARS_IN_AS: &str = "UNBOUND TYPE VARIABLES IN AS";

pub fn can_problem<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    filename: PathBuf,
    problem: Problem,
) -> Report<'b> {
    let doc;
    let title;
    let severity = problem.severity();

    match problem {
        Problem::UnusedDef(symbol, region) => {
            let line =
                r#" then remove it so future readers of your code don't wonder why it is there."#;

            doc = alloc.stack([
                alloc
                    .symbol_unqualified(symbol)
                    .append(alloc.reflow(" is not used anywhere in your code.")),
                alloc.region(lines.convert_region(region), severity),
                alloc
                    .reflow("If you didn't intend on using ")
                    .append(alloc.symbol_unqualified(symbol))
                    .append(alloc.reflow(line)),
            ]);

            title = UNUSED_DEF.to_string();
        }
        Problem::UnusedImport(symbol, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.symbol_qualified(symbol),
                    alloc.reflow(" is not used in this module."),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Since "),
                    alloc.symbol_qualified(symbol),
                    alloc.reflow(" isn't used, you don't need to import it."),
                ]),
            ]);

            title = UNUSED_IMPORT.to_string();
        }
        Problem::UnusedModuleImport(module_id, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.module(module_id),
                    alloc.reflow(" is imported but not used."),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Since "),
                    alloc.module(module_id),
                    alloc.reflow(" isn't used, you don't need to import it."),
                ]),
            ]);

            title = UNUSED_IMPORT.to_string();
        }
        Problem::ImportNameConflict {
            name,
            is_alias,
            new_module_id,
            new_import_region,
            existing_import,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.module(new_module_id),
                    if is_alias {
                        alloc.concat([
                            alloc.reflow(" was imported as "),
                            alloc.module_name(name.clone()),
                            alloc.reflow(":"),
                        ])
                    } else {
                        alloc.reflow(" was imported here: ")
                    },
                ]),
                alloc.region(lines.convert_region(new_import_region), severity),

                match existing_import {
                    ScopeModuleSource::Import(existing_import_region) => {
                        alloc.stack([
                            alloc.concat([
                                alloc.reflow("but "),
                                alloc.module_name(name.clone()),
                                alloc.reflow(" is already used by a previous import:"),
                            ]),
                            alloc.region(lines.convert_region(existing_import_region), severity),
                        ])
                    }
                    ScopeModuleSource::Builtin => {
                        alloc.concat([
                            alloc.reflow("but "),
                            alloc.module_name(name),
                            alloc.reflow(" is also the name of a builtin."),
                        ])
                    }
                    ScopeModuleSource::Current => {
                        alloc.concat([
                            alloc.reflow("but "),
                            alloc.module_name(name),
                            alloc.reflow(" is also the name of the current module."),
                        ])
                    }
                },
                alloc.reflow("Using the same name for both can make it hard to tell which module you are referring to."),
                if is_alias {
                    alloc.reflow("Make sure each import has a unique alias or none at all.")
                } else {
                    alloc.stack([
                        alloc.reflow("You can assign a different name to a module like this:"),
                        alloc.reflow("import JsonDecode as JD").indent(4),
                    ])
                },
            ]);
            title = IMPORT_NAME_CONFLICT.to_string();
        }

        Problem::ExplicitBuiltinImport(module_id, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The builtin "),
                    alloc.module(module_id),
                    alloc.reflow(" was imported here:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Builtins are imported automatically, so you can remove this import."),
                alloc.reflow("Tip: Learn more about builtins in the tutorial:\n<https://www.roc-lang.org/tutorial#builtin-modules>"),
            ]);

            title = EXPLICIT_BUILTIN_IMPORT.to_string();
        }

        Problem::ExplicitBuiltinTypeImport(symbol, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.symbol_qualified(symbol),
                    alloc.reflow(" was imported here:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("All types from builtins are automatically exposed, so you can remove "),
                    alloc.symbol_unqualified(symbol),
                    alloc.reflow(" from the exposing list.")
                ]),
                alloc.reflow("Tip: Learn more about builtins in the tutorial:\n<https://www.roc-lang.org/tutorial#builtin-modules>"),
            ]);

            title = EXPLICIT_BUILTIN_IMPORT.to_string();
        }

        Problem::ImportShadowsSymbol {
            region,
            new_symbol,
            existing_symbol_region,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This import exposes "),
                    alloc.symbol_qualified(new_symbol),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("However, the name "),
                    alloc.symbol_unqualified(new_symbol),
                    alloc.reflow(" was already used here:"),
                ]),
                alloc.region(lines.convert_region(existing_symbol_region), severity),
                alloc.concat([
                    alloc.reflow("You can rename it, or use the qualified name: "),
                    alloc.symbol_qualified(new_symbol),
                ]),
            ]);

            title = DUPLICATE_NAME.to_string();
        }

        Problem::DefsOnlyUsedInRecursion(1, region) => {
            doc = alloc.stack([
                alloc.reflow("This definition is only used in recursion with itself:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow(
                    "If you don't intend to use or export this definition, it should be removed!",
                ),
            ]);

            title = "DEFINITION ONLY USED IN RECURSION".to_string();
        }
        Problem::DefsOnlyUsedInRecursion(n, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("These "),
                    alloc.string(n.to_string()),
                    alloc.reflow(" definitions are only used in mutual recursion with themselves:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow(
                    "If you don't intend to use or export any of them, they should all be removed!",
                ),
            ]);

            title = "DEFINITIONS ONLY USED IN RECURSION".to_string();
        }
        Problem::ExposedButNotDefined(symbol) => {
            doc = alloc.stack([
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
        }
        Problem::UnusedArgument(closure_symbol, is_anonymous, argument_symbol, region) => {
            let line = "\". Adding an underscore at the start of a variable name is a way of saying that the variable is not used.";

            doc = alloc.stack([
                alloc.concat([
                    if is_anonymous {
                        alloc.reflow("This function")
                    } else {
                        alloc.symbol_unqualified(closure_symbol)
                    },
                    alloc.reflow(" doesn't use "),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.text("."),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("If you don't need "),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.reflow(", then you can just remove it. However, if you really do need "),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.reflow(" as an argument of "),
                    if is_anonymous {
                        alloc.reflow("this function")
                    } else {
                        alloc.symbol_unqualified(closure_symbol)
                    },
                    alloc.reflow(", prefix it with an underscore, like this: \"_"),
                    alloc.symbol_unqualified(argument_symbol),
                    alloc.reflow(line),
                ]),
            ]);

            title = UNUSED_ARG.to_string();
        }
        Problem::UnusedBranchDef(symbol, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.symbol_unqualified(symbol),
                    alloc.reflow(" is not used in this "),
                    alloc.keyword("when"),
                    alloc.reflow(" branch."),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("If you don't need to use "),
                    alloc.symbol_unqualified(symbol),
                    alloc.reflow(", prefix it with an underscore, like \"_"),
                    alloc.reflow(symbol.as_str(alloc.interns)),
                    alloc.reflow("\", or replace it with just an \"_\"."),
                ]),
            ]);

            title = UNUSED_DEF.to_string();
        }
        Problem::PrecedenceProblem(BothNonAssociative(region, left_bin_op, right_bin_op)) => {
            doc = alloc.stack([
                if left_bin_op.value == right_bin_op.value {
                    alloc.concat([
                        alloc.reflow("Using more than one "),
                        alloc.binop(left_bin_op.value),
                        alloc.reflow(concat!(
                            " like this requires parentheses,",
                            " to clarify how things should be grouped.",
                        )),
                    ])
                } else {
                    alloc.concat([
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
                alloc.region(lines.convert_region(region), severity),
            ]);

            title = SYNTAX_PROBLEM.to_string();
        }
        Problem::UnsupportedPattern(BadPattern::Unsupported(pattern_type), region) => {
            use roc_parse::pattern::PatternType::*;

            let this_thing = match pattern_type {
                TopLevelDef => "a top-level definition:",
                DefExpr => "a value definition:",
                FunctionArg => "function arguments:",
                ModuleParams => "module params:",
                WhenBranch => unreachable!("all patterns are allowed in a When"),
            };

            let suggestion = [
                alloc.reflow(
                    "Patterns like this don't cover all possible shapes of the input type. Use a ",
                ),
                alloc.keyword("when"),
                alloc.reflow(" ... "),
                alloc.keyword("is"),
                alloc.reflow(" instead."),
            ];

            doc = alloc.stack([
                alloc
                    .reflow("This pattern is not allowed in ")
                    .append(alloc.reflow(this_thing)),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat(suggestion),
            ]);

            title = SYNTAX_PROBLEM.to_string();
        }
        Problem::Shadowing {
            original_region,
            shadow,
            kind,
        } => {
            let (res_title, res_doc) =
                report_shadowing(alloc, lines, original_region, shadow, kind, severity);

            doc = res_doc;
            title = res_title.to_string();
        }
        Problem::CyclicAlias(symbol, region, others, alias_kind) => {
            let answer = crate::error::r#type::cyclic_alias(
                alloc, lines, symbol, region, others, alias_kind, severity,
            );

            doc = answer.0;
            title = answer.1;
        }
        Problem::PhantomTypeArgument {
            typ: alias,
            variable_region,
            variable_name,
            alias_kind,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The "),
                    alloc.type_variable(variable_name),
                    alloc.reflow(" type parameter is not used in the "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" "),
                    alloc.reflow(alias_kind.as_str()),
                    alloc.reflow(" definition:"),
                ]),
                alloc.region(lines.convert_region(variable_region), severity),
                alloc.reflow("Roc does not allow unused type parameters!"),
                // TODO add link to this guide section
                alloc.tip().append(alloc.reflow(
                    "If you want an unused type parameter (a so-called \"phantom type\"), \
                read the guide section on phantom values.",
                )),
            ]);

            title = UNUSED_ALIAS_PARAM.to_string();
        }
        Problem::WildcardNotAllowed {
            typ: alias,
            num_wildcards,
            one_occurrence,
            kind,
        } => {
            let mut stack = Vec::with_capacity(4);
            if num_wildcards == 1 {
                stack.push(alloc.concat([
                    alloc.reflow("The definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" includes a wildcard ("),
                    alloc.keyword("*"),
                    alloc.reflow(") type variable:"),
                ]));
            } else {
                stack.push(alloc.concat([
                    alloc.reflow("The definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" includes "),
                    text!(alloc, "{}", num_wildcards),
                    alloc.reflow(" wildcard ("),
                    alloc.keyword("*"),
                    alloc.reflow(") type variables. Here is one of them:"),
                ]));
            }
            stack.push(alloc.region(lines.convert_region(one_occurrence), severity));
            stack.push(alloc.concat([
                alloc.reflow(match kind {
                    AliasKind::Structural => "Type alias",
                    AliasKind::Opaque => "Opaque type",
                }),
                alloc.reflow(" definitions may not use wildcard ("),
                alloc.keyword("*"),
                alloc.reflow(") type variables. Only named type variables are allowed."),
            ]));
            doc = alloc.stack(stack);

            title = WILDCARD_NOT_ALLOWED.to_string();
        }
        Problem::UnderscoreNotAllowed {
            typ: alias,
            num_underscores,
            one_occurrence,
            kind,
        } => {
            let mut stack = Vec::with_capacity(4);
            if num_underscores == 1 {
                stack.push(alloc.concat([
                    alloc.reflow("The definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" includes an inferred ("),
                    alloc.keyword("_"),
                    alloc.reflow(") type:"),
                ]));
            } else {
                stack.push(alloc.concat([
                    alloc.reflow("The definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" includes "),
                    text!(alloc, "{}", num_underscores),
                    alloc.reflow(" inferred ("),
                    alloc.keyword("_"),
                    alloc.reflow(") types:"),
                ]));
                stack.push(alloc.reflow("Here is one of them:"));
            }
            stack.push(alloc.region(lines.convert_region(one_occurrence), severity));
            stack.push(alloc.concat([
                alloc.reflow(match kind {
                    AliasKind::Structural => "Type alias",
                    AliasKind::Opaque => "Opaque type",
                }),
                alloc.reflow(" definitions may not use inferred types ("),
                alloc.keyword("_"),
                alloc.reflow(")."),
            ]));
            doc = alloc.stack(stack);

            title = UNDERSCORE_NOT_ALLOWED.to_string();
        }
        Problem::UndeclaredTypeVar {
            typ: alias,
            num_unbound,
            one_occurrence,
            kind,
        } => {
            let decl_symbol = match kind {
                AliasKind::Structural => ":",
                AliasKind::Opaque => ":=",
            };
            let mut stack = Vec::with_capacity(4);

            if num_unbound == 1 {
                stack.push(alloc.concat([
                    alloc.reflow("The definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" includes an undeclared type variable:"),
                ]));
            } else {
                stack.push(alloc.concat([
                    alloc.reflow("The definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" includes "),
                    text!(alloc, "{}", num_unbound),
                    alloc.reflow(" undeclared type variables."),
                ]));
                stack.push(alloc.reflow("Here is one of them:"));
            }
            stack.push(alloc.region(lines.convert_region(one_occurrence), severity));
            stack.push(alloc.concat([
                alloc.reflow("All type variables in "),
                alloc.reflow(match kind {
                    AliasKind::Structural => "type alias",
                    AliasKind::Opaque => "opaque type",
                }),
                alloc.reflow(" definitions must be declared."),
            ]));
            stack.push(alloc.tip().append(alloc.concat([
                alloc.reflow("You can declare type variables by putting them right before the "),
                alloc.keyword(decl_symbol),
                alloc.reflow(" symbol, separated by spaces."),
            ])));
            doc = alloc.stack(stack);

            title = UNDECLARED_TYPE_VARIABLE.to_string();
        }
        Problem::BadRecursion(entries) => {
            doc = to_circular_def_doc(alloc, lines, &entries, severity);
            title = CIRCULAR_DEF.to_string();
        }
        Problem::DuplicateRecordFieldValue {
            field_name,
            field_region,
            record_region,
            replaced_region,
        } => {
            doc = alloc.stack([
                alloc.concat([
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
                alloc.concat([
                    alloc.reflow("For clarity, remove the previous "),
                    alloc.record_field(field_name),
                    alloc.reflow(" definitions from this record."),
                ]),
            ]);

            title = DUPLICATE_FIELD_NAME.to_string();
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
            doc = alloc.stack([
                alloc.concat([
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
                alloc.concat([
                    alloc.reflow("For clarity, remove the previous "),
                    alloc.record_field(field_name),
                    alloc.reflow(" definitions from this record type."),
                ]),
            ]);

            title = DUPLICATE_FIELD_NAME.to_string();
        }
        Problem::DuplicateTag {
            tag_name,
            tag_union_region,
            tag_region,
            replaced_region,
        } => {
            doc = alloc.stack([
                alloc.concat([
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
                alloc.concat([
                    alloc.reflow("For clarity, remove the previous "),
                    alloc.tag_name(tag_name),
                    alloc.reflow(" definitions from this tag union type."),
                ]),
            ]);

            title = DUPLICATE_TAG_NAME.to_string();
        }
        Problem::SignatureDefMismatch {
            ref annotation_pattern,
            ref def_pattern,
        } => {
            doc = alloc.stack([
                alloc.reflow(
                    "This annotation does not match the definition immediately following it:",
                ),
                alloc.region(
                    lines.convert_region(Region::span_across(annotation_pattern, def_pattern)),
                    severity,
                ),
                alloc.reflow("Is it a typo? If not, put either a newline or comment between them."),
            ]);

            title = NAMING_PROBLEM.to_string();
        }
        Problem::InvalidAliasRigid {
            alias_name: type_name,
            region,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This definition of "),
                    alloc.symbol_unqualified(type_name),
                    alloc.reflow(" has an unexpected pattern:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Only type variables like "),
                    alloc.type_variable("a".into()),
                    alloc.reflow(" or "),
                    alloc.type_variable("value".into()),
                    alloc.reflow(" can occur in this position."),
                ]),
            ]);

            title = SYNTAX_PROBLEM.to_string();
        }
        Problem::InvalidHexadecimal(region) => {
            doc = alloc.stack([
                alloc.reflow("This unicode code point is invalid:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow(r"I was expecting a hexadecimal number, like "),
                    alloc.parser_suggestion("\\u(1100)"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("\\u(00FF)"),
                    alloc.text("."),
                ]),
                alloc.reflow(r"Learn more about working with unicode in roc at TODO"),
            ]);

            title = INVALID_UNICODE.to_string();
        }
        Problem::InvalidUnicodeCodePt(region) => {
            doc = alloc.stack([
                alloc.reflow("This unicode code point is invalid:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Learn more about working with unicode in roc at TODO"),
            ]);

            title = INVALID_UNICODE.to_string();
        }
        Problem::InvalidInterpolation(region) => {
            doc = alloc.stack([
                alloc.reflow("This string interpolation is invalid:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow(r"String interpolations cannot contain newlines or other interpolations."),
                alloc.reflow(r"You can learn more about string interpolation at <https://www.roc-lang.org/tutorial#string-interpolation>"),
            ]);

            title = SYNTAX_PROBLEM.to_string();
        }
        Problem::RuntimeError(runtime_error) => {
            let answer = pretty_runtime_error(alloc, lines, runtime_error);

            doc = answer.0;
            title = answer.1.to_string();
        }
        Problem::NestedDatatype {
            alias,
            def_region,
            differing_recursion_region,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" is a nested datatype. Here is one recursive usage of it:"),
                ]),
                alloc.region(lines.convert_region(differing_recursion_region), severity),
                alloc.concat([
                    alloc.reflow("But recursive usages of "),
                    alloc.symbol_unqualified(alias),
                    alloc.reflow(" must match its definition:"),
                ]),
                alloc.region(lines.convert_region(def_region), severity),
                alloc.reflow("Nested datatypes are not supported in Roc."),
                alloc.concat([
                    alloc.hint("Consider rewriting the definition of "),
                    alloc.symbol_unqualified(alias),
                    alloc.text(" to use the recursive type with the same arguments."),
                ]),
            ]);

            title = NESTED_DATATYPE.to_string();
        }

        Problem::InvalidExtensionType { region, kind } => {
            let (kind_str, can_only_contain) = match kind {
                ExtensionTypeKind::Record => ("record", "a type variable or another record"),
                ExtensionTypeKind::TagUnion => {
                    ("tag union", "a type variable or another tag union")
                }
            };

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "),
                    alloc.text(kind_str),
                    alloc.reflow(" extension type is invalid:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.note("A "),
                    alloc.reflow(kind_str),
                    alloc.reflow(" extension variable can only contain "),
                    alloc.reflow(can_only_contain),
                    alloc.reflow("."),
                ]),
            ]);

            title = INVALID_EXTENSION_TYPE.to_string();
        }

        Problem::AbilityHasTypeVariables {
            name,
            variables_region,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The definition of the "),
                    alloc.symbol_unqualified(name),
                    alloc.reflow(" ability includes type variables:"),
                ]),
                alloc.region(lines.convert_region(variables_region), severity),
                alloc.reflow(
                    "Abilities cannot depend on type variables, but their member values can!",
                ),
            ]);
            title = ABILITY_HAS_TYPE_VARIABLES.to_string();
        }

        Problem::ImplementsClauseIsNotAbility {
            region: clause_region,
        } => {
            doc = alloc.stack([
                alloc.reflow(
                    r#"The type referenced in this "implements" clause is not an ability:"#,
                ),
                alloc.region(lines.convert_region(clause_region), severity),
            ]);
            title = IMPLEMENTS_CLAUSE_IS_NOT_AN_ABILITY.to_string();
        }

        Problem::IllegalImplementsClause { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("An "),
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.reflow(" clause is not allowed here:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.reflow(
                        " clauses can only be specified on the top-level type annotations.",
                    ),
                ]),
            ]);
            title = ILLEGAL_IMPLEMENTS_CLAUSE.to_string();
        }

        Problem::DuplicateImplementsAbility { ability, region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("I already saw that this type variable is bound to the "),
                    alloc.symbol_foreign_qualified(ability),
                    alloc.reflow(" ability once before:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Abilities only need to bound to a type variable once in an "),
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.reflow(" clause!"),
                ]),
            ]);
            title = "DUPLICATE BOUND ABILITY".to_string();
        }

        Problem::AbilityMemberMissingImplementsClause {
            member,
            ability,
            region,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The definition of the ability member "),
                    alloc.symbol_unqualified(member),
                    alloc.reflow(" does not include an "),
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.reflow(" clause binding a type variable to the ability "),
                    alloc.symbol_unqualified(ability),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Ability members must include an "),
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.reflow(" clause binding a type variable to an ability, like"),
                ]),
                alloc.type_block(alloc.concat([
                    alloc.type_variable("a".into()),
                    alloc.space(),
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.space(),
                    alloc.symbol_unqualified(ability),
                ])),
                alloc.concat([alloc
                    .reflow("Otherwise, the function does not need to be part of the ability!")]),
            ]);
            title = ABILITY_MEMBER_MISSING_IMPLEMENTS_CLAUSE.to_string();
        }

        Problem::AbilityMemberMultipleBoundVars {
            member,
            ability,
            span_implements_clauses: span_has_clauses,
            mut bound_var_names,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The definition of the ability member "),
                    alloc.symbol_unqualified(member),
                    alloc.reflow(" includes multiple variables bound to the "),
                    alloc.symbol_unqualified(ability),
                    alloc.keyword(" ability:"),
                ]),
                alloc.region(lines.convert_region(span_has_clauses), severity),
                alloc.reflow("Ability members can only bind one type variable to their parent ability. Otherwise, I wouldn't know what type implements an ability by looking at specializations!"),
                alloc.concat([
                    alloc.hint("Did you mean to only bind "),
                    alloc.type_variable(bound_var_names.swap_remove(0)),
                    alloc.reflow(" to "),
                    alloc.symbol_unqualified(ability),
                    alloc.reflow("?"),
                ])
            ]);
            title = ABILITY_MEMBER_BINDS_MULTIPLE_VARIABLES.to_string();
        }

        Problem::AbilityNotOnToplevel { region } => {
            doc = alloc.stack([
                alloc
                    .concat([alloc
                        .reflow("This ability definition is not on the top-level of a module:")]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Abilities can only be defined on the top-level of a Roc module."),
            ]);
            title = ABILITY_NOT_ON_TOPLEVEL.to_string();
        }

        Problem::AbilityUsedAsType(suggested_var_name, ability, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("You are attempting to use the ability "),
                    alloc.symbol_unqualified(ability),
                    alloc.reflow(" as a type directly:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow(
                    "Abilities can only be used in type annotations to constrain type variables.",
                ),
                alloc
                    .hint("")
                    .append(alloc.reflow("Perhaps you meant to include an "))
                    .append(alloc.keyword(roc_parse::keyword::IMPLEMENTS))
                    .append(alloc.reflow(" annotation, like")),
                alloc.type_block(alloc.concat([
                    alloc.type_variable(suggested_var_name),
                    alloc.space(),
                    alloc.keyword(roc_parse::keyword::IMPLEMENTS),
                    alloc.space(),
                    alloc.symbol_unqualified(ability),
                ])),
            ]);
            title = ABILITY_USED_AS_TYPE.to_string();
        }
        Problem::NestedSpecialization(member, region) => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This specialization of the "),
                    alloc.symbol_unqualified(member),
                    alloc.reflow(" ability member is in a nested scope:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Specializations can only be defined on the top-level of a module."),
            ]);
            title = SPECIALIZATION_NOT_ON_TOPLEVEL.to_string();
        }
        Problem::IllegalDerivedAbility(region) => {
            doc = alloc.stack([
                alloc.reflow("This ability cannot be derived:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Only builtin abilities can be derived."),
                alloc
                    .note("The builtin abilities are ")
                    .append(list_builtin_abilities(alloc)),
            ]);
            title = ILLEGAL_DERIVE.to_string();
        }
        Problem::NotAnAbility(region) => {
            doc = alloc.stack([
                alloc.reflow("This identifier is not an ability in scope:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Only abilities can be implemented."),
            ]);
            title = NOT_AN_ABILITY.to_string();
        }
        Problem::NotAnAbilityMember {
            ability,
            name,
            region,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The "), alloc.symbol_unqualified(ability), alloc.reflow(" ability does not have a member "), alloc.string(name),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Only implementations for members an ability has can be specified in this location.")
            ]);
            title = NOT_AN_ABILITY_MEMBER.to_string();
        }
        Problem::ImplementationNotFound { member, region } => {
            let member_str = member.as_str(alloc.interns);
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("An implementation of "), alloc.symbol_unqualified(member), alloc.reflow(" could not be found in this scope:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.tip().append(alloc.concat([alloc.reflow("consider adding a value of name "), alloc.symbol_unqualified(member), alloc.reflow(" in this scope, or using another variable that implements this ability member, like "), alloc.type_str(&format!("{{ {member_str}: my{member_str} }}"))]))
            ]);
            title = IMPLEMENTATION_NOT_FOUND.to_string();
        }
        Problem::OptionalAbilityImpl { ability, region } => {
            let hint = if ability.is_builtin() {
                alloc.hint("").append(
                    alloc.reflow("if you want this implementation to be derived, don't include a record of implementations. For example,")
                        .append(alloc.type_block(alloc.concat([alloc.type_str(roc_parse::keyword::IMPLEMENTS), alloc.type_str(" ["), alloc.symbol_unqualified(ability), alloc.type_str("]")])))
                        .append(alloc.reflow(" will attempt to derive ").append(alloc.symbol_unqualified(ability))))
            } else {
                alloc.nil()
            };

            doc = alloc.stack([
                alloc.reflow("Ability implementations cannot be optional:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Custom implementations must be supplied fully."),
                hint,
            ]);
            title = OPTIONAL_ABILITY_IMPLEMENTATION.to_string();
        }
        Problem::QualifiedAbilityImpl { region } => {
            doc = alloc.stack([
                alloc.reflow("This ability implementation is qualified:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow(
                    "Custom implementations must be defined in the local scope, and unqualified.",
                ),
            ]);
            title = QUALIFIED_ABILITY_IMPLEMENTATION.to_string();
        }
        Problem::AbilityImplNotIdent { region } => {
            doc = alloc.stack([
                alloc.reflow("This ability implementation is not an identifier:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow(
                    "Custom ability implementations defined in this position can only be unqualified identifiers, not arbitrary expressions.",
                ),
                alloc.tip().append(alloc.reflow("consider defining this expression as a variable."))
            ]);
            title = ABILITY_IMPLEMENTATION_NOT_IDENTIFIER.to_string();
        }
        Problem::DuplicateImpl {
            original,
            duplicate,
        } => {
            doc = alloc.stack([
                alloc.reflow("This ability member implementation is duplicate:"),
                alloc.region(lines.convert_region(duplicate), severity),
                alloc.reflow("The first implementation was defined here:"),
                alloc.region(lines.convert_region(original), severity),
                alloc
                    .reflow("Only one custom implementation can be defined for an ability member."),
            ]);
            title = DUPLICATE_IMPLEMENTATION.to_string();
        }
        Problem::ImplementsNonRequired {
            region,
            ability,
            not_required,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This type implements members that are not part of the "),
                    alloc.symbol_unqualified(ability),
                    alloc.reflow(" ability:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("The following implemented members should not be listed:"),
                alloc.type_block(
                    alloc.intersperse(
                        not_required
                            .into_iter()
                            .map(|sym| alloc.symbol_unqualified(sym)),
                        alloc.string(",".to_string()).append(alloc.space()),
                    ),
                ),
            ]);
            title = UNNECESSARY_IMPLEMENTATIONS.to_string();
        }
        Problem::DoesNotImplementAbility {
            region,
            ability,
            not_implemented,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This type does not fully implement the "),
                    alloc.symbol_unqualified(ability),
                    alloc.reflow(" ability:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("The following necessary members are missing implementations:"),
                alloc.type_block(
                    alloc.intersperse(
                        not_implemented
                            .into_iter()
                            .map(|sym| alloc.symbol_unqualified(sym)),
                        alloc.string(",".to_string()).append(alloc.space()),
                    ),
                ),
            ]);
            title = INCOMPLETE_ABILITY_IMPLEMENTATION.to_string();
        }
        Problem::NotBoundInAllPatterns {
            unbound_symbol,
            region,
        } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.symbol_unqualified(unbound_symbol),
                    alloc.reflow(" is not bound in all patterns of this "),
                    alloc.keyword("when"),
                    alloc.reflow(" branch"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Identifiers introduced in a "),
                    alloc.keyword("when"),
                    alloc.reflow(" branch must be bound in all patterns of the branch. Otherwise, the program would crash when it tries to use an identifier that wasn't bound!"),
                ]),
            ]);
            title = "NAME NOT BOUND IN ALL PATTERNS".to_string();
        }
        Problem::NoIdentifiersIntroduced(region) => {
            doc = alloc.stack([
                alloc.reflow("This destructure assignment doesn't introduce any new variables:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("If you don't need to use the value on the right-hand side of this assignment, consider removing the assignment. Since effects are not allowed at the top-level, assignments that don't introduce variables cannot affect a program's behavior"),
            ]);
            title = "UNNECESSARY DEFINITION".to_string();
        }
        Problem::OverloadedSpecialization {
            ability_member,
            overload,
            original_opaque,
        } => {
            doc = alloc.stack([
                alloc.reflow("This ability member specialization is already claimed to specialize another opaque type:"),
                alloc.region(lines.convert_region(overload), severity),
                alloc.concat([
                    alloc.reflow("Previously, we found it to specialize "),
                    alloc.symbol_unqualified(ability_member),
                    alloc.reflow(" for "),
                    alloc.symbol_unqualified(original_opaque),
                    alloc.reflow("."),
                ]),
                alloc.reflow("Ability specializations can only provide implementations for one opaque type, since all opaque types are different!"),
            ]);
            title = "OVERLOADED SPECIALIZATION".to_string();
        }
        Problem::UnnecessaryOutputWildcard { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This type annotation has a wildcard type variable ("),
                    alloc.keyword("*"),
                    alloc.reflow(") that isn't needed."),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Annotations for tag unions which are constants, or which are returned from functions, work the same way with or without a "),
                    alloc.keyword("*"),
                    alloc.reflow(" at the end. (The "),
                    alloc.keyword("*"),
                    alloc.reflow(" means something different when the tag union is an argument to a function, though!)"),
                ]),
                alloc.reflow("You can safely remove this to make the code more concise without changing what it means."),
            ]);
            title = "UNNECESSARY WILDCARD".to_string();
        }
        Problem::MultipleListRestPattern { region } => {
            doc = alloc.stack([
                alloc.reflow("This list pattern match has multiple rest patterns:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("I only support compiling list patterns with one "),
                    alloc.parser_suggestion(".."),
                    alloc.reflow(" pattern! Can you remove this additional one?"),
                ]),
            ]);
            title = "MULTIPLE LIST REST PATTERNS".to_string();
        }
        Problem::BadTypeArguments {
            symbol,
            region,
            type_got,
            alias_needs,
            alias_kind,
        } => {
            let needed_arguments = if alias_needs == 1 {
                alloc.reflow("1 type argument")
            } else {
                alloc
                    .text(alias_needs.to_string())
                    .append(alloc.reflow(" type arguments"))
            };

            let found_arguments = alloc.text(type_got.to_string());

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The "),
                    alloc.symbol_unqualified(symbol),
                    alloc.reflow(" "),
                    alloc.reflow(alias_kind.as_str()),
                    alloc.reflow(" expects "),
                    needed_arguments,
                    alloc.reflow(", but it got "),
                    found_arguments,
                    alloc.reflow(" instead:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Are there missing parentheses?"),
            ]);

            title = if type_got > alias_needs {
                "TOO MANY TYPE ARGUMENTS".to_string()
            } else {
                "TOO FEW TYPE ARGUMENTS".to_string()
            };
        }
        Problem::UnappliedCrash { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "), alloc.keyword("crash"), alloc.reflow(" doesn't have a message given to it:")
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.keyword("crash"), alloc.reflow(" must be passed a message to crash with at the exact place it's used. "),
                    alloc.keyword("crash"), alloc.reflow(" can't be used as a value that's passed around, like functions can be - it must be applied immediately!"),
                ])
            ]);
            title = "UNAPPLIED CRASH".to_string();
        }
        Problem::OverAppliedCrash { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "),
                    alloc.keyword("crash"),
                    alloc.reflow(" has too many values given to it:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.keyword("crash"),
                    alloc.reflow(" must be given exactly one message to crash with."),
                ]),
            ]);
            title = "OVERAPPLIED CRASH".to_string();
        }
        Problem::UnappliedDbg { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "), alloc.keyword("dbg"), alloc.reflow(" doesn't have a value given to it:")
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.keyword("dbg"), alloc.reflow(" must be passed a value to print at the exact place it's used. "),
                    alloc.keyword("dbg"), alloc.reflow(" can't be used as a value that's passed around, like functions can be - it must be applied immediately!"),
                ])
            ]);
            title = "UNAPPLIED DBG".to_string();
        }
        Problem::OverAppliedDbg { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "),
                    alloc.keyword("dbg"),
                    alloc.reflow(" has too many values given to it:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.keyword("dbg"),
                    alloc.reflow(" must be given exactly one value to print."),
                ]),
            ]);
            title = "OVERAPPLIED DBG".to_string();
        }
        Problem::FileProblem { filename, error } => {
            let report = to_file_problem_report(alloc, filename, error);
            doc = report.doc;
            title = report.title;
        }

        Problem::ReturnOutsideOfFunction {
            region,
            return_kind,
        } => {
            let return_keyword;
            (title, return_keyword) = match return_kind {
                EarlyReturnKind::Return => ("RETURN OUTSIDE OF FUNCTION".to_string(), "return"),
                EarlyReturnKind::Try => ("TRY OUTSIDE OF FUNCTION".to_string(), "try"),
            };

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "),
                    alloc.keyword(return_keyword),
                    alloc.reflow(" doesn't belong to a function:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("I wouldn't know where to return to if I used it!"),
            ]);
        }

        Problem::StatementsAfterReturn { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This code won't run because it follows a "),
                    alloc.keyword("return"),
                    alloc.reflow(" statement:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.hint("you can move the "),
                    alloc.keyword("return"),
                    alloc.reflow(
                        " statement below this block to make the code that follows it run.",
                    ),
                ]),
            ]);

            title = "UNREACHABLE CODE".to_string();
        }

        Problem::ReturnAtEndOfFunction { region } => {
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "),
                    alloc.keyword("return"),
                    alloc.reflow(" keyword is redundant:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("The last expression in a function is treated like a "),
                    alloc.keyword("return"),
                    alloc.reflow(" statement. You can safely remove "),
                    alloc.keyword("return"),
                    alloc.reflow(" here."),
                ]),
            ]);

            title = "UNNECESSARY RETURN".to_string();
        }

        Problem::StmtAfterExpr(region) => {
            doc = alloc.stack([
                alloc
                    .reflow(r"I just finished parsing an expression with a series of definitions,"),
                alloc.reflow(
                    r"and this line is indented as if it's intended to be part of that expression:",
                ),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([alloc.reflow(
                    "However, I already saw the final expression in that series of definitions.",
                )]),
                alloc.tip().append(
                    alloc.reflow(
                        "An expression like `4`, `\"hello\"`, or `functionCall MyThing` is like `return 4` in other programming languages. To me, it seems like you did `return 4` followed by more code in the lines after, that code would never be executed!"
                    )
                ),
                alloc.tip().append(
                    alloc.reflow(
                        "If you are working with `Task`, this error can happen if you forgot a `!` somewhere."
                    )
                )
            ]);

            title = STATEMENT_AFTER_EXPRESSION.to_string();
        }

        Problem::UnsuffixedEffectfulRecordField(region) => {
            doc = alloc.stack([
                alloc.reflow(
                    "The type of this record field is an effectful function, but its name does not indicate so:",
                ),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Add an exclamation mark at the end, like:"),
                alloc
                    .parser_suggestion("{ readFile!: Str => Str }")
                    .indent(4),
                alloc.reflow("This will help readers identify it as a source of effects."),
            ]);

            title = MISSING_EXCLAMATION.to_string();
        }

        Problem::SuffixedPureRecordField(region) => {
            doc = alloc.stack([
                alloc.reflow(
                    "The type of this record field is a pure function, but its name suggests otherwise:",
                ),
                alloc.region(lines.convert_region(region), severity),
                alloc
                    .reflow("The exclamation mark at the end is reserved for effectful functions."),
                alloc.concat([
                    alloc.hint("Did you mean to use "),
                    alloc.keyword("=>"),
                    alloc.text(" instead of "),
                    alloc.keyword("->"),
                    alloc.text("?"),
                ]),
            ]);

            title = UNNECESSARY_EXCLAMATION.to_string();
        }

        Problem::EmptyTupleType(region) => {
            doc = alloc.stack([
                alloc.reflow("This tuple type is empty:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("Empty tuples are not allowed in Roc."),
            ]);

            title = EMPTY_TUPLE_TYPE.to_string();
        }
        Problem::UnboundTypeVarsInAs(region) => {
            // NOTE for the enterprising contributor:
            // this may be something we want to support in the future! (not sure?)
            doc = alloc.stack([
                alloc.reflow("This type annotation has unbound type variables:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow(
                    "Type variables must be bound in the same scope as the type annotation.",
                ),
            ]);

            title = UNBOUND_TYPE_VARS_IN_AS.to_string();
        }
    };

    Report {
        title,
        filename,
        doc,
        severity,
    }
}

fn list_builtin_abilities<'a>(alloc: &'a RocDocAllocator<'a>) -> RocDocBuilder<'a> {
    alloc.intersperse(
        DERIVABLE_ABILITIES
            .iter()
            .map(|(ab, _)| alloc.symbol_unqualified(*ab)),
        alloc.reflow(", "),
    )
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
    alloc.stack([
        alloc.concat([
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
    severity: Severity,
) -> RocDocBuilder<'b> {
    use roc_parse::ident::BadIdent::*;

    match bad_ident {
        Start(_) | Space(_, _) => unreachable!("these are handled in the parser"),
        WeirdDotAccess(pos) | StrayDot(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow(r"I am trying to parse a record field access here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("So I expect to see a lowercase letter next, like "),
                    alloc.parser_suggestion(".name"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion(".height"),
                    alloc.reflow("."),
                ]),
            ])
        }

        WeirdAccessor(_pos) => alloc.stack([
            alloc.reflow("I am very confused by this field access"),
            alloc.region(lines.convert_region(surroundings), severity),
            alloc.concat([
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

        StrayAmpersand(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow(r"I am trying to parse a record updater function here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("So I expect to see a lowercase letter next, like "),
                    alloc.parser_suggestion("&name"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("&height"),
                    alloc.reflow("."),
                ]),
            ])
        }

        WeirdDotQualified(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("I was expecting to see an identifier next, like "),
                    alloc.parser_suggestion("height"),
                    alloc.reflow(". A complete qualified name looks something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("."),
                ]),
            ])
        }
        QualifiedTupleAccessor(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("This looks like a tuple accessor on a module or tag name,"),
                    alloc.reflow(r"but neither modules nor tags can have tuple elements! "),
                    alloc.reflow(r"Maybe you wanted a qualified name, something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("."),
                ]),
            ])
        }
        QualifiedTag(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"This looks like a qualified tag name to me, "),
                    alloc.reflow(r"but tags cannot be qualified! "),
                    alloc.reflow(r"Maybe you wanted a qualified name, something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("?"),
                ]),
            ])
        }

        UnderscoreAlone(_pos) => {
            alloc.stack([
                alloc.reflow("An underscore is being used as a variable here:"),
                alloc.region(lines.convert_region(surroundings), severity),
                alloc.concat([alloc
                    .reflow(r"An underscore can be used to ignore a value when pattern matching, but it cannot be used as a variable.")]),
            ])
        }

        UnderscoreInMiddle(_pos) => {
            alloc.stack([
                alloc.reflow("Underscores are not allowed in tag or opaque ref names:"),
                alloc.region(lines.convert_region(surroundings), severity),
                alloc.concat([alloc
                    .reflow(r"I recommend using PascalCase. It's the standard style in Roc code!")]),
            ])
        }

        UnderscoreAtStart {
            position: _pos,
            declaration_region,
        } => {
            let line = "This variable's name starts with an underscore:";
            alloc.stack([
                match declaration_region {
                    None => alloc.reflow(line),
                    Some(declaration_region) => alloc.stack([
                        alloc.reflow(line),
                        alloc.region(lines.convert_region(declaration_region), severity),
                        alloc.reflow("But then it is used here:"),
                    ])
                },
                alloc.region(lines.convert_region(surroundings), severity),
                alloc.concat([
                    alloc.reflow(r"A variable's name can only start with an underscore if the variable is unused. "),
                    match declaration_region {
                        None => alloc.reflow(r"But it looks like the variable is being used here!"),
                        Some(_) => alloc.reflow(r"Since you are using this variable, you could remove the underscore from its name in both places."),
                    }
                ]),
            ])
        }

        TooManyUnderscores(_pos) => {
            alloc.stack([
                alloc.reflow("This variable's name is using snake case, but has more than one consecutive underscore ('_') characters."),
                alloc.region(lines.convert_region(surroundings), severity),
                alloc.concat([
                    alloc.reflow(r"When using snake case, Roc style recommends only using a single underscore consecutively. This will be fixed by the formatter.")
                ]),
            ])
        }

        BadOpaqueRef(pos) => {
            use BadIdentNext::*;
            let kind = "an opaque reference";

            match what_is_next(alloc.src_lines, lines.convert_pos(pos)) {
                LowercaseAccess(width) => {
                    let region = Region::new(pos, pos.bump_column(width));
                    alloc.stack([
                        alloc.reflow("I am very confused by this field access:"),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            lines.convert_region(region),
                            severity
                        ),
                        alloc.concat([
                            alloc.reflow(r"It looks like a record field access on "),
                            alloc.reflow(kind),
                            alloc.text("."),
                        ]),
                    ])
                }
                UppercaseAccess(width) => {
                    let region = Region::new(pos, pos.bump_column(width));
                    alloc.stack([
                        alloc.reflow("I am very confused by this expression:"),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            lines.convert_region(region),
                            severity
                        ),
                        alloc.concat([
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
                    alloc.stack([
                        alloc.concat([
                            alloc.reflow("I am trying to parse "),
                            alloc.reflow(kind),
                            alloc.reflow(" here:"),
                        ]),
                        alloc.region_with_subregion(
                            lines.convert_region(surroundings),
                            lines.convert_region(region),
                            severity,
                        ),
                        alloc.concat([
                            alloc.reflow(r"But after the "),
                            alloc.keyword("@"),
                            alloc.reflow(r" symbol I found a lowercase letter. "),
                            alloc.reflow(r"All opaque references "),
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
    severity: Severity,
) -> RocDocBuilder<'b> {
    use roc_parse::ident::BadIdent::*;

    match bad_ident {
        Start(_) | Space(_, _) => unreachable!("these are handled in the parser"),
        WeirdDotAccess(pos) | StrayDot(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow(r"I am trying to parse a record field accessor here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("Something like "),
                    alloc.parser_suggestion(".name"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion(".height"),
                    alloc.reflow(" that accesses a value from a record."),
                ]),
            ])
        }

        StrayAmpersand(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow(r"I am trying to parse a record updater function here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow("Something like "),
                    alloc.parser_suggestion("&name"),
                    alloc.reflow(" or "),
                    alloc.parser_suggestion("&height"),
                    alloc.reflow(" that updates a field in a record."),
                ]),
            ])
        }

        WeirdAccessor(_pos) => alloc.stack([
            alloc.reflow("I am very confused by this field access"),
            alloc.region(lines.convert_region(surroundings), severity),
            alloc.concat([
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

        QualifiedTupleAccessor(pos) | WeirdDotQualified(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
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

            alloc.stack([
                alloc.reflow("I am trying to parse a qualified name here:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"This looks like a qualified tag name to me, "),
                    alloc.reflow(r"but tags cannot be qualified! "),
                    alloc.reflow(r"Maybe you wanted a qualified name, something like "),
                    alloc.parser_suggestion("Json.Decode.string"),
                    alloc.text("?"),
                ]),
            ])
        }

        UnderscoreAlone(..) | UnderscoreAtStart { .. } => {
            unreachable!(
                "it's fine to have an underscore at the beginning of an identifier in a pattern"
            )
        }

        TooManyUnderscores(_pos) => {
            alloc.stack([
                alloc.reflow("I am trying to parse an identifier here:"),
                alloc.region(lines.convert_region(surroundings), severity),
                alloc.concat([
                    alloc.reflow(r"Snake case is allowed here, but only a single consecutive underscore should be used.")
                ]),
            ])
        }

        UnderscoreInMiddle(pos) => {
            let region = Region::from_pos(pos.sub(1));

            alloc.stack([
                alloc.reflow("I am trying to parse a tag or opaque ref here:"),
                alloc.region_with_subregion(
                    lines.convert_region(surroundings),
                    lines.convert_region(region),
                    severity,
                ),
                alloc.concat([alloc.reflow(
                    r"Underscores are not allowed in tags or opaque refs. Use PascalCase instead!",
                )]),
            ])
        }

        BadOpaqueRef(pos) => {
            let region = LineColumnRegion::from_pos(lines.convert_pos(pos));

            alloc.stack([
                alloc.reflow("This opaque type reference has an invalid name:"),
                alloc.region_with_subregion(lines.convert_region(surroundings), region, severity),
                alloc.concat([
                    alloc.reflow(r"Opaque type names must begin with a capital letter, "),
                    alloc.reflow(r"and must contain only letters and numbers."),
                ]),
            ])
        }
    }
}

#[derive(Debug)]
enum BadIdentNext<'a> {
    LowercaseAccess(u32),
    UppercaseAccess(u32),
    #[allow(dead_code)]
    // The field u32 will be used once todo is implemented in to_bad_ident_expr_report
    NumberAccess(u32),
    #[allow(dead_code)]
    // The field str will be used once todo is implemented in to_bad_ident_expr_report
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
    kind: ShadowKind,
    severity: Severity,
) -> (&'static str, RocDocBuilder<'b>) {
    let (what, what_plural, is_builtin) = match kind {
        ShadowKind::Variable => ("variable", "variables", false),
        ShadowKind::Alias(sym) => ("alias", "aliases", sym.is_builtin()),
        ShadowKind::Opaque(sym) => ("opaque type", "opaque types", sym.is_builtin()),
        ShadowKind::Ability(sym) => ("ability", "abilities", sym.is_builtin()),
    };

    let doc = if is_builtin {
        alloc.stack([
            alloc.concat([
                alloc.reflow("This "),
                alloc.reflow(what),
                alloc.reflow(" has the same name as a builtin:"),
            ]),
            alloc.region(lines.convert_region(shadow.region), severity),
            alloc.concat([
                alloc.reflow("All builtin "),
                alloc.reflow(what_plural),
                alloc.reflow(" are in scope by default, so I need this "),
                alloc.reflow(what),
                alloc.reflow(" to have a different name!"),
            ]),
        ])
    } else {
        alloc.stack([
            alloc
                .text("The ")
                .append(alloc.ident(shadow.value))
                .append(alloc.reflow(" name is first defined here:")),
            alloc.region(lines.convert_region(original_region), severity),
            alloc.reflow("But then it's defined a second time here:"),
            alloc.region(lines.convert_region(shadow.region), severity),
            alloc.concat([
                alloc.reflow("Since these "),
                alloc.reflow(what_plural),
                alloc.reflow(" have the same name, it's easy to use the wrong one by accident. Give one of them a new name."),
            ]),
        ])
    };

    (DUPLICATE_NAME, doc)
}

fn pretty_runtime_error<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    runtime_error: RuntimeError,
) -> (RocDocBuilder<'b>, &'static str) {
    let doc;
    let title;

    let severity = Severity::RuntimeError;

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
            kind,
        } => {
            (title, doc) = report_shadowing(alloc, lines, original_region, shadow, kind, severity);
        }

        RuntimeError::LookupNotInScope {
            loc_name,
            suggestion_options: options,
            underscored_suggestion_region,
        } => {
            doc = not_found(
                alloc,
                lines,
                loc_name.region,
                &loc_name.value,
                options,
                underscored_suggestion_region,
                severity,
            );
            title = UNRECOGNIZED_NAME;
        }
        RuntimeError::CircularDef(entries) => {
            doc = to_circular_def_doc(alloc, lines, &entries, severity);
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
                    doc = to_bad_ident_pattern_report(alloc, lines, bad_ident, region, severity);

                    return (doc, title);
                }
                Unknown | CantApplyPattern => " ",
                QualifiedIdentifier => " qualified ",
                EmptySingleQuote => " empty character literal ",
                MultipleCharsInSingleQuote => " overfull literal ",
                DuplicateListRestPattern => " second rest pattern ",
            };

            let tip = match problem {
                MalformedInt | MalformedFloat | MalformedBase(_) => alloc
                    .tip()
                    .append(alloc.reflow("Learn more about number literals at TODO")),
                EmptySingleQuote
                | MultipleCharsInSingleQuote
                | Unknown
                | BadIdent(_)
                | CantApplyPattern => alloc.nil(),
                QualifiedIdentifier => alloc
                    .tip()
                    .append(alloc.reflow("In patterns, only tags can be qualified")),
                DuplicateListRestPattern => alloc
                    .tip()
                    .append(alloc.reflow("List patterns can only have one rest pattern")),
            };

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This"),
                    alloc.text(name),
                    alloc.reflow("pattern is malformed:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
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
                alloc.concat([
                    alloc.reflow("In fact, it looks like "),
                    alloc.module_name(module_name.clone()),
                    alloc.reflow(" doesn't expose any values!"),
                ])
            } else {
                let qualified_suggestions = suggestions
                    .into_iter()
                    .map(|v| alloc.string(module_name.to_string() + "." + v.as_str()));
                alloc.stack([
                    alloc.reflow("Did you mean one of these?"),
                    alloc.vcat(qualified_suggestions).indent(4),
                ])
            };
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The "),
                    alloc.module_name(module_name),
                    alloc.reflow(" module does not expose `"),
                    alloc.string(ident.to_string()),
                    alloc.reflow("`:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                did_you_mean,
            ]);

            title = VALUE_NOT_EXPOSED;
        }

        RuntimeError::ModuleNotImported {
            module_name,
            imported_modules,
            region,
            module_exists,
        } => {
            doc = module_not_found(
                alloc,
                lines,
                region,
                &module_name,
                imported_modules,
                module_exists,
                severity,
            );

            title = MODULE_NOT_IMPORTED;
        }
        RuntimeError::ReadIngestedFileError {
            filename,
            error,
            region: _,
        } => {
            let report = to_file_problem_report(alloc, filename, error);

            doc = report.doc;
            title = INGESTED_FILE_ERROR;
        }
        RuntimeError::InvalidPrecedence(_, _) => {
            // do nothing, reported with PrecedenceProblem
            unreachable!();
        }
        RuntimeError::MalformedIdentifier(_box_str, bad_ident, surroundings) => {
            doc = to_bad_ident_expr_report(alloc, lines, bad_ident, surroundings, severity);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::MalformedTypeName(_box_str, surroundings) => {
            doc = alloc.stack([
                alloc.reflow(r"I am confused by this type name:"),
                alloc.region(lines.convert_region(surroundings), severity),
                alloc.concat([
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
        RuntimeError::MalformedSuffixed(_) => {
            todo!("error for malformed suffix");
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

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This float literal is too "),
                    alloc.text(big_or_small),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc
                        .reflow("Roc uses signed 64-bit floating points, allowing values between "),
                    text!(alloc, "{:e}", f64::MIN),
                    alloc.reflow(" and "),
                    text!(alloc, "{:e}", f64::MAX),
                ]),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidFloat(FloatErrorKind::Error, region, _raw_str) => {
            let tip = alloc
                .tip()
                .append(alloc.reflow("Learn more about number literals at TODO"));

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This float literal contains an invalid digit:"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
                    alloc.reflow("Floating point literals can only contain the digits 0-9, or use scientific notation 10e4, or have a float suffix."),
                ]),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidFloat(FloatErrorKind::IntSuffix, region, _raw_str) => {
            doc = alloc.stack([
                alloc
                    .concat([alloc
                        .reflow("This number literal is a float, but it has an integer suffix:")]),
                alloc.region(lines.convert_region(region), severity),
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

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This "),
                    alloc.text(name),
                    alloc.reflow(" literal contains "),
                    alloc.text(problem),
                    alloc.text(":"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([
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
                    alloc.concat([
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
                    alloc.concat([
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

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This integer literal is too "),
                    alloc.text(big_or_small),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region), severity),
                info,
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::InvalidInt(IntErrorKind::FloatSuffix, _base, region, _raw_str) => {
            doc = alloc.stack([
                alloc
                    .concat([alloc
                        .reflow("This number literal is an integer, but it has a float suffix:")]),
                alloc.region(lines.convert_region(region), severity),
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
            doc = alloc.stack([
                alloc.concat([alloc
                    .reflow("This integer literal overflows the type indicated by its suffix:")]),
                alloc.region(lines.convert_region(region), severity),
                alloc.tip().append(alloc.concat([
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
            doc = alloc.stack([
                alloc.concat([alloc
                    .reflow("This integer literal underflows the type indicated by its suffix:")]),
                alloc.region(lines.convert_region(region), severity),
                alloc.tip().append(alloc.concat([
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
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This expression cannot be updated"),
                    alloc.reflow(":"),
                ]),
                alloc.region(lines.convert_region(region), severity),
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
            doc = alloc.stack([alloc
                .symbol_unqualified(symbol)
                .append(alloc.reflow(" was listed as exposed in "))
                .append(alloc.module(symbol.module_id()))
                .append(alloc.reflow(", but it was not defined anywhere in that module."))]);

            title = MISSING_DEFINITION;
        }
        RuntimeError::EmptySingleQuote(region) => {
            let tip = alloc
                .tip()
                .append(alloc.reflow("Learn more about character literals at TODO"));

            doc = alloc.stack([
                alloc.concat([alloc.reflow("This character literal is empty.")]),
                alloc.region(lines.convert_region(region), severity),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
        }
        RuntimeError::MultipleCharsInSingleQuote(region) => {
            let tip = alloc
                .tip()
                .append(alloc.reflow("Learn more about character literals at TODO"));

            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("This character literal contains more than one code point.")
                ]),
                alloc.region(lines.convert_region(region), severity),
                alloc.concat([alloc.reflow("Character literals can only contain one code point.")]),
                tip,
            ]);

            title = SYNTAX_PROBLEM;
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
                alloc.stack([
                    alloc
                        .tip()
                        .append(alloc.reflow("Did you mean one of these opaque types?")),
                    alloc.vcat(qualified_suggestions).indent(4),
                ])
            };

            let mut stack = vec![
                alloc.concat([
                    alloc.reflow("The opaque type "),
                    alloc.type_str(opaque.as_inline_str().as_str()),
                    alloc.reflow(" referenced here is not defined:"),
                ]),
                alloc.region(lines.convert_region(used_region), severity),
            ];

            if let Some(defined_alias_region) = opt_defined_alias {
                stack.push(alloc.stack([
                    alloc.note("There is an alias of the same name:"),
                    alloc.region(lines.convert_region(defined_alias_region), severity),
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
            doc = alloc.stack([
                alloc.concat([
                    alloc.reflow("The unwrapped opaque type "),
                    alloc.type_str(opaque.as_inline_str().as_str()),
                    alloc.reflow(" referenced here:"),
                ]),
                alloc.region(lines.convert_region(referenced_region), severity),
                alloc.reflow("is imported from another module:"),
                alloc.region(lines.convert_region(imported_region), severity),
                alloc.note(
                    "Opaque types can only be wrapped and unwrapped in the module they are defined in!",
                ),
            ]);

            title = OPAQUE_DECLARED_OUTSIDE_SCOPE;
        }
        RuntimeError::OpaqueNotApplied(loc_ident) => {
            doc = alloc.stack([
                alloc.reflow("This opaque type is not applied to an argument:"),
                alloc.region(lines.convert_region(loc_ident.region), severity),
                alloc.note("Opaque types always wrap exactly one argument!"),
            ]);

            title = OPAQUE_NOT_APPLIED;
        }
        RuntimeError::OpaqueAppliedToMultipleArgs(region) => {
            doc = alloc.stack([
                alloc.reflow("This opaque type is applied to multiple arguments:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.note("Opaque types always wrap exactly one argument!"),
            ]);

            title = OPAQUE_OVER_APPLIED;
        }
        RuntimeError::DegenerateBranch(region) => {
            doc = alloc.stack([
                alloc.reflow("This branch pattern does not bind all symbols its body needs:"),
                alloc.region(lines.convert_region(region), severity),
            ]);

            title = "DEGENERATE BRANCH";
        }
        RuntimeError::EmptyRecordBuilder(region) => {
            doc = alloc.stack([
                alloc.reflow("This record builder has no fields:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("I need at least two fields to combine their values into a record."),
            ]);

            title = "EMPTY RECORD BUILDER";
        }
        RuntimeError::SingleFieldRecordBuilder(region) => {
            doc = alloc.stack([
                alloc.reflow("This record builder only has one field:"),
                alloc.region(lines.convert_region(region), severity),
                alloc.reflow("I need at least two fields to combine their values into a record."),
            ]);

            title = "NOT ENOUGH FIELDS IN RECORD BUILDER";
        }
        RuntimeError::OptionalFieldInRecordBuilder {
            record: record_region,
            field: field_region,
        } => {
            doc = alloc.stack([
                alloc.reflow("Optional fields are not allowed to be used in record builders."),
                alloc.region_with_subregion(
                    lines.convert_region(record_region),
                    lines.convert_region(field_region),
                    severity,
                ),
                alloc.reflow("Record builders can only have required values for their fields."),
            ]);

            title = "OPTIONAL FIELD IN RECORD BUILDER";
        }
    }

    (doc, title)
}

pub fn to_circular_def_doc<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    entries: &[roc_problem::can::CycleEntry],
    severity: Severity,
) -> RocDocBuilder<'b> {
    // TODO "are you trying to mutate a variable?
    // TODO tip?
    match entries {
        [] => unreachable!(),
        [CycleEntry { symbol, symbol_region, expr_region }] =>
             alloc.stack([
                alloc.concat([
                    alloc.symbol_unqualified(*symbol),
                    alloc.reflow(" is defined directly in terms of itself:"),
                ]),
                alloc.region(lines.convert_region(Region::span_across(symbol_region, expr_region)), severity),
                alloc.reflow("Roc evaluates values strictly, so running this program would enter an infinite loop!"),
                alloc.hint("").append(alloc.concat([
                    alloc.reflow("Did you mean to define "),alloc.symbol_unqualified(*symbol),alloc.reflow(" as a function?"),
                ])),
            ]),
        [first, others @ ..] => {
            alloc.stack([
                alloc
                    .reflow("The ")
                    .append(alloc.symbol_unqualified(first.symbol))
                    .append(alloc.reflow(" definition is causing a very tricky infinite loop:")),
                alloc.region(lines.convert_region(first.symbol_region), severity),
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
    options: MutSet<Box<str>>,
    underscored_suggestion_region: Option<Region>,
    severity: Severity,
) -> RocDocBuilder<'b> {
    let mut suggestions = suggest::sort(
        name.as_inline_str().as_str(),
        options.iter().map(|v| v.as_ref()).collect(),
    );
    suggestions.truncate(4);

    let default_no = alloc.concat([
        alloc.reflow("Is there an "),
        alloc.keyword("import"),
        alloc.reflow(" or "),
        alloc.keyword("exposing"),
        alloc.reflow(" missing up-top"),
    ]);

    let default_yes = match underscored_suggestion_region {
        Some(underscored_region) => alloc.stack([
            alloc.reflow("There is an ignored identifier of a similar name here:"),
            alloc.region(lines.convert_region(underscored_region), severity),
            alloc.reflow("Did you mean to remove the leading underscore?"),
            alloc.reflow("If not, did you mean one of these?"),
        ]),
        None => alloc.reflow("Did you mean one of these?"),
    };

    let to_details = |no_suggestion_details, yes_suggestion_details| {
        if suggestions.is_empty() {
            no_suggestion_details
        } else {
            alloc.stack([
                yes_suggestion_details,
                alloc
                    .vcat(suggestions.into_iter().map(|v| alloc.string(v.to_string())))
                    .indent(4),
            ])
        }
    };

    alloc.stack([
        alloc.concat([
            alloc.reflow("Nothing is named `"),
            alloc.string(name.to_string()),
            alloc.reflow("` in this scope."),
        ]),
        alloc.region(lines.convert_region(region), severity),
        to_details(default_no, default_yes),
    ])
}

/// Generate a message informing the user that a module was referenced, but not found
///
/// See [`roc_problem::can::ModuleNotImported`]
fn module_not_found<'b>(
    alloc: &'b RocDocAllocator<'b>,
    lines: &LineInfo,
    region: roc_region::all::Region,
    name: &ModuleName,
    options: MutSet<Box<str>>,
    module_exists: bool,
    severity: Severity,
) -> RocDocBuilder<'b> {
    // If the module exists, suggest that the user import it
    let details = if module_exists {
        // TODO:  Maybe give an example of how to do that
        alloc.reflow("Did you mean to import it?")
    } else {
        // If the module might not exist, suggest that it's a typo
        let mut suggestions =
            suggest::sort(name.as_str(), options.iter().map(|v| v.as_ref()).collect());
        suggestions.truncate(4);

        if suggestions.is_empty() {
            // We don't have any recommended spelling corrections
            alloc.concat([
                alloc.reflow("Is there an "),
                alloc.keyword("import"),
                alloc.reflow(" or "),
                alloc.keyword("exposing"),
                alloc.reflow(" missing up-top"),
            ])
        } else {
            alloc.stack([
                alloc.reflow("Is there an import missing? Perhaps there is a typo. Did you mean one of these?"),
                alloc
                    .vcat(suggestions.into_iter().map(|v| alloc.string(v.to_string())))
                    .indent(4),
            ])
        }
    };

    alloc.stack([
        alloc.concat([
            alloc.reflow("The `"),
            alloc.string(name.to_string()),
            alloc.reflow("` module is not imported:"),
        ]),
        alloc.region(lines.convert_region(region), severity),
        details,
    ])
}
