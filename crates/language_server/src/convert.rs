use std::any::Any;

use roc_load::LoadingProblem;
use roc_parse::parser::{EHeader, EPattern, EType, SourceError, SyntaxError};
use roc_region::all::{LineColumn, LineColumnRegion, LineInfo, Region};
use tower_lsp::lsp_types::{Position, Range};

pub(crate) trait ToRange {
    type Feed;

    fn to_range(&self, feed: &Self::Feed) -> Range;
}

impl ToRange for Region {
    type Feed = LineInfo;

    fn to_range(&self, line_info: &LineInfo) -> Range {
        let LineColumnRegion { start, end } = line_info.convert_region(*self);
        Range {
            start: Position {
                line: start.line,
                character: start.column,
            },
            end: Position {
                line: end.line,
                character: end.column,
            },
        }
    }
}

pub(crate) trait ToRegion {
    type Feed;

    fn to_region(&self, feed: &Self::Feed) -> Region;
}

impl ToRegion for Range {
    type Feed = LineInfo;

    fn to_region(&self, line_info: &LineInfo) -> Region {
        let lc_region = LineColumnRegion {
            start: LineColumn {
                line: self.start.line,
                column: self.start.character,
            },
            end: LineColumn {
                line: self.end.line,
                column: self.end.line,
            },
        };

        line_info.convert_line_column_region(lc_region)
    }
}

pub(crate) trait ToRocPosition {
    type Feed;

    fn to_roc_position(&self, feed: &Self::Feed) -> roc_region::all::Position;
}

impl ToRocPosition for tower_lsp::lsp_types::Position {
    type Feed = LineInfo;

    fn to_roc_position(&self, line_info: &LineInfo) -> roc_region::all::Position {
        let lc = LineColumn {
            line: self.line,
            column: self.character,
        };
        line_info.convert_line_column(lc)
    }
}

fn get_etype_err_region(e_type: &EType) -> Region {
    let pos = match e_type {
        EType::Space(bad_input_error, position) => position,
        EType::UnderscoreSpacing(position) => position,
        EType::TRecord(etype_record, position) => position,
        EType::TTagUnion(etype_tag_union, position) => position,
        EType::TInParens(etype_in_parens, position) => position,
        EType::TApply(etype_apply, position) => position,
        EType::TInlineAlias(etype_inline_alias, position) => position,
        EType::TBadTypeVariable(position) => position,
        EType::TWildcard(position) => position,
        EType::TInferred(position) => position,
        EType::TStart(position) => position,
        EType::TEnd(position) => position,
        EType::TFunctionArgument(position) => position,
        EType::TWhereBar(position) => position,
        EType::TImplementsClause(position) => position,
        EType::TAbilityImpl(etype_ability_impl, position) => position,
        EType::TIndentStart(position) => position,
        EType::TIndentEnd(position) => position,
        EType::TAsIndentStart(position) => position,
    };
    Region::from_pos(*pos)
}

fn get_epattern_region(epattern: &EPattern) -> Region {
    let pos = match epattern {
        EPattern::Record(precord, position) => position,
        EPattern::List(plist, position) => position,
        EPattern::AsKeyword(position) => position,
        EPattern::AsIdentifier(position) => position,
        EPattern::Underscore(position) => position,
        EPattern::NotAPattern(position) => position,
        EPattern::Start(position) => position,
        EPattern::End(position) => position,
        EPattern::Space(bad_input_error, position) => position,
        EPattern::PInParens(pin_parens, position) => position,
        EPattern::NumLiteral(enumber, position) => position,
        EPattern::IndentStart(position) => position,
        EPattern::IndentEnd(position) => position,
        EPattern::AsIndentStart(position) => position,
        EPattern::AccessorFunction(position) => position,
        EPattern::RecordUpdaterFunction(position) => position,
    };
    Region::from_pos(*pos)
}
fn get_eheader_region(eheader: &EHeader) -> Region {
    let pos = match eheader {
        EHeader::Provides(eprovides, position) => position,
        EHeader::Params(eparams, position) => position,
        EHeader::Exposes(eexposes, position) => position,
        EHeader::Imports(eimports, position) => position,
        EHeader::Requires(erequires, position) => position,
        EHeader::Packages(epackages, position) => position,
        EHeader::Space(bad_input_error, position) => position,
        EHeader::Start(position) => position,
        EHeader::ModuleName(position) => position,
        EHeader::AppName(estring, position) => position,
        EHeader::PackageName(epackage_name, position) => position,
        EHeader::PlatformName(epackage_name, position) => position,
        EHeader::IndentStart(position) => position,
        EHeader::InconsistentModuleName(region) => return *region,
    };
    Region::from_pos(*pos)
}

fn get_source_err_region(problem: &SyntaxError) -> Option<Region> {
    let region = match problem {
        SyntaxError::Unexpected(r) => Some(*r),
        SyntaxError::Eof(r) => Some(*r),
        SyntaxError::ReservedKeyword(r) => Some(*r),
        SyntaxError::ArgumentsBeforeEquals(r) => Some(*r),
        SyntaxError::Type(e_type) => Some(get_etype_err_region(e_type)),
        SyntaxError::Pattern(e_pattern) => Some(get_epattern_region(e_pattern)),
        SyntaxError::NotEndOfFile(pos) => Some(Region::from_pos(*pos)),
        SyntaxError::Expr(e_expr, pos) => Some(Region::from_pos(*pos)),
        SyntaxError::Header(e_header) => Some(get_eheader_region(e_header)),
        SyntaxError::NotYetImplemented(_) => None,
        SyntaxError::OutdentedTooFar => None,
        SyntaxError::Todo => None,
        SyntaxError::InvalidPattern => None,
        SyntaxError::BadUtf8 => None,
        SyntaxError::Space(bad_input) => None,
    };
    region
}

pub(crate) fn get_loading_problem_region(problem: &LoadingProblem<'_>) -> Option<Region> {
    match problem {
        LoadingProblem::FileProblem { filename, error } => None,
        LoadingProblem::ParsingFailed(err) => get_source_err_region(&err.problem.problem),
        LoadingProblem::UnexpectedHeader(_) => None,
        LoadingProblem::MultiplePlatformPackages {
            filename,
            module_id,
            source,
            region,
        } => Some(*region),
        LoadingProblem::NoPlatformPackage {
            filename,
            module_id,
            source,
            region,
        } => Some(*region),
        LoadingProblem::UnrecognizedPackageShorthand {
            filename,
            module_id,
            source,
            region,
            shorthand,
            available,
        } => Some(*region),
        LoadingProblem::ErrJoiningWorkerThreads => None,
        LoadingProblem::TriedToImportAppModule => None,
        LoadingProblem::FormattedReport(_) => None,
        LoadingProblem::ImportCycle(_, _) => None,
        LoadingProblem::IncorrectModuleName(_) => None,
        LoadingProblem::CouldNotFindCacheDir => None,
        LoadingProblem::ChannelProblem(_) => None,
    }
}

pub(crate) mod diag {
    use std::path::Path;

    use roc_load::LoadingProblem;
    use roc_region::all::{LineInfo, Region};
    use roc_solve_problem::TypeError;

    use roc_problem::Severity;
    use roc_reporting::report::RocDocAllocator;
    use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

    use super::ToRange;

    pub trait IntoLspSeverity {
        fn into_lsp_severity(self) -> DiagnosticSeverity;
    }

    impl IntoLspSeverity for Severity {
        fn into_lsp_severity(self) -> DiagnosticSeverity {
            match self {
                Severity::RuntimeError => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
                Severity::Fatal => DiagnosticSeverity::ERROR,
            }
        }
    }

    pub trait IntoLspDiagnostic<'a> {
        type Feed;

        fn into_lsp_diagnostic(self, feed: &'a Self::Feed) -> Option<Diagnostic>;
    }

    impl IntoLspDiagnostic<'_> for &LoadingProblem<'_> {
        type Feed = LineInfo;

        fn into_lsp_diagnostic(self, line_info: &LineInfo) -> Option<Diagnostic> {
            let range = crate::convert::get_loading_problem_region(self)
                .unwrap_or(Region::new(
                    roc_region::all::Position::new(0),
                    roc_region::all::Position::new(10000000),
                ))
                .to_range(line_info);

            let msg = match self {
                LoadingProblem::FileProblem { filename, error } => {
                    format!(
                        "Failed to load {} due to an I/O error: {}",
                        filename.display(),
                        error
                    )
                }
                LoadingProblem::MultiplePlatformPackages { filename, .. } => {
                    format!(
                        "Multiple platform packages specified ({}). An app must specify exactly one platform.",
                        filename.display()
                    )
                }
                LoadingProblem::NoPlatformPackage { filename, .. } => {
                    format!(
                        "No platform package specified ({}). An app must specify exactly one platform.",
                        filename.display()
                    )
                }
                LoadingProblem::ParsingFailed(fe) => {
                    let problem = &fe.problem.problem;
                    format!("Failed to parse Roc source file: {problem:?}")
                }
                LoadingProblem::UnexpectedHeader(header) => {
                    format!("Unexpected header: {}", header)
                }
                LoadingProblem::ChannelProblem(_) => {
                    "Internal error: message channel died".to_string()
                }
                LoadingProblem::ErrJoiningWorkerThreads => {
                    "Internal error: analysis worker threads died".to_string()
                }
                LoadingProblem::TriedToImportAppModule => {
                    "Attempted to import app module".to_string()
                }
                LoadingProblem::FormattedReport(report) => report.clone(),
                LoadingProblem::ImportCycle(_, _) => {
                    "Circular dependency between modules".to_string()
                }
                LoadingProblem::IncorrectModuleName(_) => "Incorrect module name".to_string(),
                LoadingProblem::CouldNotFindCacheDir => {
                    format!(
                        "Could not find Roc cache directory {}",
                        roc_packaging::cache::roc_cache_packages_dir().display()
                    )
                }
                LoadingProblem::UnrecognizedPackageShorthand { shorthand, .. } => {
                    format!("Unrecognized package shorthand: {}", shorthand)
                }
            };

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("load".to_owned()),
                message: msg,
                related_information: None,
                tags: None,
                data: None,
            })
        }
    }

    pub struct ProblemFmt<'a> {
        pub alloc: &'a RocDocAllocator<'a>,
        pub line_info: &'a LineInfo,
        pub path: &'a Path,
    }

    impl<'a> IntoLspDiagnostic<'a> for roc_problem::can::Problem {
        type Feed = ProblemFmt<'a>;

        fn into_lsp_diagnostic(self, fmt: &'a ProblemFmt<'a>) -> Option<Diagnostic> {
            let range = self
                .region()
                .unwrap_or_else(Region::zero)
                .to_range(fmt.line_info);

            let report = roc_reporting::report::can_problem(
                fmt.alloc,
                fmt.line_info,
                fmt.path.to_path_buf(),
                self,
            );

            let severity = report.severity.into_lsp_severity();
            let mut msg = String::new();
            report.render_language_server(&mut msg, fmt.alloc);

            Some(Diagnostic {
                range,
                severity: Some(severity),
                code: None,
                code_description: None,
                source: None,
                message: msg,
                related_information: None,
                tags: None,
                data: None,
            })
        }
    }

    impl<'a> IntoLspDiagnostic<'a> for TypeError {
        type Feed = ProblemFmt<'a>;

        fn into_lsp_diagnostic(self, fmt: &'a ProblemFmt<'a>) -> Option<Diagnostic> {
            let range = self
                .region()
                .unwrap_or_else(Region::zero)
                .to_range(fmt.line_info);

            let report = roc_reporting::report::type_problem(
                fmt.alloc,
                fmt.line_info,
                fmt.path.to_path_buf(),
                self,
            )?;

            let severity = report.severity.into_lsp_severity();

            let mut msg = String::new();
            report.render_language_server(&mut msg, fmt.alloc);

            Some(Diagnostic {
                range,
                severity: Some(severity),
                code: None,
                code_description: None,
                source: None,
                message: msg,
                related_information: None,
                tags: None,
                data: None,
            })
        }
    }
}
