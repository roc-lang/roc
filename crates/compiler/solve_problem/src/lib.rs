use roc_can::expected::{Expected, PExpected};
use roc_module::symbol::Symbol;
use roc_problem::can::CycleEntry;
use roc_region::all::Region;

use roc_types::types::{Category, ErrorType, PatternCategory};

#[derive(Debug, Clone)]
pub enum TypeError {
    BadExpr(Region, Category, ErrorType, Expected<ErrorType>),
    BadPattern(Region, PatternCategory, ErrorType, PExpected<ErrorType>),
    CircularType(Region, Symbol, ErrorType),
    CircularDef(Vec<CycleEntry>),
    BadType(roc_types::types::Problem),
    UnexposedLookup(Symbol),
    UnfulfilledAbility(Unfulfilled),
    BadExprMissingAbility(Region, Category, ErrorType, Vec<Unfulfilled>),
    BadPatternMissingAbility(Region, PatternCategory, ErrorType, Vec<Unfulfilled>),
    Exhaustive(roc_exhaustive::Error),
    StructuralSpecialization {
        region: Region,
        typ: ErrorType,
        ability: Symbol,
        member: Symbol,
    },
    WrongSpecialization {
        region: Region,
        ability_member: Symbol,
        expected_opaque: Symbol,
        found_opaque: Symbol,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum Unfulfilled {
    /// No claimed implementation of an ability for an opaque type.
    OpaqueDoesNotImplement { typ: Symbol, ability: Symbol },
    /// Cannot derive implementation of an ability for a structural type.
    AdhocUnderivable {
        typ: ErrorType,
        ability: Symbol,
        reason: UnderivableReason,
    },
    /// Cannot derive implementation of an ability for an opaque type.
    OpaqueUnderivable {
        typ: ErrorType,
        ability: Symbol,
        opaque: Symbol,
        derive_region: Region,
        reason: UnderivableReason,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnderivableReason {
    NotABuiltin,
    /// The surface type is not derivable
    SurfaceNotDerivable,
    /// A nested type is not derivable
    NestedNotDerivable(ErrorType),
}
