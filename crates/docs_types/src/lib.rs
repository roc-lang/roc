//! A representation of Roc types that can be used in rendering documentation.
//! This crate is not concerned with how to obtain these, only with representing them.
#![cfg_attr(not(feature = "std"), no_std)]

mod type_visitor;

pub use type_visitor::{AbilityMember, RecordField, TypeVisitor};
