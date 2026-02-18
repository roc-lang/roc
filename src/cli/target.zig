//! Roc target definitions and link configuration
//!
//! Re-exports types from the compile layer and roc_target module.

const compile = @import("compile");

// Re-export RocTarget from the shared build module
pub const RocTarget = @import("roc_target").RocTarget;

// Re-export link configuration types from the compile layer
pub const TargetsConfig = compile.targets_config.TargetsConfig;
pub const TargetLinkSpec = compile.targets_config.TargetLinkSpec;
pub const LinkItem = compile.targets_config.LinkItem;
pub const LinkType = compile.targets_config.LinkType;
