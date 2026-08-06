//! Roc target definitions and link configuration
//!
//! Re-exports types from the compile layer and roc_target module.

const compile = @import("compile");

// Re-export RocTarget from the shared build module
pub const RocTarget = @import("roc_target").RocTarget;
pub const CpuLevel = @import("roc_target").CpuLevel;
pub const macos_deployment = @import("roc_target").macos_deployment;
pub const llvmCpuName = @import("roc_target").llvmCpuName;
pub const llvmFeatureString = @import("roc_target").llvmFeatureString;
pub const host_cpu = @import("roc_target").host_cpu;

// Re-export link configuration types from the compile layer
pub const TargetsConfig = compile.targets_config.TargetsConfig;
pub const TargetLinkSpec = compile.targets_config.TargetLinkSpec;
pub const LinkItem = compile.targets_config.LinkItem;
pub const OutputKind = compile.targets_config.OutputKind;
pub const WasmTargetConfig = compile.targets_config.WasmTargetConfig;
