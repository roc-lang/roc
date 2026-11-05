//! The facts a checked module derives from its own tables and from the
//! modules it reaches, each computed by the first consumer that needs it,
//! exactly once, into a slot the module owns (design.md "On-Demand Analyses
//! And Write-Once Slots"). Nothing here is serialized.
//!
//! An analysis that reads other modules finds them through the
//! `ModuleResolver` its consumer passes. Every resolver a consumer builds
//! resolves every module of the build, so the result never depends on which
//! consumer computed it.

const std = @import("std");
const collections = @import("collections");
const checked_artifact = @import("checked_artifact.zig");

const Allocator = std.mem.Allocator;
const CheckedModuleArtifact = checked_artifact.CheckedModuleArtifact;
const ImportedModuleView = checked_artifact.ImportedModuleView;
const ModuleResolver = checked_artifact.ModuleResolver;
const ExportClosures = checked_artifact.ExportClosures;
const PublicApiDependencies = checked_artifact.PublicApiDependencies;
const LoweringVisibility = checked_artifact.LoweringVisibility;
const ModuleInterfaceCapabilities = checked_artifact.ModuleInterfaceCapabilities;
const CheckedTypeStore = checked_artifact.CheckedTypeStore;
const canonical = @import("canonical_names.zig");

/// The per-module analyses of one checked module. `allocator` must be safe
/// to use from every thread that reads the module.
pub const ModuleAnalyses = struct {
    allocator: Allocator,
    io: std.Io,
    /// The platform a platform/app pairing reads: the pairing's export
    /// closures and public API dependencies are its platform's.
    platform: ?*const CheckedModuleArtifact,
    export_closures: collections.OnceSlot(*ExportClosures, .module_export_closures) = .empty,
    public_api_dependencies: collections.OnceSlot(*PublicApiDependencies, .module_public_api_dependencies) = .empty,
    lowering_visibility: collections.OnceSlot(*LoweringVisibility, .module_lowering_visibility) = .empty,
    interface_capabilities: collections.OnceSlot(*ModuleInterfaceCapabilities, .module_interface_capabilities) = .empty,

    pub fn create(allocator: Allocator, io: std.Io, platform: ?*const CheckedModuleArtifact) Allocator.Error!*ModuleAnalyses {
        const analyses = try allocator.create(ModuleAnalyses);
        analyses.* = .{ .allocator = allocator, .io = io, .platform = platform };
        return analyses;
    }

    pub fn destroy(self: *ModuleAnalyses) void {
        const allocator = self.allocator;
        if (self.export_closures.peek()) |closures| closures.destroy(allocator);
        if (self.public_api_dependencies.peek()) |dependencies| {
            dependencies.deinit(allocator);
            allocator.destroy(dependencies);
        }
        if (self.lowering_visibility.peek()) |visibility| {
            visibility.deinit(allocator);
            allocator.destroy(visibility);
        }
        if (self.interface_capabilities.peek()) |capabilities| {
            capabilities.deinit(allocator);
            allocator.destroy(capabilities);
        }
        allocator.destroy(self);
    }

    /// The template closure of every row of `owner`'s export tables.
    pub fn exportClosures(self: *ModuleAnalyses, owner: ImportedModuleView, resolver: ModuleResolver) Allocator.Error!*const ExportClosures {
        if (self.platform) |platform| return platformAnalyses(platform).exportClosures(checked_artifact.importedView(platform), resolver);
        return self.export_closures.getOrCompute(self.io, Allocator.Error, Request{ .analyses = self, .owner = owner, .resolver = resolver }, Request.exportClosures);
    }

    /// The checked modules a consumer of `owner`'s public API reads.
    pub fn publicApiDependencies(self: *ModuleAnalyses, owner: ImportedModuleView, resolver: ModuleResolver) Allocator.Error!*const PublicApiDependencies {
        if (self.platform) |platform| return platformAnalyses(platform).publicApiDependencies(checked_artifact.importedView(platform), resolver);
        return self.public_api_dependencies.getOrCompute(self.io, Allocator.Error, Request{ .analyses = self, .owner = owner, .resolver = resolver }, Request.publicApiDependencies);
    }

    /// The checked modules lowering `owner`'s roots reads.
    pub fn loweringVisibility(self: *ModuleAnalyses, owner: ImportedModuleView, resolver: ModuleResolver) Allocator.Error!*const LoweringVisibility {
        return self.lowering_visibility.getOrCompute(self.io, Allocator.Error, Request{ .analyses = self, .owner = owner, .resolver = resolver }, Request.loweringVisibility);
    }

    /// The box payload capabilities of the local nominal types of `store`,
    /// the store of this module's types.
    pub fn interfaceCapabilities(self: *ModuleAnalyses, store: *CheckedTypeStore, current_module: canonical.ModuleIdentityId) Allocator.Error!*const ModuleInterfaceCapabilities {
        return self.interface_capabilities.getOrCompute(self.io, Allocator.Error, CapabilityRequest{ .analyses = self, .store = store, .current_module = current_module }, CapabilityRequest.compute);
    }

    /// The interface capabilities when a consumer has demanded them.
    pub fn peekInterfaceCapabilities(self: *const ModuleAnalyses) ?*const ModuleInterfaceCapabilities {
        return self.interface_capabilities.peek();
    }

    const CapabilityRequest = struct {
        analyses: *ModuleAnalyses,
        store: *CheckedTypeStore,
        current_module: canonical.ModuleIdentityId,

        fn compute(request: CapabilityRequest) Allocator.Error!*ModuleInterfaceCapabilities {
            const analyses = request.analyses;
            const allocator = analyses.allocator;
            const capabilities = try allocator.create(ModuleInterfaceCapabilities);
            errdefer allocator.destroy(capabilities);
            request.store.beginCreation(analyses.io);
            defer request.store.endCreation(analyses.io);
            capabilities.* = try ModuleInterfaceCapabilities.compute(allocator, request.store, request.current_module);
            return capabilities;
        }
    };

    fn platformAnalyses(platform: *const CheckedModuleArtifact) *ModuleAnalyses {
        return platform.module_analyses orelse checked_artifact.checkedArtifactInvariant("a pairing's platform has no module analyses", .{});
    }

    const Request = struct {
        analyses: *ModuleAnalyses,
        owner: ImportedModuleView,
        resolver: ModuleResolver,

        fn exportClosures(request: Request) Allocator.Error!*ExportClosures {
            const allocator = request.analyses.allocator;
            const closures = try allocator.create(ExportClosures);
            errdefer allocator.destroy(closures);
            closures.* = try checked_artifact.computeExportClosures(allocator, request.owner, request.resolver);
            return closures;
        }

        fn publicApiDependencies(request: Request) Allocator.Error!*PublicApiDependencies {
            const allocator = request.analyses.allocator;
            const dependencies = try allocator.create(PublicApiDependencies);
            errdefer allocator.destroy(dependencies);
            dependencies.* = try checked_artifact.computePublicApiDependencies(allocator, request.owner, request.resolver);
            return dependencies;
        }

        fn loweringVisibility(request: Request) Allocator.Error!*LoweringVisibility {
            const allocator = request.analyses.allocator;
            const visibility = try allocator.create(LoweringVisibility);
            errdefer allocator.destroy(visibility);
            visibility.* = try checked_artifact.computeLoweringVisibility(allocator, request.owner, request.resolver);
            return visibility;
        }
    };
};
