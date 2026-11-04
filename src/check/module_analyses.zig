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
const static_dispatch = @import("static_dispatch_registry.zig");
const canonical = @import("canonical_names.zig");
const CheckedDispatch = checked_artifact.CheckedDispatch;
const SchemeAnalysis = checked_artifact.SchemeAnalysis;
const TemplateDispatch = checked_artifact.TemplateDispatch;
const TemplateDispatchRelations = checked_artifact.TemplateDispatchRelations;
const ModuleDispatch = checked_artifact.ModuleDispatch;
const DispatchIndex = checked_artifact.DispatchIndex;
const PlatformRequirementRootEvidence = checked_artifact.PlatformRequirementRootEvidence;
const DispatchScopeId = checked_artifact.DispatchScopeId;

const TemplateSchemaSlot = collections.OnceSlot(*SchemeAnalysis, .module_template_schema);
const ScopeSchemaSlot = collections.OnceSlot(*SchemeAnalysis, .module_scope_schema);
const TemplateDispatchSlot = collections.OnceSlot(*TemplateDispatch, .module_template_dispatch);
const TemplateRelationsSlot = collections.OnceSlot(*TemplateDispatchRelations, .module_template_dispatch_relations);

/// The per-template and per-scope dispatch slots of a module, one for each
/// template and generalized local scope its dispatch sites name.
const DispatchSlots = struct {
    template_schemas: []TemplateSchemaSlot,
    scope_schemas: []ScopeSchemaSlot,
    template_dispatch: []TemplateDispatchSlot,
    template_relations: []TemplateRelationsSlot,

    fn create(allocator: Allocator, template_count: usize, scope_count: usize) Allocator.Error!*DispatchSlots {
        const slots = try allocator.create(DispatchSlots);
        errdefer allocator.destroy(slots);
        const template_schemas = try allocator.alloc(TemplateSchemaSlot, template_count);
        errdefer allocator.free(template_schemas);
        const scope_schemas = try allocator.alloc(ScopeSchemaSlot, scope_count);
        errdefer allocator.free(scope_schemas);
        const template_dispatch = try allocator.alloc(TemplateDispatchSlot, template_count);
        errdefer allocator.free(template_dispatch);
        const template_relations = try allocator.alloc(TemplateRelationsSlot, template_count);
        @memset(template_schemas, .empty);
        @memset(scope_schemas, .empty);
        @memset(template_dispatch, .empty);
        @memset(template_relations, .empty);
        slots.* = .{
            .template_schemas = template_schemas,
            .scope_schemas = scope_schemas,
            .template_dispatch = template_dispatch,
            .template_relations = template_relations,
        };
        return slots;
    }

    fn destroy(self: *DispatchSlots, allocator: Allocator) void {
        for (self.template_schemas) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        for (self.scope_schemas) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        for (self.template_dispatch) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        for (self.template_relations) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        allocator.free(self.template_schemas);
        allocator.free(self.scope_schemas);
        allocator.free(self.template_dispatch);
        allocator.free(self.template_relations);
        allocator.destroy(self);
    }
};

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
    /// The modules dispatch resolution searches for method targets and
    /// procedure schemes, in search order (`CheckedModuleArtifact.dispatch_view_keys`).
    dispatch_imports: []const ImportedModuleView = &.{},
    dispatch_slots: collections.OnceSlot(*DispatchSlots, .module_dispatch_slots) = .empty,
    dispatch_index: collections.OnceSlot(*DispatchIndex, .module_dispatch_index) = .empty,
    module_dispatch: collections.OnceSlot(*ModuleDispatch, .module_dispatch) = .empty,
    platform_root_evidence: collections.OnceSlot(*PlatformRequirementRootEvidence, .module_platform_root_evidence) = .empty,

    pub fn create(allocator: Allocator, io: std.Io, platform: ?*const CheckedModuleArtifact) Allocator.Error!*ModuleAnalyses {
        const analyses = try allocator.create(ModuleAnalyses);
        analyses.* = .{ .allocator = allocator, .io = io, .platform = platform };
        return analyses;
    }

    /// Give the analyses the modules dispatch resolution searches, in
    /// search order. The views must stay valid for as long as the module.
    pub fn setDispatchImports(self: *ModuleAnalyses, views: []const ImportedModuleView) Allocator.Error!void {
        std.debug.assert(self.dispatch_imports.len == 0);
        self.dispatch_imports = try self.allocator.dupe(ImportedModuleView, views);
    }

    pub fn destroy(self: *ModuleAnalyses) void {
        const allocator = self.allocator;
        if (self.dispatch_slots.peek()) |slots| slots.destroy(allocator);
        if (self.dispatch_index.peek()) |index| index.destroy();
        if (self.module_dispatch.peek()) |result| result.destroy(allocator);
        if (self.platform_root_evidence.peek()) |result| result.destroy(allocator);
        allocator.free(self.dispatch_imports);
        if (self.export_closures.peek()) |closures| closures.destroy(allocator);
        if (self.public_api_dependencies.peek()) |dependencies| {
            dependencies.deinit(allocator);
            allocator.destroy(dependencies);
        }
        if (self.lowering_visibility.peek()) |visibility| {
            visibility.deinit(allocator);
            allocator.destroy(visibility);
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

    fn dispatchSlots(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*DispatchSlots {
        return self.dispatch_slots.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.createSlots);
    }

    /// The schema of procedure template `template`.
    pub fn templateSchema(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const SchemeAnalysis {
        const all = try self.dispatchSlots(dispatch);
        const raw = @backingInt(template);
        if (raw >= all.template_schemas.len) return &checked_artifact.empty_scheme_analysis;
        return all.template_schemas[raw].getOrCompute(self.io, Allocator.Error, TemplateRequest{ .analyses = self, .dispatch = dispatch, .template = template }, TemplateRequest.schema);
    }

    /// The schema of generalized local scope `scope`.
    pub fn scopeSchema(self: *ModuleAnalyses, dispatch: CheckedDispatch, scope: DispatchScopeId) Allocator.Error!*const SchemeAnalysis {
        const all = try self.dispatchSlots(dispatch);
        return all.scope_schemas[@backingInt(scope)].getOrCompute(self.io, Allocator.Error, ScopeRequest{ .analyses = self, .dispatch = dispatch, .scope = scope }, ScopeRequest.schema);
    }

    /// The dispatch resolution of procedure template `template`.
    pub fn templateDispatch(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const TemplateDispatch {
        const all = try self.dispatchSlots(dispatch);
        const raw = @backingInt(template);
        if (raw >= all.template_dispatch.len) return &checked_artifact.empty_template_dispatch;
        return all.template_dispatch[raw].getOrCompute(self.io, Allocator.Error, TemplateRequest{ .analyses = self, .dispatch = dispatch, .template = template }, TemplateRequest.dispatchResolution);
    }

    /// How procedure template `template`'s plans relate to its
    /// specialization.
    pub fn templateDispatchRelations(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const TemplateDispatchRelations {
        const all = try self.dispatchSlots(dispatch);
        const raw = @backingInt(template);
        if (raw >= all.template_relations.len) return &checked_artifact.empty_template_relations;
        return all.template_relations[raw].getOrCompute(self.io, Allocator.Error, TemplateRequest{ .analyses = self, .dispatch = dispatch, .template = template }, TemplateRequest.relations);
    }

    /// The module-wide indexes dispatch resolution reads.
    pub fn dispatchIndex(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*const DispatchIndex {
        return self.dispatch_index.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.index);
    }

    /// The module's dispatch resolution of what no template resolves.
    pub fn moduleDispatch(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*const ModuleDispatch {
        return self.module_dispatch.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.moduleResolution);
    }

    /// An app's root evidence for each of its platform requirement
    /// solutions.
    pub fn platformRequirementRootEvidence(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*const PlatformRequirementRootEvidence {
        return self.platform_root_evidence.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.platformRootEvidence);
    }

    /// The resolution of plan `plan`: the first template in template order
    /// whose chain binds it, otherwise the module's.
    pub fn planResolution(self: *ModuleAnalyses, dispatch: CheckedDispatch, plan: static_dispatch.StaticDispatchPlanId) Allocator.Error!static_dispatch.PlanResolution {
        const index = try self.dispatchIndex(dispatch);
        for (index.planSites(plan)) |reach| {
            const template = reach.template orelse continue;
            if ((try self.templateDispatch(dispatch, @fromBackingInt(template))).plan(plan)) |resolution| return resolution;
        }
        return (try self.moduleDispatch(dispatch)).plan(plan) orelse
            checked_artifact.checkedArtifactInvariant("a dispatch plan had no resolution", .{});
    }

    /// The resolution of iterator plan `plan`, found as `planResolution`
    /// finds a plan's.
    pub fn iteratorPlanResolution(self: *ModuleAnalyses, dispatch: CheckedDispatch, plan: static_dispatch.IteratorForPlanId) Allocator.Error!static_dispatch.IteratorPlanResolution {
        const index = try self.dispatchIndex(dispatch);
        for (index.iteratorSites(plan)) |reach| {
            const template = reach.template orelse continue;
            if ((try self.templateDispatch(dispatch, @fromBackingInt(template))).iteratorPlan(plan)) |resolution| return resolution;
        }
        return (try self.moduleDispatch(dispatch)).iteratorPlan(plan) orelse
            checked_artifact.checkedArtifactInvariant("an iterator plan had no resolution", .{});
    }

    /// The evidence of use site `site`: the first template in template order
    /// that resolved it, otherwise the module's; null when no analysis
    /// recorded evidence for it.
    pub fn siteEvidence(self: *ModuleAnalyses, dispatch: CheckedDispatch, site: u32) Allocator.Error!?checked_artifact.SiteEvidenceLookup {
        const index = try self.dispatchIndex(dispatch);
        for (index.siteTemplates(site)) |reach| {
            const template: canonical.CheckedProcedureTemplateId = @fromBackingInt(reach.template);
            if ((try self.templateDispatch(dispatch, template)).site(site)) |entry| return .{ .entry = entry.*, .owner = static_dispatch.EvidenceOwner.forTemplate(template) };
        }
        if ((try self.moduleDispatch(dispatch)).site(site)) |entry| return .{ .entry = entry.*, .owner = .module };
        return null;
    }

    const DispatchRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,

        fn createSlots(request: DispatchRequest) Allocator.Error!*DispatchSlots {
            const source = request.dispatch.source;
            return DispatchSlots.create(
                request.analyses.allocator,
                source.templates.dispatch_sites.walkedTemplateCount(),
                source.templates.dispatch_scopes.len,
            );
        }

        fn index(request: DispatchRequest) Allocator.Error!*DispatchIndex {
            return checked_artifact.computeDispatchIndex(request.analyses.allocator, request.dispatch);
        }

        fn moduleResolution(request: DispatchRequest) Allocator.Error!*ModuleDispatch {
            return checked_artifact.computeModuleDispatch(request.analyses.allocator, request.dispatch);
        }

        fn platformRootEvidence(request: DispatchRequest) Allocator.Error!*PlatformRequirementRootEvidence {
            return checked_artifact.computePlatformRequirementRootEvidence(request.analyses.allocator, request.dispatch);
        }
    };

    const TemplateRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,
        template: canonical.CheckedProcedureTemplateId,

        fn schema(request: TemplateRequest) Allocator.Error!*SchemeAnalysis {
            return checked_artifact.computeTemplateSchema(request.analyses.allocator, request.dispatch, request.template);
        }

        fn dispatchResolution(request: TemplateRequest) Allocator.Error!*TemplateDispatch {
            return checked_artifact.computeTemplateDispatch(request.analyses.allocator, request.dispatch, request.template);
        }

        fn relations(request: TemplateRequest) Allocator.Error!*TemplateDispatchRelations {
            return checked_artifact.computeTemplateDispatchRelations(request.analyses.allocator, request.dispatch, request.template);
        }
    };

    const ScopeRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,
        scope: DispatchScopeId,

        fn schema(request: ScopeRequest) Allocator.Error!*SchemeAnalysis {
            return checked_artifact.computeScopeSchema(request.analyses.allocator, request.dispatch, request.scope);
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
