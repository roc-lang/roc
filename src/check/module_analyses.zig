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
const static_dispatch = @import("static_dispatch_registry.zig");
const CheckedDispatch = checked_artifact.CheckedDispatch;
const SchemeAnalysis = checked_artifact.SchemeAnalysis;
const TemplateDispatch = checked_artifact.TemplateDispatch;
const TemplateDispatchRelations = checked_artifact.TemplateDispatchRelations;
const ModuleDispatch = checked_artifact.ModuleDispatch;
const DispatchIndex = checked_artifact.DispatchIndex;
const PlatformRequirementRootEvidence = checked_artifact.PlatformRequirementRootEvidence;
const DispatchScopeId = checked_artifact.DispatchScopeId;
const CheckedBodies = checked_artifact.CheckedBodies;
const NestedProcSiteOwner = checked_artifact.NestedProcSiteOwner;
const OwnerNestedProcSites = checked_artifact.OwnerNestedProcSites;
const ModuleNestedProcSites = checked_artifact.ModuleNestedProcSites;
const TemplateExhaustivenessSites = checked_artifact.TemplateExhaustivenessSites;
const CheckedExhaustivenessSiteTable = checked_artifact.CheckedExhaustivenessSiteTable;
const CIR = @import("can").CIR;
const TemplateDemandGraph = checked_artifact.TemplateDemandGraph;
const TemplateDemandComponent = checked_artifact.TemplateDemandComponent;
const SchemeVarDemand = checked_artifact.SchemeVarDemand;

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

const OwnerNestedSitesSlot = collections.OnceSlot(*OwnerNestedProcSites, .module_owner_nested_proc_sites);
const TemplateExhaustivenessSlot = collections.OnceSlot(*TemplateExhaustivenessSites, .module_template_exhaustiveness_sites);

/// The per-owner slots of a module's body analyses: the nested procedure
/// sites of each template and of each defaulted-field default, and the
/// exhaustiveness sites each template's body reaches.
const OwnerSlots = struct {
    template_nested_sites: []OwnerNestedSitesSlot,
    default_nested_sites: []OwnerNestedSitesSlot,
    template_exhaustiveness: []TemplateExhaustivenessSlot,
    /// Each generalized local scope by the expression it is checked at.
    scope_by_checked_expr: collections.DenseMap(CIR.Expr.Idx, DispatchScopeId),

    fn create(allocator: Allocator, bodies: CheckedBodies) Allocator.Error!*OwnerSlots {
        const templates = (bodies.tables.templates orelse checked_artifact.checkedArtifactInvariant("owner analyses were demanded of bodies without templates", .{}));
        const template_count = templates.templates.items.len;
        const slots = try allocator.create(OwnerSlots);
        errdefer allocator.destroy(slots);
        const template_nested_sites = try allocator.alloc(OwnerNestedSitesSlot, template_count);
        errdefer allocator.free(template_nested_sites);
        const default_nested_sites = try allocator.alloc(OwnerNestedSitesSlot, bodies.index().defaults.len);
        errdefer allocator.free(default_nested_sites);
        const template_exhaustiveness = try allocator.alloc(TemplateExhaustivenessSlot, template_count);
        errdefer allocator.free(template_exhaustiveness);
        @memset(template_nested_sites, .empty);
        @memset(default_nested_sites, .empty);
        @memset(template_exhaustiveness, .empty);
        slots.* = .{
            .template_nested_sites = template_nested_sites,
            .default_nested_sites = default_nested_sites,
            .template_exhaustiveness = template_exhaustiveness,
            .scope_by_checked_expr = try checked_artifact.nestedProcScopeMap(allocator, templates.dispatch_scopes),
        };
        return slots;
    }

    fn destroy(self: *OwnerSlots, allocator: Allocator) void {
        for (self.template_nested_sites) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        for (self.default_nested_sites) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        for (self.template_exhaustiveness) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        allocator.free(self.template_nested_sites);
        allocator.free(self.default_nested_sites);
        allocator.free(self.template_exhaustiveness);
        self.scope_by_checked_expr.deinit();
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
    interface_capabilities: collections.OnceSlot(*ModuleInterfaceCapabilities, .module_interface_capabilities) = .empty,
    /// The modules dispatch resolution searches for method targets and
    /// procedure schemes, in search order (`CheckedModuleArtifact.dispatch_view_keys`).
    dispatch_imports: []const ImportedModuleView = &.{},
    dispatch_slots: collections.OnceSlot(*DispatchSlots, .module_dispatch_slots) = .empty,
    dispatch_index: collections.OnceSlot(*DispatchIndex, .module_dispatch_index) = .empty,
    module_dispatch: collections.OnceSlot(*ModuleDispatch, .module_dispatch) = .empty,
    platform_root_evidence: collections.OnceSlot(*PlatformRequirementRootEvidence, .module_platform_root_evidence) = .empty,
    owner_slots: collections.OnceSlot(*OwnerSlots, .module_owner_slots) = .empty,
    nested_proc_sites: collections.OnceSlot(*ModuleNestedProcSites, .module_nested_proc_sites) = .empty,
    exhaustiveness_sites: collections.OnceSlot(*CheckedExhaustivenessSiteTable, .module_exhaustiveness_sites) = .empty,
    template_demand_graph: collections.OnceSlot(*TemplateDemandGraph, .module_template_demand_graph) = .empty,

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
        if (self.owner_slots.peek()) |slots| slots.destroy(allocator);
        if (self.nested_proc_sites.peek()) |result| result.destroy(allocator);
        if (self.exhaustiveness_sites.peek()) |result| result.destroy(allocator);
        if (self.template_demand_graph.peek()) |graph| graph.destroy(allocator);
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

    fn ownerSlots(self: *ModuleAnalyses, bodies: CheckedBodies) Allocator.Error!*OwnerSlots {
        return self.owner_slots.getOrCompute(self.io, Allocator.Error, BodiesRequest{ .analyses = self, .bodies = bodies }, BodiesRequest.ownerSlots);
    }

    /// The nested procedure sites of `owner`'s body. `bodies` are this
    /// module's; a platform/app pairing reads its platform's sites.
    pub fn nestedProcSites(self: *ModuleAnalyses, bodies: CheckedBodies, owner: NestedProcSiteOwner) Allocator.Error!*const OwnerNestedProcSites {
        if (self.platform) |platform| return platformAnalyses(platform).nestedProcSites(platform.bodies(), owner);
        const slots = try self.ownerSlots(bodies);
        const slot = switch (owner) {
            .template => |template| blk: {
                const raw = @backingInt(template.template);
                if (raw >= slots.template_nested_sites.len) checked_artifact.checkedArtifactInvariant("nested procedure sites were demanded of a template outside the module's templates", .{});
                break :blk &slots.template_nested_sites[raw];
            },
            .default_root => |default| &slots.default_nested_sites[defaultPosition(bodies, default)],
        };
        return slot.getOrCompute(self.io, Allocator.Error, OwnerRequest{ .bodies = bodies, .slots = slots, .owner = owner, .allocator = self.allocator }, OwnerRequest.nestedProcSites);
    }

    /// The module's nested procedure sites by expression, with their runtime
    /// captures. A platform/app pairing reads its platform's.
    pub fn moduleNestedProcSites(self: *ModuleAnalyses, bodies: CheckedBodies) Allocator.Error!*const ModuleNestedProcSites {
        if (self.platform) |platform| return platformAnalyses(platform).moduleNestedProcSites(platform.bodies());
        return self.nested_proc_sites.getOrCompute(self.io, Allocator.Error, BodiesRequest{ .analyses = self, .bodies = bodies }, BodiesRequest.moduleNestedProcSites);
    }

    /// The exhaustiveness sites procedure template `template`'s body
    /// reaches. A platform/app pairing reads its platform's.
    pub fn templateExhaustivenessSites(self: *ModuleAnalyses, bodies: CheckedBodies, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const TemplateExhaustivenessSites {
        if (self.platform) |platform| return platformAnalyses(platform).templateExhaustivenessSites(platform.bodies(), template);
        const slots = try self.ownerSlots(bodies);
        const raw = @backingInt(template);
        if (raw >= slots.template_exhaustiveness.len) checked_artifact.checkedArtifactInvariant("exhaustiveness sites were demanded of a template outside the module's templates", .{});
        return slots.template_exhaustiveness[raw].getOrCompute(self.io, Allocator.Error, ExhaustivenessRequest{ .allocator = self.allocator, .bodies = bodies, .template = template }, ExhaustivenessRequest.compute);
    }

    /// Every exhaustiveness check of `owner` with the policy that decides
    /// it. A platform/app pairing decides its platform's checks by its own
    /// root requests.
    pub fn exhaustivenessSites(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const CheckedExhaustivenessSiteTable {
        const platform_sites: ?*const CheckedExhaustivenessSiteTable = if (self.platform) |platform|
            try platformAnalyses(platform).exhaustivenessSites(checked_artifact.importedView(platform))
        else
            null;
        return self.exhaustiveness_sites.getOrCompute(self.io, Allocator.Error, PolicyRequest{ .analyses = self, .owner = owner, .platform_sites = platform_sites }, PolicyRequest.compute);
    }

    fn defaultPosition(bodies: CheckedBodies, default: CIR.Expr.Idx) usize {
        const defaults = bodies.index().defaults;
        return std.sort.binarySearch(u32, defaults, @backingInt(default), struct {
            fn order(key: u32, item: u32) std.math.Order {
                return std.math.order(key, item);
            }
        }.order) orelse checked_artifact.checkedArtifactInvariant("nested procedure sites were demanded of an expression that is not a defaulted-field default", .{});
    }

    const BodiesRequest = struct {
        analyses: *ModuleAnalyses,
        bodies: CheckedBodies,

        fn ownerSlots(request: BodiesRequest) Allocator.Error!*OwnerSlots {
            return OwnerSlots.create(request.analyses.allocator, request.bodies);
        }

        fn moduleNestedProcSites(request: BodiesRequest) Allocator.Error!*ModuleNestedProcSites {
            return checked_artifact.computeModuleNestedProcSites(request.analyses.allocator, request.bodies, request.analyses);
        }
    };

    const OwnerRequest = struct {
        allocator: Allocator,
        bodies: CheckedBodies,
        slots: *const OwnerSlots,
        owner: NestedProcSiteOwner,

        fn nestedProcSites(request: OwnerRequest) Allocator.Error!*OwnerNestedProcSites {
            return checked_artifact.computeOwnerNestedProcSites(request.allocator, request.bodies, &request.slots.scope_by_checked_expr, request.owner);
        }
    };

    const ExhaustivenessRequest = struct {
        allocator: Allocator,
        bodies: CheckedBodies,
        template: canonical.CheckedProcedureTemplateId,

        fn compute(request: ExhaustivenessRequest) Allocator.Error!*TemplateExhaustivenessSites {
            return checked_artifact.computeTemplateExhaustivenessSites(request.allocator, request.bodies, request.template);
        }
    };

    const PolicyRequest = struct {
        analyses: *ModuleAnalyses,
        owner: ImportedModuleView,
        platform_sites: ?*const CheckedExhaustivenessSiteTable,

        fn compute(request: PolicyRequest) Allocator.Error!*CheckedExhaustivenessSiteTable {
            return checked_artifact.computeExhaustivenessSites(request.analyses.allocator, request.analyses, request.owner, request.platform_sites);
        }
    };

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

    /// The module's demand call graph: every template's own demand vector,
    /// its demand edges and the strongly connected components they form.
    pub fn templateDemandGraph(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*TemplateDemandGraph {
        return self.template_demand_graph.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.demandGraph);
    }

    /// The demand vector of procedure template `template`.
    pub fn templateDemands(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error![]const SchemeVarDemand {
        const graph = try self.templateDemandGraph(dispatch);
        const raw = @backingInt(template);
        const component = try self.templateDemandComponent(dispatch, graph, graph.component_of[raw]);
        return component.member(graph.member_index[raw]);
    }

    /// The demand vectors of component `component` of `graph`. Before it
    /// claims a component, a consumer demands, holding no claim, every
    /// component it reaches through local edges (callees first) and every
    /// imported template those components call, so a component's
    /// computation only reads finished vectors and never waits on a slot of
    /// its own kind.
    fn templateDemandComponent(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *TemplateDemandGraph, component: u32) Allocator.Error!*const TemplateDemandComponent {
        if (graph.component_slots[component].peek()) |done| return done;
        const allocator = self.allocator;
        var visited = try std.DynamicBitSetUnmanaged.initEmpty(allocator, graph.component_slots.len);
        defer visited.deinit(allocator);
        var order: std.ArrayList(u32) = .empty;
        defer order.deinit(allocator);
        const Frame = struct { component: u32, next_callee: u32 };
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(allocator);
        visited.set(component);
        try frames.append(allocator, .{ .component = component, .next_callee = 0 });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            const callees = graph.componentCallees(frame.component);
            if (frame.next_callee < callees.len) {
                const callee = callees[frame.next_callee];
                frame.next_callee += 1;
                if (visited.isSet(callee) or graph.component_slots[callee].peek() != null) continue;
                visited.set(callee);
                try frames.append(allocator, .{ .component = callee, .next_callee = 0 });
                continue;
            }
            try order.append(allocator, frame.component);
            _ = frames.pop();
        }
        for (order.items) |next| {
            for (graph.componentMembers(next)) |member| for (graph.templateEdges(member)) |edge| switch (edge.callee) {
                .imported => |callee| _ = try checked_artifact.importedTemplateDemands(dispatch, callee),
                .local => {},
            };
            _ = try graph.component_slots[next].getOrCompute(self.io, Allocator.Error, ComponentRequest{ .analyses = self, .dispatch = dispatch, .graph = graph, .component = next }, ComponentRequest.compute);
        }
        return graph.component_slots[component].peek().?;
    }

    const ComponentRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,
        graph: *const TemplateDemandGraph,
        component: u32,

        fn compute(request: ComponentRequest) Allocator.Error!*TemplateDemandComponent {
            return checked_artifact.computeTemplateDemandComponent(request.analyses.allocator, request.dispatch, request.graph, request.component);
        }
    };

    const DispatchRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,

        fn demandGraph(request: DispatchRequest) Allocator.Error!*TemplateDemandGraph {
            return checked_artifact.computeTemplateDemandGraph(request.analyses.allocator, request.dispatch);
        }

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
