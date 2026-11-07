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
const ExportRows = checked_artifact.ExportRows;
const ProvidedExportTable = checked_artifact.ProvidedExportTable;
const PlatformTypeInputs = checked_artifact.PlatformTypeInputs;
const PlatformRequirementSolutionTable = checked_artifact.PlatformRequirementSolutionTable;
const HostedProcTable = checked_artifact.HostedProcTable;
const HostedBindingTable = checked_artifact.HostedBindingTable;
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
const TemplateDemandNode = checked_artifact.TemplateDemandNode;
const TemplateDemandComponent = checked_artifact.TemplateDemandComponent;
const SchemeVarDemand = checked_artifact.SchemeVarDemand;
const LiteralGraph = checked_artifact.LiteralGraph;
const LiteralNodeEdges = checked_artifact.LiteralNodeEdges;
const LiteralPoolHolders = checked_artifact.LiteralPoolHolders;
const LiteralComponentOrder = checked_artifact.LiteralComponentOrder;
const LiteralComponent = checked_artifact.LiteralComponent;
const LiteralArgumentDemand = checked_artifact.LiteralArgumentDemand;
const UnitLiteralConversions = checked_artifact.UnitLiteralConversions;
const RootRequestTable = checked_artifact.RootRequestTable;
const CheckedTables = checked_artifact.CheckedTables;
const ResolvedValueRefTable = checked_artifact.ResolvedValueRefTable;
const PlatformPairing = checked_artifact.PlatformPairing;
const UnitId = @import("body_analyses.zig").UnitId;
const EvidenceTraversal = checked_artifact.EvidenceTraversal;
const ModuleEnv = @import("can").ModuleEnv;

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

const UnitConversionSlot = collections.OnceSlot(*UnitLiteralConversions, .module_unit_literal_conversions);

/// One slot per top-level unit of a module: the literal conversion roots of
/// the unit's literals.
const UnitConversionSlots = struct {
    slots: []UnitConversionSlot,

    fn create(allocator: Allocator, count: usize) Allocator.Error!*UnitConversionSlots {
        const result = try allocator.create(UnitConversionSlots);
        errdefer allocator.destroy(result);
        const slots = try allocator.alloc(UnitConversionSlot, count);
        @memset(slots, .empty);
        result.* = .{ .slots = slots };
        return result;
    }

    fn destroy(self: *UnitConversionSlots, allocator: Allocator) void {
        for (self.slots) |*slot| if (slot.peek()) |result| result.destroy(allocator);
        allocator.free(self.slots);
        allocator.destroy(self);
    }
};

/// The modules a module's dispatch resolution searches.
const DispatchImports = struct {
    views: []const ImportedModuleView,
};

const DispatchImportsRequest = struct {
    analyses: *ModuleAnalyses,
    module: checked_artifact.ModuleId,
    direct_imports: []const checked_artifact.CheckedModuleArtifactKey,
    store: *const CheckedTypeStore,

    fn compute(self: DispatchImportsRequest) Allocator.Error!*DispatchImports {
        const allocator = self.analyses.allocator;
        const views = try checked_artifact.computeDispatchImports(allocator, self.module, self.direct_imports, self.store.sealed_origins.items, self.analyses.resolver);
        errdefer allocator.free(views);
        const result = try allocator.create(DispatchImports);
        result.* = .{ .views = views };
        return result;
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
    /// The platform/app pairing whose analyses these are, which classifies
    /// its own value references (`PlatformPairing.valueRefs`).
    pairing: ?*PlatformPairing = null,
    checked_tables: collections.OnceSlot(*CheckedTables, .module_checked_tables) = .empty,
    export_rows: collections.OnceSlot(*ExportRows, .module_export_rows) = .empty,
    provided_exports: collections.OnceSlot(*ProvidedExportTable, .module_provided_exports) = .empty,
    platform_type_inputs: collections.OnceSlot(*PlatformTypeInputs, .module_platform_type_inputs) = .empty,
    platform_requirement_solutions: collections.OnceSlot(*PlatformRequirementSolutionTable, .module_platform_requirement_solutions) = .empty,
    hosted_procs: collections.OnceSlot(*HostedProcTable, .module_hosted_procs) = .empty,
    hosted_bindings: collections.OnceSlot(*HostedBindingTable, .module_hosted_bindings) = .empty,
    export_closures: collections.OnceSlot(*ExportClosures, .module_export_closures) = .empty,
    public_api_dependencies: collections.OnceSlot(*PublicApiDependencies, .module_public_api_dependencies) = .empty,
    lowering_visibility: collections.OnceSlot(*LoweringVisibility, .module_lowering_visibility) = .empty,
    interface_capabilities: collections.OnceSlot(*ModuleInterfaceCapabilities, .module_interface_capabilities) = .empty,
    /// Resolves the modules the module's analyses read other than its own:
    /// the importing compilation's resolver, or one over publication's
    /// import views (`owned_import_views`).
    resolver: ?checked_artifact.ModuleResolver = null,
    /// A copy of the import views publication resolved against, which
    /// `resolver` resolves through for as long as the module lives.
    owned_import_views: ?*checked_artifact.CheckedImportViews = null,
    /// The modules dispatch resolution searches and resolves types in
    /// (`checked_artifact.computeDispatchImports`).
    dispatch_imports: collections.OnceSlot(*DispatchImports, .module_dispatch_imports) = .empty,
    dispatch_slots: collections.OnceSlot(*DispatchSlots, .module_dispatch_slots) = .empty,
    dispatch_index: collections.OnceSlot(*DispatchIndex, .module_dispatch_index) = .empty,
    module_dispatch: collections.OnceSlot(*ModuleDispatch, .module_dispatch) = .empty,
    platform_root_evidence: collections.OnceSlot(*PlatformRequirementRootEvidence, .module_platform_root_evidence) = .empty,
    owner_slots: collections.OnceSlot(*OwnerSlots, .module_owner_slots) = .empty,
    nested_proc_sites: collections.OnceSlot(*ModuleNestedProcSites, .module_nested_proc_sites) = .empty,
    exhaustiveness_sites: collections.OnceSlot(*CheckedExhaustivenessSiteTable, .module_exhaustiveness_sites) = .empty,
    template_demand_graph: collections.OnceSlot(*TemplateDemandGraph, .module_template_demand_graph) = .empty,
    literal_graph: collections.OnceSlot(*LiteralGraph, .module_literal_graph) = .empty,
    unit_conversion_slots: collections.OnceSlot(*UnitConversionSlots, .module_unit_conversion_slots) = .empty,
    root_requests: collections.OnceSlot(*RootRequestTable, .module_root_requests) = .empty,
    /// The root requests the compilation reading the module makes of it by
    /// name, beyond its checked candidates. They select among the module's
    /// candidates when its root requests are read; nothing records them
    /// with the module.
    explicit_roots: []const checked_artifact.ExplicitRootRequestInput = &.{},
    /// The traversals no evidence pass of the module has borrowed
    /// (`borrowEvidenceTraversal`). Its capacity covers every traversal the
    /// module has created, so a return never allocates.
    idle_evidence_traversals: std.ArrayListUnmanaged(*EvidenceTraversal) = .empty,
    evidence_traversal_count: usize = 0,
    /// Guards `idle_evidence_traversals` and `evidence_traversal_count`; held
    /// around a borrow's or a return's list update only.
    evidence_traversal_mutex: std.Io.Mutex = .init,
    /// Set once the module is published (`CheckedModuleArtifact.openToAnalyses`):
    /// no slot of the module runs before it.
    published: std.atomic.Value(bool) = .init(false),

    pub fn create(allocator: Allocator, io: std.Io, platform: ?*const CheckedModuleArtifact) Allocator.Error!*ModuleAnalyses {
        const analyses = try allocator.create(ModuleAnalyses);
        analyses.* = .{ .allocator = allocator, .io = io, .platform = platform };
        return analyses;
    }

    /// Give the analyses the explicit root requests of the compilation that
    /// reads the module. They must be given before any root request is read.
    pub fn attachExplicitRoots(self: *ModuleAnalyses, roots: []const checked_artifact.ExplicitRootRequestInput) Allocator.Error!void {
        std.debug.assert(self.explicit_roots.len == 0 and self.root_requests.peek() == null);
        if (roots.len == 0) return;
        self.explicit_roots = try self.allocator.dupe(checked_artifact.ExplicitRootRequestInput, roots);
    }

    /// Give the analyses the resolver they find other modules through. It
    /// must resolve every module of the build for as long as the module lives.
    pub fn attachResolver(self: *ModuleAnalyses, resolver: checked_artifact.ModuleResolver) void {
        std.debug.assert(self.resolver == null);
        self.resolver = resolver;
    }

    /// Give the analyses a resolver over a copy of the views publication
    /// resolved against: its direct imports and the modules available to it.
    pub fn attachImportViews(
        self: *ModuleAnalyses,
        imports: []const checked_artifact.PublishImportArtifact,
        available: []const ImportedModuleView,
    ) Allocator.Error!void {
        const owned = try self.allocator.create(checked_artifact.CheckedImportViews);
        errdefer self.allocator.destroy(owned);
        const direct = try self.allocator.dupe(checked_artifact.PublishImportArtifact, imports);
        errdefer self.allocator.free(direct);
        owned.* = .{ .current_owner = .{}, .direct = direct, .available = try self.allocator.dupe(ImportedModuleView, available) };
        self.owned_import_views = owned;
        self.attachResolver(owned.resolver());
    }

    /// The modules dispatch resolution searches and resolves types in,
    /// computed on first demand from the module's direct imports and its
    /// types' name owners, transitively.
    pub fn dispatchImports(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error![]const ImportedModuleView {
        const source = dispatch.source;
        return self.reachedImports(source.key.digest(), source.direct_imports, source.types.store);
    }

    /// The modules the module `module`, which imports `direct_imports`
    /// directly and whose types `store` holds, reaches through its direct
    /// imports and its types' name owners, transitively: its dispatch imports.
    pub fn reachedImports(
        self: *ModuleAnalyses,
        module: checked_artifact.ModuleId,
        direct_imports: []const checked_artifact.CheckedModuleArtifactKey,
        store: *const CheckedTypeStore,
    ) Allocator.Error![]const ImportedModuleView {
        self.assertPublished();
        const result = try self.dispatch_imports.getOrCompute(self.io, Allocator.Error, DispatchImportsRequest{
            .analyses = self,
            .module = module,
            .direct_imports = direct_imports,
            .store = store,
        }, DispatchImportsRequest.compute);
        return result.views;
    }

    /// Record that the module is published: its type store and name store
    /// are, and its interface capabilities have their source.
    pub fn markPublished(self: *ModuleAnalyses) void {
        self.published.store(true, .release);
    }

    fn assertPublished(self: *const ModuleAnalyses) void {
        if (std.debug.runtime_safety and !self.published.load(.acquire)) {
            checked_artifact.checkedArtifactInvariant("a slot of a module was claimed before the module was published", .{});
        }
    }

    pub fn destroy(self: *ModuleAnalyses) void {
        const allocator = self.allocator;
        if (self.checked_tables.peek()) |tables| tables.destroy(allocator);
        if (self.dispatch_slots.peek()) |slots| slots.destroy(allocator);
        if (self.dispatch_index.peek()) |index| index.destroy();
        if (self.module_dispatch.peek()) |result| result.destroy(allocator);
        if (self.platform_root_evidence.peek()) |result| result.destroy(allocator);
        if (self.owner_slots.peek()) |slots| slots.destroy(allocator);
        if (self.nested_proc_sites.peek()) |result| result.destroy(allocator);
        if (self.exhaustiveness_sites.peek()) |result| result.destroy(allocator);
        if (self.template_demand_graph.peek()) |graph| graph.destroy(allocator);
        if (self.literal_graph.peek()) |graph| graph.destroy(allocator);
        if (self.unit_conversion_slots.peek()) |slots| slots.destroy(allocator);
        if (self.root_requests.peek()) |requests| {
            requests.deinit(allocator);
            allocator.destroy(requests);
        }
        allocator.free(self.explicit_roots);
        if (self.dispatch_imports.peek()) |imports| {
            allocator.free(imports.views);
            allocator.destroy(imports);
        }
        if (self.owned_import_views) |owned| {
            allocator.free(owned.direct);
            allocator.free(owned.available);
            allocator.destroy(owned);
        }
        if (self.export_closures.peek()) |closures| closures.destroy(allocator);
        if (self.platform_type_inputs.peek()) |inputs| {
            inputs.deinit(allocator);
            allocator.destroy(inputs);
        }
        if (self.platform_requirement_solutions.peek()) |solutions| {
            solutions.deinit(allocator);
            allocator.destroy(solutions);
        }
        if (self.hosted_procs.peek()) |table| {
            table.deinit(allocator);
            allocator.destroy(table);
        }
        if (self.hosted_bindings.peek()) |table| {
            table.deinit(allocator);
            allocator.destroy(table);
        }
        if (self.provided_exports.peek()) |table| {
            allocator.free(table.exports);
            allocator.destroy(table);
        }
        if (self.export_rows.peek()) |rows| {
            rows.deinit(allocator);
            allocator.destroy(rows);
        }
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
        std.debug.assert(self.idle_evidence_traversals.items.len == self.evidence_traversal_count);
        for (self.idle_evidence_traversals.items) |traversal| traversal.destroy(allocator);
        self.idle_evidence_traversals.deinit(allocator);
        allocator.destroy(self);
    }

    /// A traversal over the module's solver store (`env`'s) for one evidence
    /// pass to use until it returns it (`returnEvidenceTraversal`): an idle
    /// one, or a new one when every traversal the module has is borrowed.
    pub fn borrowEvidenceTraversal(self: *ModuleAnalyses, env: *const ModuleEnv) Allocator.Error!*EvidenceTraversal {
        {
            self.evidence_traversal_mutex.lockUncancelable(self.io);
            defer self.evidence_traversal_mutex.unlock(self.io);
            if (self.idle_evidence_traversals.pop()) |traversal| return traversal;
        }
        const traversal = try EvidenceTraversal.create(self.allocator, env);
        errdefer traversal.destroy(self.allocator);
        self.evidence_traversal_mutex.lockUncancelable(self.io);
        defer self.evidence_traversal_mutex.unlock(self.io);
        try self.idle_evidence_traversals.ensureTotalCapacity(self.allocator, self.evidence_traversal_count + 1);
        self.evidence_traversal_count += 1;
        return traversal;
    }

    /// Give back a traversal `borrowEvidenceTraversal` lent.
    pub fn returnEvidenceTraversal(self: *ModuleAnalyses, traversal: *EvidenceTraversal) void {
        self.evidence_traversal_mutex.lockUncancelable(self.io);
        defer self.evidence_traversal_mutex.unlock(self.io);
        self.idle_evidence_traversals.appendAssumeCapacity(traversal);
    }

    /// The export rows of `owner`, this module. A platform/app pairing's are
    /// its platform's.
    pub fn exportRows(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const ExportRows {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).exportRows(checked_artifact.importedView(platform));
        if (self.export_rows.peek()) |rows| return rows;
        // The rows read the module's checked tables, which read other
        // modules' export rows: they are demanded before the rows are
        // claimed.
        _ = try self.checkedTables(owner);
        return self.export_rows.getOrCompute(self.io, Allocator.Error, ViewRequest{ .analyses = self, .owner = owner }, ViewRequest.exportRows);
    }

    /// The provided exports of `owner`, this module. A platform/app pairing's
    /// are its pairing's own slot, read at their types under the pairing.
    pub fn providedExports(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const ProvidedExportTable {
        self.assertPublished();
        if (self.platform != null) checked_artifact.checkedArtifactInvariant("a pairing's provided exports were read from its module analyses", .{});
        return self.provided_exports.getOrCompute(self.io, Allocator.Error, ViewRequest{ .analyses = self, .owner = owner }, ViewRequest.providedExports);
    }

    /// The requirement identity variables of `owner`, this module. A
    /// platform/app pairing's are its platform's.
    pub fn platformTypeInputs(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const PlatformTypeInputs {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).platformTypeInputs(checked_artifact.importedView(platform));
        return self.platform_type_inputs.getOrCompute(self.io, Allocator.Error, ViewRequest{ .analyses = self, .owner = owner }, ViewRequest.platformTypeInputs);
    }

    /// The solutions of its platform's requirements `owner`, this module,
    /// recorded. A platform/app pairing's are its platform's.
    pub fn platformRequirementSolutions(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const PlatformRequirementSolutionTable {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).platformRequirementSolutions(checked_artifact.importedView(platform));
        return self.platform_requirement_solutions.getOrCompute(self.io, Allocator.Error, ViewRequest{ .analyses = self, .owner = owner }, ViewRequest.platformRequirementSolutions);
    }

    /// The hosted procedures of `owner`, this module. A platform/app
    /// pairing's are its platform's.
    pub fn hostedProcs(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const HostedProcTable {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).hostedProcs(checked_artifact.importedView(platform));
        return self.hosted_procs.getOrCompute(self.io, Allocator.Error, ViewRequest{ .analyses = self, .owner = owner }, ViewRequest.hostedProcs);
    }

    /// The hosted bindings of `owner`, this module, a platform's hosted
    /// section. A platform/app pairing's are its platform's.
    pub fn hostedBindings(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const HostedBindingTable {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).hostedBindings(checked_artifact.importedView(platform));
        return self.hosted_bindings.getOrCompute(self.io, Allocator.Error, ViewRequest{ .analyses = self, .owner = owner }, ViewRequest.hostedBindings);
    }

    /// The checked tables of `owner`, this module. A platform/app pairing's
    /// are its platform's.
    pub fn checkedTables(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const CheckedTables {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).checkedTables(checked_artifact.importedView(platform));
        if (self.checked_tables.peek()) |tables| return tables;
        // A thread never waits on a slot of a kind it holds a claim of, so
        // what the tables read of other modules is demanded first, holding
        // no claim.
        try checked_artifact.demandCheckedTablesInputs(self.allocator, owner);
        return self.checked_tables.getOrCompute(self.io, Allocator.Error, ViewRequest{ .analyses = self, .owner = owner }, ViewRequest.checkedTables);
    }

    /// The resolved value references of `owner`, this module: its checked
    /// tables', or a platform/app pairing's own (`PlatformPairing.valueRefs`).
    pub fn valueRefs(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const ResolvedValueRefTable {
        if (self.pairing) |pairing| return pairing.valueRefs();
        return &(try self.checkedTables(owner)).resolved_value_refs;
    }

    const ViewRequest = struct {
        analyses: *ModuleAnalyses,
        owner: ImportedModuleView,

        fn checkedTables(request: ViewRequest) Allocator.Error!*CheckedTables {
            return checked_artifact.computeCheckedTables(request.analyses.allocator, request.owner);
        }

        fn hostedProcs(request: ViewRequest) Allocator.Error!*HostedProcTable {
            return checked_artifact.computeHostedProcs(request.analyses.allocator, request.owner);
        }

        fn hostedBindings(request: ViewRequest) Allocator.Error!*HostedBindingTable {
            return checked_artifact.computeHostedBindings(request.analyses.allocator, request.owner, request.analyses.resolver);
        }

        fn exportRows(request: ViewRequest) Allocator.Error!*ExportRows {
            return checked_artifact.computeExportRows(request.analyses.allocator, request.owner);
        }

        fn platformTypeInputs(request: ViewRequest) Allocator.Error!*PlatformTypeInputs {
            return checked_artifact.computePlatformTypeInputs(request.analyses.allocator, request.owner);
        }

        fn platformRequirementSolutions(request: ViewRequest) Allocator.Error!*PlatformRequirementSolutionTable {
            return checked_artifact.computePlatformRequirementSolutions(request.analyses.allocator, request.owner);
        }

        fn providedExports(request: ViewRequest) Allocator.Error!*ProvidedExportTable {
            return checked_artifact.computeModuleProvidedExports(request.analyses.allocator, request.owner);
        }
    };

    /// The template closure of every row of `owner`'s export rows.
    pub fn exportClosures(self: *ModuleAnalyses, owner: ImportedModuleView, resolver: ModuleResolver) Allocator.Error!*const ExportClosures {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).exportClosures(checked_artifact.importedView(platform), resolver);
        return self.export_closures.getOrCompute(self.io, Allocator.Error, Request{ .analyses = self, .owner = owner, .resolver = resolver }, Request.exportClosures);
    }

    /// The checked modules a consumer of `owner`'s public API reads.
    pub fn publicApiDependencies(self: *ModuleAnalyses, owner: ImportedModuleView, resolver: ModuleResolver) Allocator.Error!*const PublicApiDependencies {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).publicApiDependencies(checked_artifact.importedView(platform), resolver);
        return self.public_api_dependencies.getOrCompute(self.io, Allocator.Error, Request{ .analyses = self, .owner = owner, .resolver = resolver }, Request.publicApiDependencies);
    }

    /// The checked modules lowering `owner`'s roots reads.
    pub fn loweringVisibility(self: *ModuleAnalyses, owner: ImportedModuleView, resolver: ModuleResolver) Allocator.Error!*const LoweringVisibility {
        self.assertPublished();
        return self.lowering_visibility.getOrCompute(self.io, Allocator.Error, Request{ .analyses = self, .owner = owner, .resolver = resolver }, Request.loweringVisibility);
    }

    /// The box payload capabilities of the local nominal types of `store`,
    /// the store of this module's types.
    pub fn interfaceCapabilities(
        self: *ModuleAnalyses,
        store: *CheckedTypeStore,
        current_module: canonical.ModuleIdentityId,
    ) Allocator.Error!*const ModuleInterfaceCapabilities {
        self.assertPublished();
        return self.interface_capabilities.getOrCompute(self.io, Allocator.Error, CapabilityRequest{
            .analyses = self,
            .store = store,
            .current_module = current_module,
        }, CapabilityRequest.compute);
    }

    /// The interface capabilities when a consumer has demanded them.
    pub fn peekInterfaceCapabilities(self: *const ModuleAnalyses) ?*const ModuleInterfaceCapabilities {
        return self.interface_capabilities.peek();
    }

    /// The interface capabilities when a consumer has demanded them, for the
    /// creation that holds the module's type store's creation lock to grow
    /// (`CheckedTypeStore.finishCreation`).
    pub fn growableInterfaceCapabilities(self: *const ModuleAnalyses) ?*ModuleInterfaceCapabilities {
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
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).nestedProcSites(try platform.bodies(), owner);
        const slots = try self.ownerSlots(bodies);
        const slot = switch (owner) {
            .template => |template| blk: {
                const raw = template.template.rowIndex() orelse return &checked_artifact.empty_owner_nested_proc_sites;
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
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).moduleNestedProcSites(try platform.bodies());
        return self.nested_proc_sites.getOrCompute(self.io, Allocator.Error, BodiesRequest{ .analyses = self, .bodies = bodies }, BodiesRequest.moduleNestedProcSites);
    }

    /// The exhaustiveness sites procedure template `template`'s body
    /// reaches. A platform/app pairing reads its platform's.
    pub fn templateExhaustivenessSites(self: *ModuleAnalyses, bodies: CheckedBodies, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const TemplateExhaustivenessSites {
        self.assertPublished();
        if (self.platform) |platform| return platformAnalyses(platform).templateExhaustivenessSites(try platform.bodies(), template);
        const raw = template.rowIndex() orelse return &checked_artifact.empty_template_exhaustiveness_sites;
        const slots = try self.ownerSlots(bodies);
        if (raw >= slots.template_exhaustiveness.len) checked_artifact.checkedArtifactInvariant("exhaustiveness sites were demanded of a template outside the module's templates", .{});
        return slots.template_exhaustiveness[raw].getOrCompute(self.io, Allocator.Error, ExhaustivenessRequest{ .allocator = self.allocator, .bodies = bodies, .template = template }, ExhaustivenessRequest.compute);
    }

    /// Every exhaustiveness check of `owner` with the policy that decides
    /// it. A platform/app pairing decides its platform's checks by its own
    /// root requests.
    pub fn exhaustivenessSites(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const CheckedExhaustivenessSiteTable {
        self.assertPublished();
        if (self.exhaustiveness_sites.peek()) |done| return done;
        const platform_sites: ?*const CheckedExhaustivenessSiteTable = if (self.platform) |platform|
            try platformAnalyses(platform).exhaustivenessSites(checked_artifact.importedView(platform))
        else
            null;
        _ = try self.rootRequests(owner);
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
        if (self.dispatch_slots.peek() == null) try checked_artifact.demandDispatchCapabilities(dispatch, &.{});
        return self.dispatch_slots.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.createSlots);
    }

    /// The schema of procedure template `template`.
    pub fn templateSchema(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const SchemeAnalysis {
        self.assertPublished();
        const all = try self.dispatchSlots(dispatch);
        const raw = template.rowIndex() orelse return &checked_artifact.empty_scheme_analysis;
        if (raw >= all.template_schemas.len) return &checked_artifact.empty_scheme_analysis;
        return all.template_schemas[raw].getOrCompute(self.io, Allocator.Error, TemplateRequest{ .analyses = self, .dispatch = dispatch, .template = template }, TemplateRequest.schema);
    }

    /// The schema of generalized local scope `scope`.
    pub fn scopeSchema(self: *ModuleAnalyses, dispatch: CheckedDispatch, scope: DispatchScopeId) Allocator.Error!*const SchemeAnalysis {
        self.assertPublished();
        const all = try self.dispatchSlots(dispatch);
        return all.scope_schemas[@backingInt(scope)].getOrCompute(self.io, Allocator.Error, ScopeRequest{ .analyses = self, .dispatch = dispatch, .scope = scope }, ScopeRequest.schema);
    }

    /// The dispatch resolution of procedure template `template`.
    pub fn templateDispatch(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const TemplateDispatch {
        self.assertPublished();
        const all = try self.dispatchSlots(dispatch);
        const raw = template.rowIndex() orelse return &checked_artifact.empty_template_dispatch;
        if (raw >= all.template_dispatch.len) return &checked_artifact.empty_template_dispatch;
        return all.template_dispatch[raw].getOrCompute(self.io, Allocator.Error, TemplateRequest{ .analyses = self, .dispatch = dispatch, .template = template }, TemplateRequest.dispatchResolution);
    }

    /// How procedure template `template`'s plans relate to its
    /// specialization.
    pub fn templateDispatchRelations(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error!*const TemplateDispatchRelations {
        self.assertPublished();
        const all = try self.dispatchSlots(dispatch);
        const raw = template.rowIndex() orelse return &checked_artifact.empty_template_relations;
        if (raw >= all.template_relations.len) return &checked_artifact.empty_template_relations;
        return all.template_relations[raw].getOrCompute(self.io, Allocator.Error, TemplateRequest{ .analyses = self, .dispatch = dispatch, .template = template }, TemplateRequest.relations);
    }

    /// The module-wide indexes dispatch resolution reads.
    pub fn dispatchIndex(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*const DispatchIndex {
        self.assertPublished();
        if (self.dispatch_index.peek() == null) try checked_artifact.demandDispatchCapabilities(dispatch, &.{});
        return self.dispatch_index.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.index);
    }

    /// The module's dispatch resolution of what no template resolves.
    pub fn moduleDispatch(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*const ModuleDispatch {
        self.assertPublished();
        if (self.module_dispatch.peek() == null) try checked_artifact.demandDispatchCapabilities(dispatch, &.{});
        return self.module_dispatch.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.moduleResolution);
    }

    /// An app's root evidence for each of its platform requirement
    /// solutions.
    pub fn platformRequirementRootEvidence(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*const PlatformRequirementRootEvidence {
        self.assertPublished();
        if (self.platform_root_evidence.peek() == null) try checked_artifact.demandDispatchCapabilities(dispatch, &.{});
        return self.platform_root_evidence.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.platformRootEvidence);
    }

    /// The resolution of plan `plan`: the first template in template order
    /// whose chain binds it, otherwise the module's.
    pub fn planResolution(self: *ModuleAnalyses, dispatch: CheckedDispatch, plan: static_dispatch.StaticDispatchPlanId) Allocator.Error!static_dispatch.PlanResolution {
        self.assertPublished();
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
        self.assertPublished();
        const index = try self.dispatchIndex(dispatch);
        for (index.iteratorSites(plan)) |reach| {
            const template = reach.template orelse continue;
            if ((try self.templateDispatch(dispatch, @fromBackingInt(template))).iteratorPlan(plan)) |resolution| return resolution;
        }
        return (try self.moduleDispatch(dispatch)).iteratorPlan(plan) orelse
            checked_artifact.checkedArtifactInvariant("an iterator plan had no resolution", .{});
    }

    /// The template whose resolution resolves plan `plan` (`planResolution`),
    /// or null when the module's does.
    pub fn planResolver(self: *ModuleAnalyses, dispatch: CheckedDispatch, plan: static_dispatch.StaticDispatchPlanId) Allocator.Error!?u32 {
        self.assertPublished();
        const index = try self.dispatchIndex(dispatch);
        for (index.planSites(plan)) |reach| {
            const template = reach.template orelse continue;
            if ((try self.templateDispatch(dispatch, @fromBackingInt(template))).plan(plan) != null) return template;
        }
        return null;
    }

    /// The template whose resolution holds use site `site`'s evidence
    /// (`siteEvidence`), or null when no template's does.
    pub fn siteResolver(self: *ModuleAnalyses, dispatch: CheckedDispatch, site: u32) Allocator.Error!?u32 {
        self.assertPublished();
        const index = try self.dispatchIndex(dispatch);
        for (index.siteTemplates(site)) |reach| {
            if ((try self.templateDispatch(dispatch, @fromBackingInt(reach.template))).site(site) != null) return reach.template;
        }
        return null;
    }

    /// The evidence of use site `site`: the first template in template order
    /// that resolved it, otherwise the module's; null when no analysis
    /// recorded evidence for it. The module's resolution holds evidence only
    /// for the defaulted-field expressions' sites and for the sites a
    /// template's chain deferred, so a site of templates that deferred none
    /// of it has none there.
    pub fn siteEvidence(self: *ModuleAnalyses, dispatch: CheckedDispatch, site: u32) Allocator.Error!?checked_artifact.SiteEvidenceLookup {
        self.assertPublished();
        const index = try self.dispatchIndex(dispatch);
        const reaches = index.siteTemplates(site);
        var deferred = false;
        for (reaches) |reach| {
            const template: canonical.CheckedProcedureTemplateId = @fromBackingInt(reach.template);
            const resolved = try self.templateDispatch(dispatch, template);
            if (resolved.site(site)) |entry| return .{ .entry = entry.*, .owner = static_dispatch.EvidenceOwner.forTemplate(template) };
            if (resolved.defersSite(site)) deferred = true;
        }
        if (reaches.len != 0 and !deferred and !index.isDefaultSite(site)) return null;
        if ((try self.moduleDispatch(dispatch)).site(site)) |entry| return .{ .entry = entry.*, .owner = .module };
        return null;
    }

    /// The module's demand call graph: the slots of its templates' own
    /// demand vectors and edges and of their components.
    pub fn templateDemandGraph(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*TemplateDemandGraph {
        self.assertPublished();
        if (self.template_demand_graph.peek() == null) try checked_artifact.demandDispatchCapabilities(dispatch, &.{});
        return self.template_demand_graph.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.demandGraph);
    }

    /// Template `template`'s own demand vector, its edges and the local
    /// templates they call.
    fn templateDemandNode(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *TemplateDemandGraph, template: u32) Allocator.Error!*const TemplateDemandNode {
        return graph.node_slots[template].getOrCompute(self.io, Allocator.Error, DemandNodeRequest{ .analyses = self, .dispatch = dispatch, .template = template }, DemandNodeRequest.compute);
    }

    const DemandNodeRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,
        template: u32,

        fn compute(request: DemandNodeRequest) Allocator.Error!*TemplateDemandNode {
            return checked_artifact.computeTemplateDemandNode(request.analyses.allocator, request.dispatch, request.template);
        }
    };

    /// The demand vector of procedure template `template`.
    pub fn templateDemands(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: canonical.CheckedProcedureTemplateId) Allocator.Error![]const SchemeVarDemand {
        self.assertPublished();
        const raw = template.rowIndex() orelse return &.{};
        const graph = try self.templateDemandGraph(dispatch);
        const component = try self.templateDemandComponent(dispatch, graph, raw);
        const leader = graph.components.leaderOf(raw).?;
        return component.member(graph.components.info(leader).memberIndex(raw));
    }

    /// The leader of template `template`'s demand component. Before
    /// assigning the template, computes, holding no lock, the slot of every
    /// unassigned template it reaches, so the assignment reads only finished
    /// slots.
    fn templateDemandLeader(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *TemplateDemandGraph, template: u32) Allocator.Error!u32 {
        if (graph.components.leaderOf(template)) |leader| return leader;
        const allocator = self.allocator;
        var seen = std.AutoHashMapUnmanaged(u32, void).empty;
        defer seen.deinit(allocator);
        var pending: std.ArrayList(u32) = .empty;
        defer pending.deinit(allocator);
        try seen.put(allocator, template, {});
        try pending.append(allocator, template);
        while (pending.pop()) |next| {
            if (graph.components.leaderOf(next) != null) continue;
            for ((try self.templateDemandNode(dispatch, graph, next)).callees) |callee| {
                if ((try seen.getOrPut(allocator, callee)).found_existing) continue;
                try pending.append(allocator, callee);
            }
        }
        graph.components.mutex.lockUncancelable(self.io);
        defer graph.components.mutex.unlock(self.io);
        collections.HeldLocks.acquired();
        defer collections.HeldLocks.released();
        try graph.components.assign(allocator, template, graph);
        return graph.components.leaderOf(template).?;
    }

    /// The demand vectors of the component of template `template`. Before it
    /// claims a component, a consumer demands, holding no claim, every
    /// component it reaches through local edges (callees first) and every
    /// imported template those components call, so a component's
    /// computation only reads finished vectors and never waits on a slot of
    /// its own kind.
    fn templateDemandComponent(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *TemplateDemandGraph, template: u32) Allocator.Error!*const TemplateDemandComponent {
        const component = try self.templateDemandLeader(dispatch, graph, template);
        if (graph.component_slots[component].peek()) |done| return done;
        const allocator = self.allocator;
        var visited = std.AutoHashMapUnmanaged(u32, void).empty;
        defer visited.deinit(allocator);
        var order: std.ArrayList(u32) = .empty;
        defer order.deinit(allocator);
        const Frame = struct { component: u32, next_callee: u32 };
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(allocator);
        try visited.put(allocator, component, {});
        try frames.append(allocator, .{ .component = component, .next_callee = 0 });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            const callees = graph.components.info(frame.component).callees;
            if (frame.next_callee < callees.len) {
                const callee = callees[frame.next_callee];
                frame.next_callee += 1;
                if (graph.component_slots[callee].peek() != null) continue;
                if ((try visited.getOrPut(allocator, callee)).found_existing) continue;
                try frames.append(allocator, .{ .component = callee, .next_callee = 0 });
                continue;
            }
            try order.append(allocator, frame.component);
            _ = frames.pop();
        }
        for (order.items) |next| {
            for (graph.components.info(next).members) |member| for (graph.finishedNode(member).edges) |edge| switch (edge.callee) {
                .imported => |callee| _ = try checked_artifact.importedTemplateDemands(dispatch, callee),
                .local => {},
            };
            _ = try graph.component_slots[next].getOrCompute(self.io, Allocator.Error, ComponentRequest{ .analyses = self, .dispatch = dispatch, .graph = graph, .component = next }, ComponentRequest.compute);
        }
        return graph.component_slots[component].peek().?;
    }

    /// The module's literal call graph (`LiteralGraph`): what of it needs no
    /// dispatch resolution, and the slots of the rest.
    pub fn literalGraph(self: *ModuleAnalyses, dispatch: CheckedDispatch) Allocator.Error!*LiteralGraph {
        self.assertPublished();
        if (self.literal_graph.peek() == null) try checked_artifact.demandDispatchCapabilities(dispatch, &.{});
        return self.literal_graph.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.literalGraph);
    }

    /// Node `node`'s edges, its own open literals and the local schemes its
    /// edges instantiate.
    pub fn literalNode(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *LiteralGraph, node: u32) Allocator.Error!*const LiteralNodeEdges {
        self.assertPublished();
        return graph.node_slots[node].getOrCompute(self.io, Allocator.Error, LiteralNodeRequest{ .analyses = self, .dispatch = dispatch, .graph = graph, .node = node }, LiteralNodeRequest.compute);
    }

    const LiteralNodeRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,
        graph: *LiteralGraph,
        node: u32,

        fn compute(request: LiteralNodeRequest) Allocator.Error!*LiteralNodeEdges {
            return checked_artifact.computeLiteralNodeEdges(request.analyses.allocator, request.dispatch, request.graph, request.node);
        }

        fn pool(request: LiteralNodeRequest) Allocator.Error!*LiteralPoolHolders {
            return checked_artifact.computeLiteralPoolHolders(request.analyses.allocator, request.dispatch, request.graph, request.node);
        }

        fn modulePool(request: LiteralNodeRequest) Allocator.Error!*LiteralPoolHolders {
            return checked_artifact.computeLiteralModulePoolHolders(request.analyses.allocator, request.dispatch, request.graph);
        }

        fn order(request: LiteralNodeRequest) Allocator.Error!*LiteralComponentOrder {
            return checked_artifact.computeLiteralComponentOrder(request.analyses.allocator, request.dispatch, request.graph);
        }
    };

    /// The first node, in node order, whose body reaches evidence node `id`;
    /// null when no literal walk reaches it.
    pub fn literalEvidenceHolder(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *LiteralGraph, id: static_dispatch.EvidenceNodeId) Allocator.Error!?u32 {
        self.assertPublished();
        const holders = if (id.owner.template()) |template| blk: {
            const raw = template.rowIndex() orelse return null;
            break :blk try graph.pool_slots[raw].getOrCompute(self.io, Allocator.Error, LiteralNodeRequest{ .analyses = self, .dispatch = dispatch, .graph = graph, .node = raw }, LiteralNodeRequest.pool);
        } else try graph.module_pool_slot.getOrCompute(self.io, Allocator.Error, LiteralNodeRequest{ .analyses = self, .dispatch = dispatch, .graph = graph, .node = graph.defaultsNode() }, LiteralNodeRequest.modulePool);
        return holders.holder(id);
    }

    /// The node holding edge `edge`, or null when no body reaches it.
    pub fn literalEdgeHolder(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *LiteralGraph, edge: checked_artifact.LiteralEdge) Allocator.Error!?u32 {
        return switch (edge) {
            .node => |id| self.literalEvidenceHolder(dispatch, graph, id),
            .use => |expr| checked_artifact.literalUseHolder(dispatch, graph, expr),
            .literal => null,
        };
    }

    /// Every component of the literal graph, by leader, callees first.
    pub fn literalComponentOrder(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *LiteralGraph) Allocator.Error!*const LiteralComponentOrder {
        self.assertPublished();
        return graph.order_slot.getOrCompute(self.io, Allocator.Error, LiteralNodeRequest{ .analyses = self, .dispatch = dispatch, .graph = graph, .node = graph.defaultsNode() }, LiteralNodeRequest.order);
    }

    /// The leader of node `node`'s component. Before assigning the node,
    /// computes, holding no lock, the slot of every unassigned node it
    /// reaches, so the assignment reads only finished slots.
    fn literalLeader(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *LiteralGraph, node: u32) Allocator.Error!u32 {
        if (graph.leaderOf(node)) |leader| return leader;
        const allocator = self.allocator;
        var seen = std.AutoHashMapUnmanaged(u32, void).empty;
        defer seen.deinit(allocator);
        var pending: std.ArrayList(u32) = .empty;
        defer pending.deinit(allocator);
        try seen.put(allocator, node, {});
        try pending.append(allocator, node);
        while (pending.pop()) |next| {
            if (graph.leaderOf(next) != null) continue;
            for ((try self.literalNode(dispatch, graph, next)).callees) |callee| {
                if ((try seen.getOrPut(allocator, callee)).found_existing) continue;
                try pending.append(allocator, callee);
            }
        }
        graph.components.mutex.lockUncancelable(self.io);
        defer graph.components.mutex.unlock(self.io);
        collections.HeldLocks.acquired();
        defer collections.HeldLocks.released();
        try graph.components.assign(allocator, node, graph);
        return graph.leaderOf(node).?;
    }

    /// The literal requirements, edge arguments and literal argument roots
    /// of the component of node `node`. Before it claims a component, a
    /// consumer demands, holding no claim, every component it reaches
    /// through local edges (callees first) and the literal requirements of
    /// every imported scheme those components instantiate, so a component's
    /// computation only reads finished requirements and never waits on a
    /// slot of its own kind.
    pub fn literalComponent(self: *ModuleAnalyses, dispatch: CheckedDispatch, graph: *LiteralGraph, node: u32) Allocator.Error!*const LiteralComponent {
        self.assertPublished();
        const component = try self.literalLeader(dispatch, graph, node);
        if (graph.component_slots[component].peek()) |done| return done;
        const allocator = self.allocator;
        var visited = std.AutoHashMapUnmanaged(u32, void).empty;
        defer visited.deinit(allocator);
        var order: std.ArrayList(u32) = .empty;
        defer order.deinit(allocator);
        const Frame = struct { component: u32, next_callee: u32 };
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(allocator);
        try visited.put(allocator, component, {});
        try frames.append(allocator, .{ .component = component, .next_callee = 0 });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            const callees = graph.componentInfo(frame.component).callees;
            if (frame.next_callee < callees.len) {
                const callee = callees[frame.next_callee];
                frame.next_callee += 1;
                if (graph.component_slots[callee].peek() != null) continue;
                if ((try visited.getOrPut(allocator, callee)).found_existing) continue;
                try frames.append(allocator, .{ .component = callee, .next_callee = 0 });
                continue;
            }
            try order.append(allocator, frame.component);
            _ = frames.pop();
        }
        const owner = dispatch.source.key.digest();
        for (order.items) |next| {
            for (graph.componentInfo(next).members) |member| for (graph.finishedNode(member).edges) |edge| switch (edge.callee) {
                .template => |template| if (!template.artifact.eql(owner)) {
                    if (try dispatch.importedView(template.artifact)) |view| _ = try (try view.dispatch()).templateLiteralRequirements(template.template);
                },
                .scope => {},
            };
            _ = try graph.component_slots[next].getOrCompute(self.io, Allocator.Error, LiteralComponentRequest{ .analyses = self, .dispatch = dispatch, .graph = graph, .component = next }, LiteralComponentRequest.compute);
        }
        return graph.component_slots[component].peek().?;
    }

    const LiteralComponentRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,
        graph: *const LiteralGraph,
        component: u32,

        fn compute(request: LiteralComponentRequest) Allocator.Error!*LiteralComponent {
            return checked_artifact.computeLiteralComponent(request.analyses.allocator, request.dispatch, request.graph, request.component);
        }
    };

    /// The literal argument roots procedure template `template`'s edges read,
    /// ascending. Demands, holding no claim, the component of every edge the
    /// template reaches before claiming.
    pub fn templateLiteralArgumentDemand(self: *ModuleAnalyses, dispatch: CheckedDispatch, template: u32) Allocator.Error![]const checked_artifact.ComptimeRootKey {
        self.assertPublished();
        const graph = try self.literalGraph(dispatch);
        if (graph.demand_slots[template].peek()) |done| return done.roots;
        for ((try self.literalNode(dispatch, graph, template)).reached) |reached| {
            _ = try self.literalComponent(dispatch, graph, reached.holder);
        }
        return (try graph.demand_slots[template].getOrCompute(self.io, Allocator.Error, LiteralDemandRequest{ .allocator = self.allocator, .graph = graph, .template = template }, LiteralDemandRequest.compute)).roots;
    }

    const LiteralDemandRequest = struct {
        allocator: Allocator,
        graph: *const LiteralGraph,
        template: u32,

        fn compute(request: LiteralDemandRequest) Allocator.Error!*LiteralArgumentDemand {
            return checked_artifact.computeLiteralArgumentDemand(request.allocator, request.graph, request.template);
        }
    };

    /// The literal conversion roots of unit `unit`'s literals.
    pub fn unitLiteralConversions(self: *ModuleAnalyses, dispatch: CheckedDispatch, unit: UnitId) Allocator.Error!*const UnitLiteralConversions {
        self.assertPublished();
        const slots = try self.unit_conversion_slots.getOrCompute(self.io, Allocator.Error, DispatchRequest{ .analyses = self, .dispatch = dispatch }, DispatchRequest.unitConversionSlots);
        return slots.slots[@backingInt(unit)].getOrCompute(self.io, Allocator.Error, UnitConversionRequest{ .analyses = self, .dispatch = dispatch, .unit = unit }, UnitConversionRequest.compute);
    }

    const UnitConversionRequest = struct {
        analyses: *ModuleAnalyses,
        dispatch: CheckedDispatch,
        unit: UnitId,

        fn compute(request: UnitConversionRequest) Allocator.Error!*UnitLiteralConversions {
            return checked_artifact.computeUnitLiteralConversions(request.analyses.allocator, request.dispatch, request.unit);
        }
    };

    /// The root requests of `owner`, this module. A platform/app pairing's
    /// are its own, built from its platform's.
    pub fn rootRequests(self: *ModuleAnalyses, owner: ImportedModuleView) Allocator.Error!*const RootRequestTable {
        self.assertPublished();
        const platform_requests: ?*const RootRequestTable = if (self.platform) |platform|
            try platformAnalyses(platform).rootRequests(checked_artifact.importedView(platform))
        else
            null;
        return self.root_requests.getOrCompute(self.io, Allocator.Error, RootRequestsRequest{ .analyses = self, .owner = owner, .platform_requests = platform_requests }, RootRequestsRequest.compute);
    }

    const RootRequestsRequest = struct {
        analyses: *ModuleAnalyses,
        owner: ImportedModuleView,
        platform_requests: ?*const RootRequestTable,

        fn compute(request: RootRequestsRequest) Allocator.Error!*RootRequestTable {
            return checked_artifact.computeRootRequests(request.analyses.allocator, request.owner, request.platform_requests);
        }
    };

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

        fn literalGraph(request: DispatchRequest) Allocator.Error!*LiteralGraph {
            return checked_artifact.computeLiteralGraph(request.analyses.allocator, request.dispatch);
        }

        fn unitConversionSlots(request: DispatchRequest) Allocator.Error!*UnitConversionSlots {
            return UnitConversionSlots.create(request.analyses.allocator, request.dispatch.source.module_index.units.len);
        }

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
