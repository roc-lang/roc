pub const Report = @import("report.zig").Report;
pub const Document = @import("document.zig").Document;
pub const DocumentElement = @import("document.zig").DocumentElement;
pub const Annotation = @import("document.zig").Annotation;
pub const SourceCodeDisplayRegion = @import("document.zig").SourceCodeDisplayRegion;
pub const UnderlineRegion = @import("document.zig").UnderlineRegion;
pub const ColorPalette = @import("style.zig").ColorPalette;
pub const ColorUtils = @import("style.zig").ColorUtils;
pub const ColorPreference = @import("config.zig").ColorPreference;
pub const ReportingConfig = @import("config.zig").ReportingConfig;
pub const Severity = @import("severity.zig").Severity;
pub const RenderTarget = @import("renderer.zig").RenderTarget;
pub const RenderTargetPreference = @import("config.zig").RenderTargetPreference;

pub const source_region = @import("source_region.zig");

pub const renderReport = @import("renderer.zig").renderReport;
pub const renderReportToTerminal = @import("renderer.zig").renderReportToTerminal;
pub const renderReportToMarkdown = @import("renderer.zig").renderReportToMarkdown;
pub const renderReportToHtml = @import("renderer.zig").renderReportToHtml;
pub const renderReportToLsp = @import("renderer.zig").renderReportToLsp;
pub const renderDocument = @import("renderer.zig").renderDocument;
pub const renderDocumentToTerminal = @import("renderer.zig").renderDocumentToTerminal;
pub const renderDocumentToMarkdown = @import("renderer.zig").renderDocumentToMarkdown;
pub const renderDocumentToHtml = @import("renderer.zig").renderDocumentToHtml;
pub const renderDocumentToLsp = @import("renderer.zig").renderDocumentToLsp;

// Configuration utilities
pub const validateUtf8 = @import("config.zig").validateUtf8;
pub const truncateUtf8 = @import("config.zig").truncateUtf8;
pub const formatUtf8 = @import("config.zig").formatUtf8;
pub const formatUtf8Bounded = @import("config.zig").formatUtf8Bounded;
