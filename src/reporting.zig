//! Reporting for the Roc compiler.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");

pub const Severity = @import("reporting/severity.zig").Severity;
pub const Document = @import("reporting/document.zig").Document;
pub const Annotation = @import("reporting/document.zig").Annotation;
pub const RenderTarget = @import("reporting/renderer.zig").RenderTarget;
pub const ColorPalette = @import("reporting/style.zig").ColorPalette;
pub const Report = @import("reporting/report.zig").Report;
pub const ReportingConfig = @import("reporting/config.zig").ReportingConfig;
pub const ColorPreference = @import("reporting/config.zig").ColorPreference;
pub const RenderTargetPreference = @import("reporting/config.zig").RenderTargetPreference;

pub const renderReport = @import("reporting/renderer.zig").renderReport;
pub const renderReportToTerminal = @import("reporting/renderer.zig").renderReportToTerminal;
pub const renderReportToMarkdown = @import("reporting/renderer.zig").renderReportToMarkdown;
pub const renderReportToHtml = @import("reporting/renderer.zig").renderReportToHtml;
pub const renderReportToLsp = @import("reporting/renderer.zig").renderReportToLsp;
pub const renderDocument = @import("reporting/renderer.zig").renderDocument;
pub const renderDocumentToTerminal = @import("reporting/renderer.zig").renderDocumentToTerminal;
pub const renderDocumentToMarkdown = @import("reporting/renderer.zig").renderDocumentToMarkdown;
pub const renderDocumentToHtml = @import("reporting/renderer.zig").renderDocumentToHtml;
pub const renderDocumentToLsp = @import("reporting/renderer.zig").renderDocumentToLsp;

// Configuration utilities
pub const validateUtf8 = @import("reporting/config.zig").validateUtf8;
pub const truncateUtf8 = @import("reporting/config.zig").truncateUtf8;
pub const formatUtf8 = @import("reporting/config.zig").formatUtf8;
pub const formatUtf8Bounded = @import("reporting/config.zig").formatUtf8Bounded;
