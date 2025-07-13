# Reporting

Error reporting and diagnostic formatting system for presenting Errors, Warnings, and Information to developers. Provides support for rendering to terminal (TTY), markdown, HTML, and Language Server Protocol (LSP) formats.

- [src/reporting/report.zig](./report.zig): Core report generation and management for creating structured diagnostic messages with source code context.
- [src/reporting/renderer.zig](./renderer.zig): Rendering system for formatting reports into human-readable output with syntax highlighting and visual formatting.
- [src/reporting/document.zig](./document.zig): Document structure and layout utilities for organizing diagnostic content and managing multi-line error displays.
- [src/reporting/source_region.zig](./source_region.zig) and [src/reporting/source_region/](./source_region/): Source code region handling for precise error location tracking and code snippet extraction.
- [src/reporting/severity.zig](./severity.zig): Severity level definitions and classification for errors, warnings, notes, and informational messages.
- [src/reporting/style.zig](./style.zig): Styling and color formatting for terminal output including ANSI color codes and formatting options.
- [src/reporting/config.zig](./config.zig): Configuration settings for report formatting including output preferences and verbosity levels.
- [src/reporting/test.zig](./test.zig): Unit tests for the reporting system ensuring accurate diagnostic formatting and output quality.
