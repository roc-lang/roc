# Reporting

Error reporting, diagnostics, and user feedback for the Roc compiler.

## Overview

The reporting module provides comprehensive error reporting and diagnostic information throughout the compilation process. It ensures that users receive clear, actionable feedback when things go wrong.

## Purpose

This module provides:
- **Error Reporting**: Structured error messages with source locations and context
- **Diagnostics**: Detailed information about compilation issues and warnings
- **Source Location**: Precise tracking of where errors occur in source code
- **User-Friendly Messages**: Clear explanations that help developers fix issues
- **Consistent Formatting**: Uniform error presentation across all compiler stages

The reporting module is used by parse, canonicalize, check, and other stages to provide consistent, helpful feedback to users when compilation fails or produces warnings.
