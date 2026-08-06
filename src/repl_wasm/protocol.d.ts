export type JsonValue = null | boolean | number | string | JsonValue[] | { [key: string]: JsonValue };
export type DefinitionKind = "value" | "annotation" | "type" | "import" | "file_import";
export type DiagnosticCode =
  | "incomplete_input"
  | "parse_error"
  | "compile_error"
  | "unsupported_file_import"
  | "type_error"
  | "expected_expression";

export interface Region {
  /** Zero-based UTF-8 byte offset. */
  start: number;
  /** Zero-based exclusive UTF-8 byte offset. */
  end: number;
}

export interface Diagnostic {
  code: DiagnosticCode;
  severity: "error";
  message: string;
  region: Region | null;
}

export interface RuntimeEvent {
  kind: "dbg" | "expect_failed" | "crashed";
  message: string;
}

export interface SnippetResult {
  source: string;
  kind: "expression" | "definition" | null;
  definition_kind: DefinitionKind | null;
  name: string | null;
  status: "ok" | "diagnostic" | "crashed";
  /** True only when this snippet changed stored definitions. */
  committed: boolean;
  revision: number;
  value: string | null;
  type: string | null;
  crash: { message: string } | null;
  diagnostics: Diagnostic[];
  events: RuntimeEvent[];
}

export interface EvalResult {
  snippets: SnippetResult[];
  completed: boolean;
  stop_reason: "diagnostic" | "crash" | null;
  committed_count: number;
  revision: number;
}

export interface AnalyzeResult {
  status: "complete" | "incomplete" | "invalid";
  kind: "expression" | "definition" | null;
  definition_kind: DefinitionKind | null;
  name: string | null;
  diagnostics: Diagnostic[];
  revision: number;
}

export interface InspectResult {
  status: "ok" | "diagnostic";
  source: string;
  type: string | null;
  diagnostics: Diagnostic[];
  revision: number;
}

export interface CompletionItem {
  label: string;
  insert_text: string;
  kind: DefinitionKind;
  detail: string | null;
}

export interface CompletionResult {
  items: CompletionItem[];
  is_incomplete: false;
  details_available: boolean;
  prefix: string;
  replacement: Region;
  cursor: number;
  offset_unit: "utf8_bytes";
  revision: number;
}

export interface StateDefinition {
  name: string;
  source: string;
  kind: DefinitionKind;
}

export interface VirtualModule {
  name: string;
  source: string;
}

export interface StateResult {
  revision: number;
  definitions: StateDefinition[];
  definition_source: string;
  modules: VirtualModule[];
  has_pending_annotation: boolean;
}

export interface SetModulesResult {
  module_count: number;
  module_names: string[];
  cleared_definition_count: number;
  revision: number;
}

export interface ClearResult {
  changed: boolean;
  removed_definition_count: number;
  revision: number;
}

export interface CapabilitiesResult {
  session_model: "one_session_per_wasm_instance";
  protocol_version: 1;
  operations: string[];
  text_encoding: "utf8";
  offset_unit: "utf8_bytes";
  revision_scope: "wasm_instance";
  revision_bits: 32;
  completion_scope: "session_definitions";
  features: Record<string, boolean | string>;
}

interface RequestBase {
  protocol: 1;
  id: JsonValue;
}

export type ReplRequest =
  | (RequestBase & { op: "capabilities" })
  | (RequestBase & { op: "eval" | "analyze" | "inspect"; params: { source: string } })
  | (RequestBase & { op: "complete"; params: { source: string; cursor: number } })
  | (RequestBase & { op: "get_state" | "clear" })
  | (RequestBase & { op: "set_modules"; params: { modules: VirtualModule[] } });

export type ReplResult =
  | CapabilitiesResult
  | EvalResult
  | AnalyzeResult
  | InspectResult
  | CompletionResult
  | StateResult
  | SetModulesResult
  | ClearResult;

export type ReplResponse =
  | { protocol: 1; id: JsonValue; ok: true; result: ReplResult }
  | { protocol: 1; id: JsonValue; ok: false; error: { code: string; message: string } };
