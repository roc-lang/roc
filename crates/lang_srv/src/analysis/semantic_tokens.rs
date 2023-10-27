use roc_region::all::{LineColumn, LineInfo, Loc};
use tower_lsp::lsp_types::SemanticToken;

use super::tokens::Token;

/// Encodes semantic tokens as described in the LSP specification.
/// See [the sample documentation](https://github.com/microsoft/vscode-extension-samples/blob/5ae1f7787122812dcc84e37427ca90af5ee09f14/semantic-tokens-sample/vscode.proposed.d.ts#L71-L128).
pub fn arrange_semantic_tokens(
    tokens: impl IntoIterator<Item = Loc<Token>>,
    line_info: &LineInfo,
) -> Vec<SemanticToken> {
    let tokens = tokens.into_iter();
    let (min, max) = tokens.size_hint();
    let size_hint = max.unwrap_or(min);
    let mut result = Vec::with_capacity(size_hint);

    let mut last_line = 0;
    let mut last_start = 0;

    for Loc {
        region,
        value: token,
    } in tokens
    {
        let length = region.len();

        let LineColumn { line, column } = line_info.convert_pos(region.start());

        let delta_line = line - last_line;
        let delta_start = if delta_line == 0 {
            column - last_start
        } else {
            column
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token as u32,
            token_modifiers_bitset: 0,
        });

        last_line = line;
        last_start = column;
    }

    result
}
