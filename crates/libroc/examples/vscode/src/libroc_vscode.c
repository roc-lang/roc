// This will get compiled into a native node addon,
// which in turn will be called by index.js.
//
// Its job is to:
// - Wrap libroc functions in Node wrappers, so index.js can register them
// - In line/col APIs, translate Node's UTF-16 columns into UTF-8 columns

// https://vscode-api.js.org/interfaces/vscode.CompletionItemProvider.html#provideCompletionItems
// takes a Position and a document, but the Position's characters are
// given in UTF-16 units - whereas libroc expects utf-8 units. We can use
// https://vscode-api.js.org/interfaces/vscode.TextDocument.html#getText
// passing in a range with an end Position equal to the given Position,
// and the start Position being the beginning of that line (so, character 0).
// then we can iterate through those utf-16 bytes and count how many utf-8
// bytes it would have taken to represent them, at which point we have our
// utf8 column!
//
// Similarly, when receiving a Position equivalent from libroc, we can
// use the line to index into the Document and then count how many utf-16
// characters it would have taken to represent those utf-8 bytes in the doc.
