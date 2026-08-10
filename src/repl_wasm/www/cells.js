const delimiterPattern = /^[\t ]*#%%[\t ]*(?:\r?\n|$)/gm;
const encoder = new TextEncoder();

/**
 * Split a notebook document into executable cells. Delimiter lines belong to
 * the cell that follows them, so placing the cursor on `#%%` selects the next
 * cell. Source ranges exclude the delimiter and the preceding line ending.
 */
export function findCells(text) {
  const delimiters = Array.from(text.matchAll(delimiterPattern), (match) => ({
    start: match.index,
    end: match.index + match[0].length,
  }));
  const cells = [];
  let boundaryStart = 0;
  let sourceStart = 0;

  for (const delimiter of delimiters) {
    let sourceEnd = delimiter.start;
    if (sourceEnd > sourceStart && text[sourceEnd - 1] === "\n") {
      sourceEnd -= 1;
      if (sourceEnd > sourceStart && text[sourceEnd - 1] === "\r") sourceEnd -= 1;
    }
    cells.push({ boundaryStart, sourceStart, sourceEnd });
    boundaryStart = delimiter.start;
    sourceStart = delimiter.end;
  }

  cells.push({ boundaryStart, sourceStart, sourceEnd: text.length });
  return cells;
}

export function activeCell(text, cursor) {
  if (!Number.isInteger(cursor) || cursor < 0 || cursor > text.length) {
    throw new RangeError("Cursor must be a valid UTF-16 document offset");
  }
  const cells = findCells(text);
  let index = 0;
  for (let candidate = 1; candidate < cells.length; candidate += 1) {
    if (cursor < cells[candidate].boundaryStart) break;
    index = candidate;
  }
  const cell = cells[index];
  return {
    ...cell,
    index,
    source: text.slice(cell.sourceStart, cell.sourceEnd),
    localCursor: Math.max(0, Math.min(cell.sourceEnd, cursor) - cell.sourceStart),
  };
}

/** Move to the following cell, adding an empty one when the active cell is last. */
export function advanceCell(text, cellIndex) {
  const cells = findCells(text);
  if (!Number.isInteger(cellIndex) || cellIndex < 0 || cellIndex >= cells.length) {
    throw new RangeError("Cell index is outside the document");
  }
  if (cellIndex + 1 < cells.length) {
    return { text, cursor: cells[cellIndex + 1].sourceStart, created: false };
  }

  const separator = text.length === 0 || text.endsWith("\n") ? "#%%\n" : "\n#%%\n";
  const nextText = text + separator;
  return { text: nextText, cursor: nextText.length, created: true };
}

export function utf16ToUtf8Offset(text, utf16Index) {
  if (!Number.isInteger(utf16Index) || utf16Index < 0 || utf16Index > text.length) {
    throw new RangeError("UTF-16 offset is outside the string");
  }
  if (utf16Index > 0 && utf16Index < text.length) {
    const before = text.charCodeAt(utf16Index - 1);
    const after = text.charCodeAt(utf16Index);
    if (before >= 0xd800 && before <= 0xdbff && after >= 0xdc00 && after <= 0xdfff) {
      throw new RangeError("UTF-16 offset splits a surrogate pair");
    }
  }
  return encoder.encode(text.slice(0, utf16Index)).length;
}

export function utf8ToUtf16Offset(text, utf8Offset) {
  if (!Number.isInteger(utf8Offset) || utf8Offset < 0) {
    throw new RangeError("UTF-8 offset must be a non-negative integer");
  }
  let bytes = 0;
  let utf16 = 0;
  for (const scalar of text) {
    if (bytes === utf8Offset) return utf16;
    const scalarBytes = encoder.encode(scalar).length;
    if (bytes + scalarBytes > utf8Offset) throw new RangeError("UTF-8 offset is not a scalar boundary");
    bytes += scalarBytes;
    utf16 += scalar.length;
  }
  if (bytes === utf8Offset) return utf16;
  throw new RangeError("UTF-8 offset is outside the string");
}

/** Translate a cell-local UTF-8 replacement range into document UTF-16 offsets. */
export function completionDocumentRange(cell, replacement) {
  if (replacement.start > replacement.end) throw new RangeError("Completion replacement range is reversed");
  return {
    start: cell.sourceStart + utf8ToUtf16Offset(cell.source, replacement.start),
    end: cell.sourceStart + utf8ToUtf16Offset(cell.source, replacement.end),
  };
}
