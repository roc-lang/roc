// Shared by the browser UI and the search ranking tests.
(function (root) {
  const normalize = text => text.toLowerCase().replace(/\s+/g, " ").trim();
  // Within each match location, prefer exact names, then prefixes; preserve ties.
  function rankApi(entries, query) {
    const q = query.trim().toLowerCase();
    if (!q) return [];
    const signatureQuery = q.replace(/\s+/g, "");
    const types = [], names = [], signatures = [];
    for (const entry of entries) {
      if (entry.name.includes(q)) {
        (entry.kind === "type" ? types : names).push(entry);
      } else if (entry.signature.includes(signatureQuery)) signatures.push(entry);
    }
    const namePriority = entry => entry.name === q ? 0 : entry.name.endsWith(`.${q}`) ? 1 :
      entry.name.startsWith(q) ? 2 : 3;
    const byName = (a, b) => namePriority(a) - namePriority(b);
    return types.sort(byName).concat(names.sort(byName), signatures);
  }
  function rank(documents, query, { allowAliasPrefixes = true } = {}) {
    const q = normalize(query);
    if (!q) return [];
    const words = q.split(" ");
    const matches = documents.flatMap((entry, order) => {
      const title = normalize(entry.title);
      const body = normalize(entry.text);
      const terms = entry.terms || [];
      const matchedTerm = terms.find(term => normalize(term) === q) ||
        (allowAliasPrefixes ? terms.find(term => /[\p{L}\p{N}]/u.test(q) && normalize(term).startsWith(q)) : undefined);
      const exactTerm = matchedTerm !== undefined && normalize(matchedTerm) === q;
      let score = exactTerm ? 1000 : matchedTerm ? 900 : title === q ? 800 : title.includes(q) ? 600 : 0;
      if (!score && words.every(word => `${title} ${body}`.includes(word))) score = 100;
      return score ? [{ ...entry, score, order, exactTerm, matchedTerm }] : [];
    }).sort((a, b) => b.score - a.score || a.order - b.order);
    const curatedSymbol = !/[\p{L}\p{N}]/u.test(q) && matches.some(item => item.exactTerm);
    const seen = new Set();
    return matches.filter(item => {
      if (curatedSymbol && !item.exactTerm) return false;
      const key = `${item.category} ${item.href}`;
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }
  function queryPattern(query) {
    const words = [...new Set(query.trim().split(/\s+/).filter(Boolean))];
    if (!words.length) return null;
    const escaped = words.sort((a, b) => b.length - a.length)
      .map(word => word.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"));
    return new RegExp(escaped.join("|"), "giu");
  }
  function snippet(text, query) {
    const clean = text.replace(/\s+/g, " ").trim();
    const pattern = queryPattern(query);
    const position = pattern ? clean.search(pattern) : -1;
    const start = Math.max(0, position - 45);
    return `${start ? "…" : ""}${clean.slice(start, start + 170)}${clean.length > start + 170 ? "…" : ""}`;
  }
  function highlightParts(text, query) {
    const pattern = queryPattern(query);
    if (!pattern) return [{ text, match: false }];
    const parts = [];
    let offset = 0;
    for (const match of text.matchAll(pattern)) {
      if (match.index > offset) parts.push({ text: text.slice(offset, match.index), match: false });
      parts.push({ text: match[0], match: true });
      offset = match.index + match[0].length;
    }
    if (offset < text.length) parts.push({ text: text.slice(offset), match: false });
    return parts;
  }
  const api = { normalize, rankApi, rank, snippet, highlightParts };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.rocDocsSearch = api;
})(globalThis);
