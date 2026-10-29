// Shared by the browser UI and the search ranking tests.
(function (root) {
  const normalize = text => text.toLocaleLowerCase().replace(/\s+/g, " ").trim();
  function rank(documents, query) {
    const q = normalize(query);
    if (!q) return [];
    const words = q.split(" ");
    const matches = documents.flatMap((entry, order) => {
      const title = normalize(entry.title);
      const body = normalize(entry.text);
      const terms = entry.terms || [];
      const matchedTerm = terms.find(term => normalize(term) === q) ||
        terms.find(term => /[\p{L}\p{N}]/u.test(q) && normalize(term).startsWith(q));
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
  function snippet(text, query) {
    const clean = text.replace(/\s+/g, " ").trim();
    const position = normalize(clean).indexOf(normalize(query));
    const start = Math.max(0, position - 45);
    return `${start ? "…" : ""}${clean.slice(start, start + 170)}${clean.length > start + 170 ? "…" : ""}`;
  }
  const api = { normalize, rank, snippet };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.rocDocsSearch = api;
})(globalThis);
