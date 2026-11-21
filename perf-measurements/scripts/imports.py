"""Find the local .roc file imported by the most other local files reachable from a root."""
from pathlib import Path
import re
import sys

IMPORT = re.compile(r"^\s*import\s+([A-Za-z_][\w]*(?:\.[A-Z][\w]*)*)", re.M)
EXPOSES = re.compile(r"^\s*(?:package|platform\s+\"[^\"]*\"(?:\s+requires\s+\{[^}]*\})?\s+exposes|module)\s*\[([^\]]*)\]", re.M | re.S)
PATH_DEP = re.compile(r'(\w+)\s*:\s*"([^"]+\.roc)"')


def package_deps(file: Path):
    """Shorthand -> local package root file declared in this file's header."""
    head = file.read_text(errors="replace").split("\n\n", 1)[0]
    text = file.read_text(errors="replace")
    header_end = text.find("}")
    header = text[: header_end + 1] if header_end != -1 else head
    deps = {}
    for short, target in PATH_DEP.findall(header):
        if target.startswith("http"):
            continue
        p = (file.parent / target).resolve()
        if p.exists():
            deps[short] = p
    return deps


def resolve(importer: Path, package_root: Path, deps, name: str):
    parts = name.split(".")
    if parts[0][0].islower():
        dep = deps.get(parts[0])
        if dep is None:
            return None, None
        base = dep.parent
        rel = parts[1:]
        root = dep
    else:
        base = package_root.parent
        rel = parts
        root = package_root
    candidate = base.joinpath(*rel[:-1], rel[-1] + ".roc") if rel else None
    if candidate and candidate.exists():
        return candidate.resolve(), root
    return None, None


def graph(root: Path):
    root = root.resolve()
    counts = {}
    seen = set()
    work = [(root, root)]
    while work:
        file, package_root = work.pop()
        if file in seen:
            continue
        seen.add(file)
        deps = package_deps(package_root)
        if file == package_root:
            deps.update(package_deps(file))
        text = file.read_text(errors="replace")
        exposed = EXPOSES.search(text[:4000]) if file == package_root else None
        if exposed:
            for name in re.findall(r"[A-Z]\w*", exposed.group(1)):
                candidate = (file.parent / (name + ".roc")).resolve()
                if candidate.exists() and candidate not in seen:
                    work.append((candidate, package_root))
        for name in IMPORT.findall(text):
            target, target_root = resolve(file, package_root, deps, name)
            if target is None:
                continue
            counts.setdefault(target, set()).add(file)
            work.append((target, target_root if target_root != package_root or target_root is None else package_root))
            if target_root is not None and target_root != package_root and target_root not in seen:
                work.append((target_root, target_root))
    return seen, counts


def most_imported(root: Path, repo: Path):
    seen, counts = graph(root)
    best = None
    for target, importers in counts.items():
        if target == root.resolve() or repo.resolve() not in target.parents:
            continue
        key = (len(importers), str(target))
        if best is None or key > best[0]:
            best = (key, target)
    return (best[1], best[0][0]) if best else (None, 0)


if __name__ == "__main__":
    repo = Path(sys.argv[1])
    root = repo / sys.argv[2]
    target, n = most_imported(root, repo)
    print(target.relative_to(repo.resolve()) if target else None, n)
