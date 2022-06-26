interface CanPath
    exposes [fromStr, root, toComponents, walkComponents]
    imports [CanPath, PathRoot, CanPathComponent]

## Calls [Path.fromStr] and then passes the result to [Path.canonicalize].
fromStr : Str -> Task CanPath (CanonicalizeErr *) [Metadata, Cwd]*

root : CanPath -> PathRoot

toComponents : CanPath -> (PathRoot, List CanPathComponent)

walkComponents :
    CanPath,
    (PathRoot -> state),
    (state, CanPathComponent -> state)
    -> state
