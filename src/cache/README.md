# Cache

Cache intermediate compilation artefacts to speed up subsequent builds. We can tokenize->parse->canonicalize each file in isolation then cache and re-use this information. Only if the source code changes do we need to recompute the canonicalized representation.
