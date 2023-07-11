interface Http exposes [request] imports [HttpInternal]

request : Str -> Str
request = \req -> HttpInternal.request req
