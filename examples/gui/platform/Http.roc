interface Http
    exposes [ Response, ReqConfig, Body, Tracker, Progress, get, put, post, request, onProgress, onDone ]

Tracker : [ @Tracker { type : U64, id : U64 } ]

ReqConfig :
    {
        url : Str,
        headers ? List { name : Str, value : Str },
        timeout ? [ Ms U64, None ]
        tracker ? [ Tracked Tracker Str, None ]
    }

Body : { contentType : Str, bytes : List U8 }

tracker : { type : U64, id : U64 } -> Tracker
tracker = @Tracker

Http.get : ReqConfig -> Task Response (ReqError *)
Http.put : Body, ReqConfig -> Task Response (ReqError *)
Http.post : Body, ReqConfig -> Task Response (ReqError *)
Http.request : Body, { method : Str }ReqConfig -> Task Response (ReqError *)

Http.onProgress : Tracker, (state, Progress, Str -> Action state) -> Sub state
Http.onDone : Tracker, (state, Http.Response, Str -> Action state) -> Sub state

Progress :
    [
        SendingBytes { sent : Nat, total : Nat },
        ReceivingBytes { received : Nat, total : Result Nat [ SizeUnspecified ] },
    ]
