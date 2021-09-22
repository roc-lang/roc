interface Response
    exposes [ Status, Response, ok, status ]
    imports [ fx.Effect, Task ]

Status : U16
Response : [ @Response Status Str ]

# ok : Str -> Response
# ok = \str -> @Response 200 str

# status : Status, Str -> Response
# status = \status, str -> @Response status str
