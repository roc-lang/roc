# A hosted function whose host really returns Err: FallibleNotFound.not_found!
# returns Err(NotFound) from its declared row [NotFound, PermissionDenied].
FallibleNotFound := [].{
	not_found! : {} => Try(Str, [NotFound, PermissionDenied])
}
