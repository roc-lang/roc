interface InternalFile
    exposes [ReadErr, WriteErr]
    imports []

ReadErr : [NotFound, Other]

WriteErr : [PermissionDenied, Other]