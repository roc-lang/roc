interface InternalFile
    exposes [ReadErr, WriteErr]
    imports []

ReadErr : [NotFound]

WriteErr : [PermissionDenied]