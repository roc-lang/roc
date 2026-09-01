import pf.Cfg as Remote

# App-local module deliberately named Cfg, like the platform's Cfg module.
# Constructing the platform Cfg with `n` omitted inlines the platform
# default (which overflows U8) into THIS module's compile-time root. The
# provenance comparison must use package-qualified module identity: matching
# by bare name would judge the crash local and render the platform module's
# byte offsets against this file.
Cfg := {}.{
	remote_cfg : Remote.Cfg
	remote_cfg = Remote.Cfg.{}
}
