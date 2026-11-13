# META
~~~ini
description=F32/F64 arithmetic, comparisons, and F64 to I64 conversion in host-called functions
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : F64, F32, I64 -> I64
main = |x, y, n| {
    scaled = x * 2.5 - I64.to_f64(n) / 3.0
    narrowed = F32.to_f64(y * 1.5 + 0.25)
    total = if scaled > narrowed { scaled - narrowed } else { narrowed + scaled }
    F64.to_i64_wrap(total)
}
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : F64, F32, I64 -> I64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : F64, F32, I64 -> I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = |x, y, n| {
	scaled = x * 2.5 - to_f64(n) / 3
	narrowed = to_f64(y * 1.5 + 0.25)
	total = if (scaled > narrowed) {
		scaled - narrowed
	} else {
		narrowed + scaled
	}
	to_i64_wrap(total)
}

~~~
# DEV OUTPUT
~~~ini
x64mac=4ca8e4ccb886c6cbf900261d445f287f005150e54ff464fa02c6270518ba6eab
x64win=ea5a291f031fb211b8aa8da775c7b740d2ac48b52b8c4266efabd3f2afc1a98c
x64mingw=ea5a291f031fb211b8aa8da775c7b740d2ac48b52b8c4266efabd3f2afc1a98c
x64freebsd=c54de1dcf52028deba5181b7b560fdb2e386971153c7b03020484697c17822b1
x64openbsd=55645e394832b55513bd8a17af1c9b3fe7c528c3ee52752ec380ebb07d01bcb1
x64netbsd=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64musl=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64glibc=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64linux=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64elf=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64v1mac=4ca8e4ccb886c6cbf900261d445f287f005150e54ff464fa02c6270518ba6eab
x64v1win=ea5a291f031fb211b8aa8da775c7b740d2ac48b52b8c4266efabd3f2afc1a98c
x64v1mingw=ea5a291f031fb211b8aa8da775c7b740d2ac48b52b8c4266efabd3f2afc1a98c
x64v1freebsd=c54de1dcf52028deba5181b7b560fdb2e386971153c7b03020484697c17822b1
x64v1openbsd=55645e394832b55513bd8a17af1c9b3fe7c528c3ee52752ec380ebb07d01bcb1
x64v1netbsd=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64v1musl=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64v1glibc=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64v1linux=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
x64v1elf=f05d030c0057d0a1774328e6d0f609f2af41c44637c70b564d9927974d7934d1
arm64mac=a58f50aadbcd3b4b4ebadc4fdbbffe838aba1c115b338fabb4d2b94bf55b1fd4
arm64win=99ccb63cf8ca4a910601bfc16edf6416f94a910e7f19160a458c2dfc85dfdd6b
arm64mingw=99ccb63cf8ca4a910601bfc16edf6416f94a910e7f19160a458c2dfc85dfdd6b
arm64linux=6a29159d1ad44bbcba8a61b8347f2155b5038d8f31da006837d05c8414d25c8e
arm64musl=6a29159d1ad44bbcba8a61b8347f2155b5038d8f31da006837d05c8414d25c8e
arm64glibc=6a29159d1ad44bbcba8a61b8347f2155b5038d8f31da006837d05c8414d25c8e
arm64v1win=99ccb63cf8ca4a910601bfc16edf6416f94a910e7f19160a458c2dfc85dfdd6b
arm64v1mingw=99ccb63cf8ca4a910601bfc16edf6416f94a910e7f19160a458c2dfc85dfdd6b
arm64v1linux=6a29159d1ad44bbcba8a61b8347f2155b5038d8f31da006837d05c8414d25c8e
arm64v1musl=6a29159d1ad44bbcba8a61b8347f2155b5038d8f31da006837d05c8414d25c8e
arm64v1glibc=6a29159d1ad44bbcba8a61b8347f2155b5038d8f31da006837d05c8414d25c8e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
