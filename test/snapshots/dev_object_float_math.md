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
x64mac=a23af42a2e34d172a04653b592e827d9ea5de8fb81a589b2aeca4ee8d3a9828a
x64win=6f9969a73dea3712abc26bf6c7a20df0886ebe99a6b4509f12e98bdc5d6f990a
x64mingw=6f9969a73dea3712abc26bf6c7a20df0886ebe99a6b4509f12e98bdc5d6f990a
x64freebsd=19ee4473ee16f2ae6283e1a11a51266f4c79dbd2f4fc1399be16dc7a1d05fd6a
x64openbsd=eed930c95c1b1409a236faa5c0eb0593546c4c248cf70253133b4bcf8762f1f0
x64netbsd=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64musl=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64glibc=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64linux=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64elf=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64v1mac=a23af42a2e34d172a04653b592e827d9ea5de8fb81a589b2aeca4ee8d3a9828a
x64v1win=6f9969a73dea3712abc26bf6c7a20df0886ebe99a6b4509f12e98bdc5d6f990a
x64v1mingw=6f9969a73dea3712abc26bf6c7a20df0886ebe99a6b4509f12e98bdc5d6f990a
x64v1freebsd=19ee4473ee16f2ae6283e1a11a51266f4c79dbd2f4fc1399be16dc7a1d05fd6a
x64v1openbsd=eed930c95c1b1409a236faa5c0eb0593546c4c248cf70253133b4bcf8762f1f0
x64v1netbsd=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64v1musl=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64v1glibc=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64v1linux=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
x64v1elf=fc4ba86d9c260b0f5ebf1cf3b77c9ff99ee06f18fb7b2b872361171ac1d49955
arm64mac=29da672387cebb71309527e6b4383fa6918609b5e784efeebc2aef396d0a50f0
arm64win=f43ab5f1634c2aadc02294e50a115c1c0ac5af58aca62c95ba8313a09cc16e7f
arm64mingw=f43ab5f1634c2aadc02294e50a115c1c0ac5af58aca62c95ba8313a09cc16e7f
arm64linux=ada17f702da109afde2de78e9b4901ca3d181bbc4d49bc3fb1ceeda04e1c302b
arm64musl=ada17f702da109afde2de78e9b4901ca3d181bbc4d49bc3fb1ceeda04e1c302b
arm64glibc=ada17f702da109afde2de78e9b4901ca3d181bbc4d49bc3fb1ceeda04e1c302b
arm64v1win=f43ab5f1634c2aadc02294e50a115c1c0ac5af58aca62c95ba8313a09cc16e7f
arm64v1mingw=f43ab5f1634c2aadc02294e50a115c1c0ac5af58aca62c95ba8313a09cc16e7f
arm64v1linux=ada17f702da109afde2de78e9b4901ca3d181bbc4d49bc3fb1ceeda04e1c302b
arm64v1musl=ada17f702da109afde2de78e9b4901ca3d181bbc4d49bc3fb1ceeda04e1c302b
arm64v1glibc=ada17f702da109afde2de78e9b4901ca3d181bbc4d49bc3fb1ceeda04e1c302b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
