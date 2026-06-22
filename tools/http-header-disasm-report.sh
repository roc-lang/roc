#!/usr/bin/env bash
set -euo pipefail

usage() {
    cat <<'EOF'
usage: tools/http-header-disasm-report.sh [--skip-roc-build] [--work-dir DIR]

Build and disassemble the Roc HTTP header platform example, picohttpparser, and
a comparable may_minihttp example. The script writes all artifacts under DIR and
prints a repeatable metrics report with sizes, function extents, stack frames,
and hot-path call counts.

The default DIR is /tmp/roc-http-header-disasm-report.
EOF
}

skip_roc_build=0
work_dir="/tmp/roc-http-header-disasm-report"

while [ "$#" -gt 0 ]; do
    case "$1" in
        --skip-roc-build)
            skip_roc_build=1
            shift
            ;;
        --work-dir)
            if [ "$#" -lt 2 ]; then
                usage >&2
                exit 2
            fi
            work_dir="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            usage >&2
            exit 2
            ;;
    esac
done

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
roc_bin="$repo_root/zig-out/bin/http_header_decoder_server_prebuilt"
llvm_objdump="${LLVM_OBJDUMP:-$(xcrun --find llvm-objdump)}"
clang_bin="${CLANG:-$(xcrun --find clang)}"
macos_sdk="${MACOS_SDK:-$(xcrun --show-sdk-path)}"

mkdir -p "$work_dir"

if [ "$skip_roc_build" -eq 0 ]; then
    (cd "$repo_root" && zig build run-test-zig-http-header-decoder-platform)
fi

if [ ! -x "$roc_bin" ]; then
    echo "missing Roc HTTP header binary: $roc_bin" >&2
    exit 1
fi

roc_disasm="$work_dir/roc_http_header.s"
"$llvm_objdump" --macho --disassemble --demangle "$roc_bin" > "$roc_disasm"

pico_dir="$work_dir/picohttpparser"
pico_tar="$work_dir/picohttpparser.tar.gz"
rm -rf "$pico_dir" "$work_dir/picohttpparser-master"
curl -L --fail --silent --show-error https://github.com/h2o/picohttpparser/archive/refs/heads/master.tar.gz -o "$pico_tar"
tar -xzf "$pico_tar" -C "$work_dir"
mv "$work_dir/picohttpparser-master" "$pico_dir"
pico_obj="$work_dir/picohttpparser_arm64_o3.o"
"$clang_bin" -O3 -fomit-frame-pointer -target arm64-apple-macos11 -isysroot "$macos_sdk" -I"$pico_dir" -c "$pico_dir/picohttpparser.c" -o "$pico_obj"
pico_disasm="$work_dir/picohttpparser_arm64_o3.s"
"$llvm_objdump" --macho --disassemble --demangle "$pico_obj" > "$pico_disasm"

may_dir="$work_dir/may_minihttp"
may_tar="$work_dir/may_minihttp.tar.gz"
rm -rf "$may_dir" "$work_dir/may_minihttp-master"
curl -L --fail --silent --show-error https://github.com/Xudong-Huang/may_minihttp/archive/refs/heads/master.tar.gz -o "$may_tar"
tar -xzf "$may_tar" -C "$work_dir"
mv "$work_dir/may_minihttp-master" "$may_dir"
cat > "$may_dir/examples/roc_compare.rs" <<'EOF'
use bytes::BufMut;
use may_minihttp::{HttpServer, HttpService, Request, Response};
use std::io::{self, Write};

#[derive(Clone)]
struct RocCompare;

impl HttpService for RocCompare {
    fn call(&mut self, req: Request, rsp: &mut Response) -> io::Result<()> {
        let mut total = 0usize;

        for header in req.headers() {
            if header.name.eq_ignore_ascii_case("foo") || header.name.eq_ignore_ascii_case("bar") {
                total += header.value.len();
            }
        }

        rsp.header("Content-Type: text/plain");
        let mut buf = itoa::Buffer::new();
        rsp.body_mut().writer().write_all(buf.format(total).as_bytes())?;
        Ok(())
    }
}

fn main() {
    let server = HttpServer(RocCompare).start("127.0.0.1:8080").unwrap();
    server.wait();
}
EOF
cargo build --manifest-path "$may_dir/Cargo.toml" --release --example roc_compare
may_bin="$may_dir/target/release/examples/roc_compare"
may_disasm="$work_dir/may_minihttp_roc_compare.s"
"$llvm_objdump" --macho --disassemble --demangle "$may_bin" > "$may_disasm"

artifact_size() {
    stat -f %z "$1"
}

print_artifact_summary() {
    printf 'artifact\ton_disk_bytes\tdisasm_lines\tpath\n'
    printf 'roc\t%s\t%s\t%s\n' "$(artifact_size "$roc_bin")" "$(wc -l < "$roc_disasm" | tr -d ' ')" "$roc_bin"
    printf 'pico\t%s\t%s\t%s\n' "$(artifact_size "$pico_obj")" "$(wc -l < "$pico_disasm" | tr -d ' ')" "$pico_obj"
    printf 'may\t%s\t%s\t%s\n' "$(artifact_size "$may_bin")" "$(wc -l < "$may_disasm" | tr -d ' ')" "$may_bin"
}

print_segment_summary() {
    printf '\nsegment summary from size(1)\n'
    size "$roc_bin" "$pico_obj" "$may_bin" 2>&1
}

print_function_metrics() {
    perl - "$@" <<'PERL'
use strict;
use warnings;
no warnings 'portable';

my @targets = (
    [roc => qr/^_(?:main|host\.cMain|roc_main|roc__proc_[A-Za-z0-9_]+)$/],
    [pico => qr/^_(?:phr_parse_request|phr_parse_headers|parse_headers|parse_request)$/],
    [may => qr/^(?:__ZN8httparse25parse_headers_iter_uninit|__ZN8httparse7Request36parse_with_config_and_uninit_headers|__ZN12may_minihttp8response6encode|__ZN11roc_compare4main|_main|__ZN9generator5stack17StackBox.*call_once)/],
);

sub parse_imm {
    my ($text) = @_;
    return 0 if !defined $text || $text eq "";
    if ($text =~ /#0x([0-9a-fA-F]+),\s*lsl\s*#([0-9]+)/) {
        return hex($1) << $2;
    }
    if ($text =~ /#0x([0-9a-fA-F]+)/) {
        return hex($1);
    }
    if ($text =~ /#([0-9]+)/) {
        return int($1);
    }
    return 0;
}

sub emit_for_file {
    my ($artifact, $path) = @_;
    open my $fh, "<", $path or die "open $path: $!";

    my @functions;
    my $current;
    while (my $line = <$fh>) {
        chomp $line;
        if ($line =~ /^([^:\s][^:]*):$/) {
            if ($current) {
                push @functions, $current;
            }
            $current = {
                name => $1,
                start => undef,
                end => undef,
                save => 0,
                local => 0,
                saw_predec_save => 0,
                saw_sub => 0,
                lines => [],
                calls => {},
            };
            next;
        }
        next unless $current;
        if ($line =~ /^\s*([0-9a-fA-F]+):/) {
            my $addr = hex($1);
            $current->{start} = $addr if !defined $current->{start};
            $current->{end} = $addr;
        }
        if (!$current->{saw_predec_save} && $line =~ /\bstp\b.*\[sp,\s*#-0x([0-9a-fA-F]+)\]!/) {
            $current->{save} += hex($1);
            $current->{saw_predec_save} = 1;
        }
        if (!$current->{saw_sub} && $line =~ /\bsub\s+sp,\s*sp,\s*(#[^;\n]+)/) {
            $current->{local} += parse_imm($1);
            $current->{saw_sub} = 1;
        }
        if ($line =~ /\bbl\s+([^\s;]+)/) {
            $current->{calls}{$1}++;
        }
    }
    push @functions, $current if $current;

    my ($regex) = map { $_->[1] } grep { $_->[0] eq $artifact } @targets;
    for my $i (0 .. $#functions) {
        my $fn = $functions[$i];
        next if !defined $fn->{start};
        next if $fn->{name} !~ $regex;
        my $next_start;
        for my $j ($i + 1 .. $#functions) {
            if (defined $functions[$j]{start}) {
                $next_start = $functions[$j]{start};
                last;
            }
        }
        my $size = defined $next_start ? $next_start - $fn->{start} : 0;
        printf "%s\t%s\t0x%x\t%d\t%d\t%d\t%d\n",
            $artifact,
            $fn->{name},
            $fn->{start},
            $size,
            $fn->{local},
            $fn->{save},
            $fn->{local} + $fn->{save};
    }
}

print "artifact\tfunction\tstart\tcode_bytes\tlocal_stack_bytes\tsave_stack_bytes\ttotal_stack_bytes\n";
while (@ARGV) {
    my $artifact = shift @ARGV;
    my $path = shift @ARGV;
    emit_for_file($artifact, $path);
}
PERL
}

print_hot_call_counts() {
    printf '\nhot-path call counts\n'
    printf 'artifact\tneedle\tcount\n'
    for artifact_path in \
        "roc:$roc_disasm" \
        "pico:$pico_disasm" \
        "may:$may_disasm"
    do
        artifact="${artifact_path%%:*}"
        path="${artifact_path#*:}"
        for needle in \
            "roc_alloc" \
            "malloc" \
            "calloc" \
            "roc_builtins_str_equal" \
            "roc_builtins_str_caseless_ascii_equals" \
            "roc_builtins_roc_crashed" \
            "roc__proc_" \
            "parse_headers" \
            "parse_with_config_and_uninit_headers"
        do
            count="$(grep -c "$needle" "$path" || true)"
            printf '%s\t%s\t%s\n' "$artifact" "$needle" "$count"
        done
    done
}

cat <<EOF
HTTP header parser disassembly report
work_dir=$work_dir
roc_disasm=$roc_disasm
pico_disasm=$pico_disasm
may_disasm=$may_disasm

EOF

print_artifact_summary
print_segment_summary
printf '\nfunction metrics\n'
print_function_metrics roc "$roc_disasm" pico "$pico_disasm" may "$may_disasm"
print_hot_call_counts
