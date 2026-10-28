#!/usr/bin/env python3
"""Focused Linux benchmark for #12571; use a ReleaseFast diagnostic compiler.

    zig build -j2 roc -Doptimize=ReleaseFast -Ddev-diagnostics
    python3 test/cli/pack_index_bench.py zig-out/bin/roc --platform <built-fx-open-platform>/main.roc

All measured commands are `roc build --opt=dev`. GNU time records wall time
and peak RSS. Synthetic history consists of valid versioned offer records for
unrequested keys, each with a distinct enlarged artifact payload. The script
checks actual executable output and asserts metadata/payload I/O independence.
Use the staged fx-open platform from a CLI test build, or prepare the checked-out
fixture with `zig build -j2 update-test-fixtures`. Temporary source, cache and
executable files live under TMPDIR.
"""
import argparse
import hashlib
import os
from pathlib import Path
import re
import statistics
import struct
import subprocess
import tempfile


def sha(data):
    return hashlib.sha256(data).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("roc", type=Path)
    parser.add_argument("--platform", type=Path, default=Path(__file__).resolve().parents[1] / "fx-open/platform/main.roc")
    parser.add_argument("--history", type=int, default=1000)
    parser.add_argument("--repeats", type=int, default=3)
    args = parser.parse_args()
    roc = args.roc.resolve()
    with tempfile.TemporaryDirectory(prefix="roc-pack-index-") as temp:
        work = Path(temp)
        cache = work / "cache"
        app = work / "main.roc"
        app.write_text('app [main!] { pf: platform "' + os.path.relpath(args.platform.resolve(), work) + '" }\n' + '''import pf.Stdout
import Stable
main! = |args| {
    Stdout.line!(Stable.total(args.len().to_u64() + 10).to_str())
    Ok({})
}
''')
        (work / "Stable.roc").write_text('''Stable := [].{
    total : U64 -> U64
    total = |limit| {
        var $sum = 0
        var $index = 0
        while $index < limit {
            $sum = $sum + $index
            $index = $index + 1
        }
        $sum
    }
}
''')
        env = dict(os.environ, ROC_CACHE_DIR=str(cache), ROC_PACK_STATS="1")
        results = []

        def run(label, force_lookup=False, no_cache=False, expected="55\n"):
            if force_lookup:
                for result in cache.rglob("*.rpo"):
                    result.unlink()
            cmd = [str(roc), "build", "--opt=dev", "--jobs=1", str(app), f"--output={work / 'app'}"]
            if no_cache:
                cmd.append("--no-cache")
            completed = subprocess.run(["/usr/bin/time", "-f", "%e %M", "-o", str(work / "time"), *cmd], env=env, capture_output=True, text=True)
            if completed.returncode:
                raise RuntimeError(completed.stdout + completed.stderr)
            stats = {}
            publication_ns = 0
            for line in completed.stderr.splitlines():
                if line.startswith("pack publication:"):
                    publication_ns += int(re.search(r"wall_ns=(\d+)", line)[1])
                if not line.startswith("pack index:"):
                    continue
                for key, value in re.findall(r"(\w+)=(\d+)", line):
                    stats[key] = stats.get(key, 0) + int(value)
            if not no_cache and not stats:
                raise RuntimeError("Build roc with -Doptimize=ReleaseFast -Ddev-diagnostics")
            output = subprocess.check_output([str(work / "app")], text=True)
            assert output == expected, output
            wall, rss = (work / "time").read_text().split()
            results.append((label, float(wall), int(rss), stats))
            print(label, f"{wall}s", f"{rss}KiB", f"publication={publication_ns / 1e6:.3f}ms", stats, flush=True)
            return stats

        run("cold publication")
        warm = run("completed-result hit")
        assert warm["payload_opens"] == warm["index_bytes"] == 0
        run("lookup stabilization", force_lookup=True)
        baseline = run("lookup baseline", force_lookup=True)
        assert baseline["payload_files"] > 0
        templates = sorted(cache.rglob("*.rpi"))
        template = templates[0]
        offer_template = template.read_bytes()
        assert offer_template[:8] == b"ROFFER03"
        namespace = template.parents[3]
        owner = namespace / "local" / ("00" * 32)
        metadata_dir = owner / "metadata"
        payload_dir = owner / "payload"
        metadata = (metadata_dir / (offer_template[8:40].hex() + ".rpm")).read_bytes()
        assert metadata[:8] == b"ROFCOMP1"
        assert struct.unpack_from("<I", metadata, 8)[0] > 0
        original_payload = (payload_dir / (metadata[12:44].hex() + ".rpa")).read_bytes()
        assert original_payload[:8] == b"RPAYLD01"
        code_len = struct.unpack_from("<Q", original_payload, 8)[0]
        spec_offset = offer_template.rindex(bytes.fromhex(template.parent.name))
        metadata_growth = payload_growth = 0
        for i in range(args.history):
            key = hashlib.sha256(f"unrequested-specialization-{i}".encode()).digest()
            # Enlarge the code region without changing any existing offset.
            extra = struct.pack("<Q", i) + bytes(64 * 1024)
            payload = b"RPAYLD01" + struct.pack("<Q", code_len + len(extra)) + original_payload[16:16 + code_len] + extra + original_payload[16 + code_len:]
            digest = sha(payload)
            (payload_dir / (digest + ".rpa")).write_bytes(payload)
            shared = bytearray(metadata)
            shared[12:44] = bytes.fromhex(digest)
            shared_hash = sha(shared)
            (metadata_dir / (shared_hash + ".rpm")).write_bytes(shared)
            offer = bytearray(offer_template)
            offer[8:40] = bytes.fromhex(shared_hash)
            offer[spec_offset:spec_offset + 32] = key
            bucket = template.parent.parent / key.hex()
            bucket.mkdir()
            (bucket / (template.name[:65] + sha(offer) + ".rpi")).write_bytes(offer)
            metadata_growth += len(shared) + len(offer)
            payload_growth += len(payload)
        print(f"history: {args.history} offers, {metadata_growth} metadata bytes, {payload_growth} payload bytes", flush=True)
        for _ in range(args.repeats):
            grown = run("lookup with history", force_lookup=True)
            for key in ("payload_files", "payload_bytes", "decoded_artifacts", "index_bytes"):
                assert grown[key] == baseline[key], (key, baseline, grown)
        original = app.read_text()
        app.write_text(original.replace("+ 10", "+ 11"))
        edited = run("small edit with history", expected="66\n")
        assert edited["served"] > 0
        run("cache disabled", no_cache=True, expected="66\n")
        app.write_text(original)
        reverted = run("reverted edit with history", force_lookup=True)
        assert reverted["served"] > 0
        sample = [r for r in results if r[0] == "lookup with history"]
        print("history lookup medians:", statistics.median(r[1] for r in sample), "seconds;", statistics.median(r[2] for r in sample), "KiB")


if __name__ == "__main__":
    main()
