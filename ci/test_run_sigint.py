"""Check roc run's actual POSIX wait status in an isolated foreground group."""
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile


def main(roc: str) -> None:
    # An installed executable lets us test a host with any signal disposition,
    # without downloading a platform or depending on a particular backend.
    version = subprocess.check_output([roc, "version"], text=True).strip()
    version = version.removeprefix("Roc compiler version ")
    with tempfile.TemporaryDirectory() as root:
        env = {**os.environ, "ROC_INSTALL_DIR": root}
        entry = Path(root, version, "sigtest")
        app = entry / "bin" / "sigtest"
        app.parent.mkdir(parents=True)
        digest = "AQmoxbAY7eQfXMbi9XUxBvBGZcxZCs1tdNeFriRRkwSc"
        url = f"https://example.com/1.2.3/{digest}.tar.zst"
        (entry / "install.manifest").write_text(
            f"roc-install-manifest 2\nkind executable\nurl {len(url)}:{url}\n"
            f"hash {len(digest)}:{digest}\ncompiler_version {len(version)}:{version}\n"
        )

        def check(script: str, expected: int) -> None:
            app.write_text("#!/bin/sh\n" + script + "\n")
            app.chmod(0o755)
            result = subprocess.run(
                [roc, "run", "sigtest"], env=env, start_new_session=True,
                input="42\n", capture_output=True, text=True, timeout=15,
            )
            assert result.returncode == expected, (result.returncode, result.stderr)
            assert not result.stderr, result.stderr

        # `kill ... 0` signals the same group a terminal Ctrl+C would. The
        # app handles it, continues reading stdin, and chooses its exit code.
        check("trap 'caught=yes' INT\nkill -INT 0\n"
              '[ "$caught" = yes ] && read -r code || exit 1\nexit "$code"', 42)
        check("kill -INT 0\nexit 99", -signal.SIGINT)
        # Signal only the app: Roc must reproduce the signal after reaping it,
        # not just happen to have been killed by its own copy of Ctrl+C.
        check("kill -INT $$\nexit 99", -signal.SIGINT)
        check("exit 130", 130)

        # Shell background jobs may start with SIGINT ignored. Preserve that
        # disposition in the app instead of forcing it back to default.
        previous = signal.signal(signal.SIGINT, signal.SIG_IGN)
        try:
            check("kill -INT 0\nexit 42", 42)
        finally:
            signal.signal(signal.SIGINT, previous)


if __name__ == "__main__":
    signal.signal(signal.SIGINT, signal.default_int_handler)
    main(sys.argv[1])
