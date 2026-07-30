app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr
import pf.Stdin

str : Str -> Str
str = |s| s

main! = || {
	bytes = Stdin.line!().to_utf8()
	sha256 = Crypto.SHA256.hash(bytes)
	blake3 = Crypto.BLAKE3.hash(bytes)

	sha256_expected = Crypto.SHA256.Digest.from_hex("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
	blake3_expected = Crypto.BLAKE3.Digest.from_hex("6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85")

	sha256_hasher0 = Crypto.SHA256.Hasher.empty()
	sha256_hasher1 = Crypto.SHA256.Hasher.write(sha256_hasher0, bytes)
	sha256_stream = Crypto.SHA256.Hasher.finish(sha256_hasher1)

	hashes_ok = match (sha256_expected, blake3_expected) {
		(Ok(expected_sha256), Ok(expected_blake3)) =>
			Crypto.SHA256.Digest.is_eq(sha256, expected_sha256)
				and Crypto.SHA256.Digest.is_eq(sha256_stream, expected_sha256)
					and Crypto.BLAKE3.Digest.is_eq(blake3, expected_blake3)
		_ => False
	}

	Stdout.line!(str("Hello from stdout!"))
	Stdout.line!(str("Line 1 to stdout"))
	Stderr.line!(str("Line 2 to stderr"))
	Stdout.line!(str("Line 3 to stdout"))
	Stderr.line!(str("Error from stderr!"))
	if hashes_ok {
		Stdout.line!(str("Crypto hashes ok"))
	} else {
		Stderr.line!(str("Crypto hashes failed"))
	}
}
