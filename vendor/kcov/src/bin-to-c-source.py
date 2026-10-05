#!/usr/bin/env python3

import sys, struct

def generate(data_in, base_name):
	print("const uint8_t %s_data_raw[] = {" % (base_name))

	for i in range(0, len(data), 20):
		line = bytearray(data[i:i+20])

		for val in line:
			print("0x%02x," % (val), end="")
		print("")

	print("};")
	print("GeneratedData %s_data(%s_data_raw, sizeof(%s_data_raw));" % (base_name, base_name, base_name))

if __name__ == "__main__":
	if len(sys.argv) < 3 or (len(sys.argv) - 1) % 2 != 0:
		print("Usage: bin-to-source.py <file> <base-name> [<file2> <base-name2>]")
		sys.exit(1)

	print("#include <stdint.h>")
	print("#include <stdlib.h>")
	print("#include <generated-data-base.hh>")
	print("using namespace kcov;")

	for i in range(1,len(sys.argv), 2):
		file = sys.argv[i]
		base_name = sys.argv[i + 1]

		f = open(file, "rb")
		data = f.read()
		f.close()

		generate(data, base_name)
