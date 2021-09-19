#!/usr/bin/python3
import numpy as np
import random
import sys
import subprocess
import time
import tqdm

# Build and strip roc executable
print('Building roc compiler...')
result_code = subprocess.call(
    [
        'cargo',
        'build',
        '--release',
    ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
if result_code != 0:
    print('Failed to build compiler')
    exit()

print('\nStripping debug info from roc compiler...')
result_code = subprocess.call(
    [
        'strip',
        '--strip-all',
        'target/release/roc',
    ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
if result_code != 0:
    print('Failed to strip debug info from the compiler')
    exit()

# Ensure the host is precompiled
print('\nPrecompiling host...')
result_code = subprocess.call(
    [
        'target/release/roc',
        'build',
        '--optimize',
        '--roc-linker',
        'examples/hello-zig/Hello.roc'
    ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
if result_code != 0:
    print('Failed to precomile the roc host')
    exit()


print('\nBenchmarking...')

N = int(sys.argv[1])
program_names = ['roc', 'python3']
programs = [[
    'target/release/roc',
    '--dev',
    '--roc-linker',
    '--precompiled-host',
    'examples/hello-zig/Hello.roc'
], [
    '/usr/bin/python3',
    'hello.py'
]]

times = np.zeros((len(programs), N))
enumerated_programs = list(enumerate(programs))
for j in tqdm.trange(N):
    random.shuffle(enumerated_programs)
    for i, program in enumerated_programs:
        start = time.time()
        result_code = subprocess.call(
            program, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if result_code != 0:
            print('The program failed to execute')
            exit()
        times[i, j] = (time.time() - start)*1000

for i, name in enumerate(program_names):
    print(f'{name}:')
    print(f'\tMean: {np.mean(times[i])} ms')
    print(f'\tStddev: {np.std(times[i])} ms')
    print(f'\tStderr: {np.std(times[i])/(N**0.5)} ms')
