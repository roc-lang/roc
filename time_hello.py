#!/usr/bin/python3
import numpy as np
import random
import re
import sys
import subprocess
import secrets
import time
import tqdm
random.seed()

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
program_names = [
    'roc',
    'python3',
    # 'zig'
]
program_files = [
    'examples/hello-zig/Hello.roc',
    'hello.py',
    # 'hello.zig'
]
programs = [[
    'target/release/roc',
    '--dev',
    '--roc-linker',
    '--precompiled-host',
    'examples/hello-zig/Hello.roc'
], [
    '/usr/bin/python3',
    'hello.py'
],
    # [
    # 'zig',
    # 'run',
    # 'hello.zig'
    # ]
]
regex = re.compile(r"Hello, .*!", re.IGNORECASE)

times = np.zeros((len(programs), N))
enumerated_programs = list(enumerate(programs))
for j in tqdm.trange(N):
    new_str = f'Hello, {secrets.token_urlsafe(16)[:6]}!'
    for file in program_files:
        with open(file, 'r') as in_file:
            content = in_file.read()
        content = regex.sub(new_str, content)
        current_str = new_str
        with open(file, 'w') as out_file:
            out_file.write(content)

    random.shuffle(enumerated_programs)
    for i, program in enumerated_programs:
        start = time.time()
        out = subprocess.run(program, capture_output=True)
        times[i, j] = (time.time() - start)*1000
        if out.returncode != 0:
            print(f'{program_names[i]} failed to execute')
            exit()
        if out.stdout.decode("utf-8") != new_str + '\n':
            print(f'{program_names[i]} gave wrong output of {out.stdout}')
            exit()

for i, name in enumerate(program_names):
    print(f'{name}:')
    print(f'\tMean: {np.mean(times[i])} ms')
    print(f'\tStddev: {np.std(times[i])} ms')
    print(f'\tStderr: {np.std(times[i])/(N**0.5)} ms')

# Reset files
for file in program_files:
    with open(file, 'r') as in_file:
        content = in_file.read()
    content = regex.sub('Hello, World!', content)
    with open(file, 'w') as out_file:
        out_file.write(content)
