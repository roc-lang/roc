# A fork of `kcov` for better covering Zig

This is a rather than naive fork of wonderful `kcov` for better supporting Zig projects.

## It adds the ability to auto recognize Zig's `unreachable` and `@panic` for auto ignoring.

Here is a screenshot of auto ignoring `unreachable`.

![zig-unreachable-example](https://github.com/liyu1981/kcov/blob/master/nocover/zig-unreachable.png?raw=true)

Here is another screenshot of auto ignoring `@panic`.

![zig-panic-example](https://github.com/liyu1981/kcov/blob/master/nocover/zig-panic.png?raw=true)

## It also adds the ability in c/c++ source file of using `/* no-cover */` to mark a line to be ignored.

Here is a screenshot of manual ignoring by placing `/* no-cover */` in c source file.

![c-no-cover example](https://github.com/liyu1981/kcov/blob/master/nocover/c-nocover.png?raw=true)

## Usage

There is no change of original `kcov` usage, it will just work. Please follow the below original Kcov README.

### But how can I get the binary?

The best way is to compile from source. It can be done as follows (you will need `cmake`, `ninja`, `llvm@>16` as I tried)

```
git clone https://github.com/liyu1981/kcov.git
cd kcov
mkdir build
cd build
CC="clang" CXX="clang++" cmake -G Ninja ..
ninja
```

after building is done. The binary is at `build/src/kcov`. Copy somewhere and use it.

(Or you can download a copy of Apple Silicon version binary from the release section.)

### But can I build it with `zig`?

There is now an experimental `build.zig` file, and you can use `zig build sign_kcov` to build on Macos with M1/M2/M3 chip. The built result will be at `zig-out/bin/kcov`, and it will work as same to the one compiled from CMake. (need this `sign_kcov` step because macos needs to sign before use for debugging purpose, which is how `kcov` underlying works).

I am planning to complete this `build.zig` in future for `Linux`/`Windows` on `x86_64` or even `arm/arm64`, but it needs some time.

## File Support

- for Zig, support source file with `.zig` extension.
- for C/C++, support source file with `.c/.cpp/.cc` extension.

## Original Kcov Readme

[![Coveralls coverage status](https://img.shields.io/coveralls/SimonKagstrom/kcov.svg)](https://coveralls.io/r/SimonKagstrom/kcov?branch=master)
[![Codecov coverage status](https://codecov.io/gh/SimonKagstrom/kcov/branch/master/graph/badge.svg)](https://codecov.io/gh/SimonKagstrom/kcov)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/2844/badge.svg)](https://scan.coverity.com/projects/2844)
![Docker Pulls](https://img.shields.io/docker/pulls/kcov/kcov.svg)

[![PayPal Donate](https://img.shields.io/badge/paypal-donate-blue.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=simon.kagstrom%40gmail%2ecom&lc=US&item_name=Simon%20Kagstrom&item_number=kcov&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted) [![Github All Releases](https://img.shields.io/github/downloads/atom/atom/total.svg)](https://github.com/SimonKagstrom/kcov/)

## _kcov_

Kcov is a FreeBSD/Linux/OSX code coverage tester for compiled languages, Python
and Bash. Kcov was originally a fork of [Bcov](http://bcov.sf.net), but has
since evolved to support a large feature set in addition to that of Bcov.

Kcov, like Bcov, uses DWARF debugging information for compiled programs to
make it possible to collect coverage information without special compiler
switches.

For a video introduction, [look at this presentation from SwedenCPP](https://www.youtube.com/watch?v=1QMHbp5LUKg)

## Installing

Refer to the [INSTALL](INSTALL.md) file for build instructions, or use our official Docker images:

- [kcov/kcov](https://hub.docker.com/r/kcov/kcov/) for releases since v31.

## How to use it

Basic usage is straight-forward:

```
kcov /path/to/outdir executable [args for the executable]
```

_/path/to/outdir_ will contain lcov-style HTML output generated
continuously while the application runs. Kcov will also write cobertura-
compatible XML output and generic JSON coverage information and can easily
be integrated in various CI systems.

## Filtering output

It's often useful to filter output, since e.g., /usr/include is seldom of interest.
This can be done in two ways:

```
kcov --exclude-pattern=/usr/include --include-pattern=part/of/path,other/path \
      /path/to/outdir executable
```

which will do a string-comparison and include everything which contains
_part/of/path_ or _other/path_ but exclude everything that has the
_/usr/include_ string in it.

```
kcov --include-path=/my/src/path /path/to/outdir executable
kcov --exclude-path=/usr/include /path/to/outdir executable
```

Does the same thing, but with proper path lookups.

## Merging multiple kcov runs

Kcov can also merge the results of multiple earlier runs. To use this mode,
call kcov with `--merge`, an output path and one or more paths to an earlier
run, e.g.,

```
kcov --merge /tmp/merged-output /tmp/kcov-output1 /tmp/kcov-output2
kcov --merge /tmp/merged-output /tmp/kcov-output*    # With a wildcard
```

## Use from continuous integration systems

kcov is easy to integrate with [travis-ci](http://travis-ci.org) together with
[coveralls.io](http://coveralls.io) or [codecov.io](http://codecov.io). It can also
be used from Jenkins, [SonarQube](http://sonarqube.org) and [GitLab CI](http://gitlab.com).
Refer to

- [coveralls](doc/coveralls.md) for details about travis-ci + coveralls, or
- [codecov](doc/codecov.md) for details about travis-ci + codecov
- [jenkins](doc/jenkins.md) for details about how to integrate in Jenkins
- [sonarqube](doc/sonarqube.md) for how to use kcov and sonarqube together
- [gitlab](doc/gitlab.md) for use with GitLab

## More information

kcov is written by Simon Kagstrom <simon.kagstrom@gmail.com> and more
information can be found at [the web page](http://simonkagstrom.github.io/kcov/index.html)
