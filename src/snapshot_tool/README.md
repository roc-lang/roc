# Snapshot Tool

This directory contains a tool for snapshot testing the Roc compiler.

Snapshot testing is a method used to verify the behavior of the compiler's different stages. The tool generates "golden snapshot" files, which are baseline outputs that are known to be correct.

During testing, the tool runs the compiler and compares its output against these golden files. If there are any differences, the test fails. This is an effective way to detect regressions and unintended changes in the compiler's behavior across a large number of test cases.

The golden snapshots are committed to the repository and are therefore tracked by Git and checked along with any changes to the codebase.
