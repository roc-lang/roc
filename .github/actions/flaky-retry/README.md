# Flaky Retry Action

A GitHub Action that retries a command if it fails with a specific error string.

## Inputs

- `command` (required): The command to run
- `error_string_contains` (required): Retry only if error output contains this string
- `retry_count` (optional): Number of times to retry on failure (default: 3)
- `heartbeat_seconds` (optional): Interval between reports of the running Zig and Roc processes and their resident memory (default: 300)

## Usage

```yaml
- name: Run flaky test
  uses: ./.github/actions/flaky-retry
  with:
    command: 'zig build my_flaky_cmd'
    error_string_contains: 'connection timeout'
    retry_count: 5
```

## Behavior

- Runs the specified command, copying its combined output to the log as it is written
- While the command runs, reports every `heartbeat_seconds` the Zig compilers, build runners, test binaries and Roc binaries that are running, with their resident memory
- If the command succeeds (exit code 0), the action succeeds immediately
- If the command fails (non-zero exit code):
  - Checks if the error output contains the specified string
  - If it does, retries up to `retry_count` times
  - If it doesn't, fails immediately without retrying
- After all retries are exhausted, the action fails with the last exit code