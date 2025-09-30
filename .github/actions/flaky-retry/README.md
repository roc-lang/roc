# Flaky Retry Action

A GitHub Action that retries a command if it fails with a specific error string.

## Inputs

- `command` (required): The command to run
- `error_string_contains` (required): Retry only if error output contains this string
- `retry_count` (optional): Number of times to retry on failure (default: 3)

## Usage

```yaml
- name: Run flaky test
  uses: ./.github/actions/flaky-retry
  with:
    command: 'cargo test my_flaky_test'
    error_string_contains: 'connection timeout'
    retry_count: 5
```

## Behavior

- Runs the specified command
- If the command succeeds (exit code 0), the action succeeds immediately
- If the command fails (non-zero exit code):
  - Checks if the error output contains the specified string
  - If it does, retries up to `retry_count` times
  - If it doesn't, fails immediately without retrying
- After all retries are exhausted, the action fails with the last exit code