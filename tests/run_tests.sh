#!/usr/bin/env bash
# Test runner for the Agent Workflow Language interpreter.
#
# Each tests/*.awl file may begin with directive comments:
#   // EXPECT_OK              -- program must exit 0 and print [ok]
#   // EXPECT_ERR: <substr>   -- program must exit non-zero, print [error],
#                                and the error message must contain <substr>
#   // CHECK: <substr>        -- the full stdout must contain <substr>
#   // CHECK_STDOUT: <substr> -- alias for CHECK
#   // NOT: <substr>          -- the full stdout must NOT contain <substr>

set -o pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
AWL="$(cabal list-bin awl 2>/dev/null)"
if [[ -z "$AWL" || ! -x "$AWL" ]]; then
  echo "could not locate awl binary; run 'cabal build' first" >&2
  exit 2
fi

TESTS=("$ROOT"/tests/*.awl)
pass=0
fail=0
failed_names=()

for t in "${TESTS[@]}"; do
  name=$(basename "$t")

  expect_ok=0
  expect_err=""
  checks=()
  nots=()

  while IFS= read -r line; do
    case "$line" in
      "// EXPECT_OK"*)        expect_ok=1 ;;
      "// EXPECT_ERR: "*)     expect_err="${line#// EXPECT_ERR: }" ;;
      "// CHECK: "*)          checks+=("${line#// CHECK: }") ;;
      "// CHECK_STDOUT: "*)   checks+=("${line#// CHECK_STDOUT: }") ;;
      "// NOT: "*)            nots+=("${line#// NOT: }") ;;
    esac
  done < "$t"

  out=$("$AWL" "$t" 2>&1)
  rc=$?

  problems=()

  if [[ $expect_ok -eq 1 ]]; then
    [[ $rc -eq 0 ]] || problems+=("expected exit 0, got $rc")
    [[ "$out" == *"[ok]"* ]] || problems+=("missing [ok] marker")
  fi

  if [[ -n "$expect_err" ]]; then
    [[ $rc -ne 0 ]] || problems+=("expected non-zero exit")
    [[ "$out" == *"[error]"* ]] || problems+=("missing [error] marker")
    [[ "$out" == *"$expect_err"* ]] || problems+=("error did not contain '$expect_err'")
  fi

  for c in "${checks[@]}"; do
    [[ "$out" == *"$c"* ]] || problems+=("stdout missing '$c'")
  done
  for n in "${nots[@]}"; do
    [[ "$out" != *"$n"* ]] || problems+=("stdout unexpectedly contains '$n'")
  done

  if [[ ${#problems[@]} -eq 0 ]]; then
    printf "  ok   %s\n" "$name"
    pass=$((pass + 1))
  else
    printf "  FAIL %s\n" "$name"
    for p in "${problems[@]}"; do printf "       - %s\n" "$p"; done
    printf "       --- output ---\n%s\n       --------------\n" "$out"
    fail=$((fail + 1))
    failed_names+=("$name")
  fi
done

total=$((pass + fail))
echo
echo "passed: $pass / $total"
if [[ $fail -gt 0 ]]; then
  echo "failed: ${failed_names[*]}"
  exit 1
fi
