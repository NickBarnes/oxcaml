#!/bin/sh

set -eu

if ! command -v jq >/dev/null 2>&1; then
  echo "jq could not be found" >> "$ocamltest_response"
  exit "$TEST_SKIP"
fi

case "$ccomp_type" in
  cc)
    gnuc=$(echo '__GNUC__' | $cxx -E -P -)

    if [ "$gnuc" -lt 13 ]; then
      echo "GCC 13 or later is required for SARIF diagnostics" \
           >> "$ocamltest_response"
      exit "$TEST_SKIP"
    fi
    ;;
  msvc)
    if $cxx '/?' | head -n1 | grep -q 'clang LLVM compiler'; then
      echo "MSVC (not clang-cl) is required for SARIF diagnostics" \
           >> "$ocamltest_response"
      exit "$TEST_SKIP"
    fi
    ;;
  *)
    echo "Unknown ccomp_type $ccomp_type" >> "$ocamltest_response"
    exit "$TEST_SKIP"
    ;;
esac

exit "$TEST_PASS"
