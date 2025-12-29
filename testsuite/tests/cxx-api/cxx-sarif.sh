#!/bin/sh

set -eu

# Try to build a C++ file including all of OCaml headers, and using a
# few functions from its API.
#
# Ask for a wide set of warnings from the C++ compiler, but explicitly
# ignore some warnings about C features that are not valid in C++ (but
# commonly supported as extensions), such as:
#
# - C99 flexible array member (FAM)
#   + https://en.wikipedia.org/wiki/Flexible_array_member
#   + https://learn.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warning-levels-2-and-4-c4200
#
# The compiler's diagnostics are produced as SARIF, because it's
# easier to parse, and use jq to filter out errors we can safely
# ignore.

case "$ccomp_type" in
  cc)
    # TODO: use GCC 15 -fdiagnostics-add-output for stderr and SARIF
    # output sinks
    if $cxx -Wall -Wextra -Wpedantic \
            -fdiagnostics-format=sarif-stderr \
            $cppflags $cflags \
            -I "$ocamlsrcdir"/runtime \
            -I "$ocamlsrcdir"/otherlibs/runtime_events \
            -I "$ocamlsrcdir"/otherlibs/str \
            -I "$ocamlsrcdir"/otherlibs/systhreads \
            -I "$ocamlsrcdir"/otherlibs/unix \
            -I "$test_build_directory" \
            -o "$test_build_directory"/stubs.o \
            -c "$test_source_directory"/stubs.cpp \
            2> stubs.sarif; then
      exit "$TEST_PASS"
    else
      cat <<'EOF' > filter.jq
.runs.[0].results.[]
| select(.message.text
        # Allow flexible array member
        | contains("ISO C++ forbids flexible array member") | not)
| length
EOF
    fi
    ;;
  msvc)
    if $cxx -W2 -permissive- \
            -experimental:log stubs \
            $cppflags $cflags \
            -I "$ocamlsrcdir"\\runtime \
            -I "$ocamlsrcdir"\\otherlibs\\runtime_events \
            -I "$ocamlsrcdir"\\otherlibs\\str \
            -I "$ocamlsrcdir"\\otherlibs\\systhreads \
            -I "$ocamlsrcdir"\\otherlibs\\unix \
            -I "$test_build_directory" \
            -Fo"$test_build_directory"\\stubs.obj \
            -c "$test_source_directory"\\stubs.cpp \
            >/dev/null 2>&1; then
      exit "$TEST_PASS"
    else
      cat <<'EOF' > filter.jq
.runs.[0].results.[]
| select(.ruleId
        # Allow flexible array member
        | contains("C4200") | not)
| length
EOF
    fi
    ;;
esac

length=$(jq -f filter.jq stubs.sarif)
if [ -z "$length" ]; then
  exit "$TEST_PASS"
fi

# pretty-print
jq . stubs.sarif > stubs.sarif.tmp
mv stubs.sarif.tmp stubs.sarif

if [ "${GITHUB_ACTIONS:-false}" = true ]; then
  echo
  echo "::group::stubs.sarif content ($(wc -l stubs.sarif) lines)"
  cat stubs.sarif
  echo
  echo '::endgroup::'
elif [ -n "${CI+true}" ]; then
  cat stubs.sarif
fi
echo "$cxx reported errors at $test_build_directory/stubs.sarif" \
     >> "$ocamltest_response"
exit "$TEST_FAIL"
