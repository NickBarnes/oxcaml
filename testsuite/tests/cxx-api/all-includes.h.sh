#!/bin/sh

set -eu

for lib in "$ocamlsrcdir"/runtime "$ocamlsrcdir"/otherlibs/*/; do
  for file in "$lib"/caml/*.h; do
    header="$(basename "$file")"
    if [ "$header" != "jumptbl.h" ] && [ -e "$file" ]; then
      echo "#include <caml/$header>" >> "$test_build_directory"/all-includes.h
    fi
  done
done
