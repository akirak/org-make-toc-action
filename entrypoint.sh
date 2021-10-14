#!/bin/bash
set -euo pipefail
shopt -s extglob
shopt -s globstar
shopt -s nullglob

exitcode=0

for file in $(echo "$*" | tr , \\n); do
    if [[ -f "$file" ]]; then
        if ! /bin/org-make-toc "$file"; then
            exitcode=1
        fi
    fi
done

exit $exitcode
