#!/bin/sh

set -e

if [ $# -lt 2 ]; then
    echo "Usage: discover.sh OCAMLC ML_OUTFILE C_OUTFILE" >&2
    exit 2
fi

OCAMLC="$1"
ML_OUTFILE="$2"
C_OUTFILE="$3"
shift 3

OCAML_CFLAGS=
. ./setup.data
[ "$posix_timers" = true ] && OCAML_CFLAGS="$OCAML_CFLAGS -ccopt -DPOSIX_TIMERS"

SRC=config/test.c
OUT=config/test.out
trap "rm -f $OUT" EXIT

$OCAMLC -ccopt -E $OCAML_CFLAGS -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' | tee > $OUT

mv "$OUT" "$ML_OUTFILE"

{
    sentinel="CORE_`basename "$C_OUTFILE" | tr a-z. A-Z_`"
    cat  <<EOF
#ifndef $sentinel
#define $sentinel
EOF
    sed 's|^#let *\([A-Za-z_0-9]*\) *= *true *$|#define \1|;
         s|^#let *\([A-Za-z_0-9]*\) *= *false *$|#undef \1|' "$ML_OUTFILE"
    cat  <<EOF
#endif /* $sentinel */
EOF
} > "$C_OUTFILE"
