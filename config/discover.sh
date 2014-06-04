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

SRC=config/test.c
OUT=config/test.out
trap "rm -f $OUT" EXIT

$OCAMLC -ccopt -E $OCAML_CFLAGS -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' | tee > $OUT

OCAML_VERSION="`ocamlc -version`"
case "$OCAML_VERSION" in
    4.0[1-9]*|4.[1-9]*)
        echo "DEFINE OCAML_4"    >> $OUT
        echo "DEFINE OCAML_4_01" >> $OUT
        ;;
    4*)
        echo "DEFINE OCAML_4" >> $OUT
        ;;
esac

mv "$OUT" "$ML_OUTFILE"


{
    sentinel="CORE_`basename "$C_OUTFILE" | tr a-z. A-Z_`"
    cat  <<EOF
#ifndef $sentinel
#define $sentinel
EOF
    sed 's/^DEFINE */#define JSC_/' "$ML_OUTFILE"
    cat  <<EOF
#endif /* $sentinel */
EOF
} > "$C_OUTFILE"
