#!/bin/sh

set -e

if [ $# -lt 3 ]; then
    echo "Usage: discover.sh OCAMLC CONFIG_MLH CONFIG_H" >&2
    exit 2
fi

OCAMLC="$1"
CONFIG_MLH="$2"
CONFIG_H="$3"
shift 3

SRC=test.c
OUT=test.out
trap "rm -f $OUT" EXIT

$OCAMLC -ccopt -E -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' > $OUT

cat >> $OUT <<EOF
#let JSC_POSIX_TIMERS = true
#let JSC_ARCH_BIG_ENDIAN = false
EOF

mv $OUT "$CONFIG_MLH"

{
    sentinel="CORE_`basename "$CONFIG_H" | tr a-z. A-Z_`"
    cat  <<EOF
#ifndef $sentinel
#define $sentinel
EOF
    sed -r 's|^#let *([A-Za-z0-9_]+) *= *true *$|#define \1|;
            s|^#let *([A-Za-z0-9_]+) *= *false *$|/* #define \1 */|' "$CONFIG_MLH"
    cat  <<EOF
#endif
EOF
} > $OUT
mv $OUT "$CONFIG_H"
