#!/bin/sh

set -e

if [ $# -lt 2 ]; then
    echo "Usage: discover.sh OCAMLC CONFIG_H" >&2
    exit 2
fi

OCAMLC="$1"
CONFIG_H="$2"
shift 2

SRC=test.c
OUT=test.out
trap "rm -f $OUT" EXIT

sentinel="CORE_`basename "$CONFIG_H" | tr a-z. A-Z_`"
cat > $OUT  <<EOF
#ifndef $sentinel
#define $sentinel
EOF

$OCAMLC -ccopt -E -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' >> $OUT

cat >> $OUT <<EOF
#define JSC_POSIX_TIMERS
#undef JSC_ARCH_BIG_ENDIAN
EOF

cat >> $OUT <<EOF
#endif
EOF

mv $OUT "$CONFIG_H"
