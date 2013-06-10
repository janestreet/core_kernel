#!/bin/sh

# Detect supported features and put the result in setup.data

set -e

if ld -lm -shared --wrap caml_modify -o /dev/null 2>/dev/null; then
    ld_wrap_possible=true
else
    ld_wrap_possible=false
fi

if [ -e setup.data ]; then
    sed '/^ld_wrap_possible=/d' setup.data > setup.data.new
    mv setup.data.new setup.data
fi

cat >> setup.data <<EOF
ld_wrap_possible="$ld_wrap_possible"
EOF
