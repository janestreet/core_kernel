#!/bin/sh
if ld -lrt -shared -o /dev/null 2>/dev/null; then
    echo "(rt)"
else
    echo "()"
fi
