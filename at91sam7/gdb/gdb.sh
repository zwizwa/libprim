#!/bin/bash
GDB=arm-eabi-gdb-7.2  # YMMV
exec $GDB -x $(dirname $0)/util.gdb -x $(dirname $0)/init.gdb "$@"
