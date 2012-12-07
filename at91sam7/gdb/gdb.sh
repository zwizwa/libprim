#!/bin/bash
# GDB=arm-eabi-gdb-7.2
# GDB=/opt/xc/gnutools/bin/arm-none-eabi-gdb
GDB=/opt/xc/sat/bin/arm-none-eabi-gdb
exec $GDB -x $(dirname $0)/util.gdb -x $(dirname $0)/init.gdb "$@"
