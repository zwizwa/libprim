#!/bin/bash
PF=$(readlink -f $(dirname $0)/../build/pf/pf.elf)
if [ ! -x $PF ]; then
    echo "can't find $PF"
    exit 1
fi
CMD=$(tempfile -p gdbpf)
cat <<EOF >$CMD
run
EOF
gdb -x $CMD --args $PF "$@"
rm -f $CMD
