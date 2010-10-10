#ifndef _CELL_READ_H_
#define _CELL_READ_H_

#include <leaf/port.h>
#include <cell/vm.h>

cell *vm_read_stdin(vm *vm);
cell *vm_read(vm *vm, port *port);
#endif

