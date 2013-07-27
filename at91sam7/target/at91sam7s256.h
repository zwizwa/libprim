/* Linker script configuration. */
#define FLASH_START 0x00100000
#define FLASH_SIZE  0x40000

#define RAM_START   0x00200000
#define RAM_SIZE    0x10000

#define STACK_END    0x20FFFC
#define STACK_BEGIN  0x20F000

/* C defs for target. */
#ifndef JUST_LINKER_DEFS

#ifdef __ASSEMBLY__
// for crt0.S
#else
#include "atmel/AT91SAM7S256.h"
#endif

/* Datasheet section 10.4 PIO Controller A Multiplexing */
#define PIOA_DBGU_TX (1<<10)
#define PIOA_DBGU_RX (1<<9)

#endif

