/* Linker script configuration for TARGET=at91sam7s256 */
#define FLASH_START 0x00100000
#define FLASH_SIZE  0x40000

#define RAM_START   0x00200000
#define RAM_SIZE    0x10000

#define STACK_END    0x20FFFC
#define STACK_BEGIN  0x20F000

/* C defs for target. */
#ifndef JUST_LINKER_DEFS
#include "atmel/AT91SAM7X256.h"
#endif
