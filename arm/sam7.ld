/* Starting from scratch to see how this actually works:
   http://www.delorie.com/gnu/docs/binutils/ld_9.html */
     
SECTIONS
{
  /* ROM */        
  . = 0x100000;
  .text : { *(.text) }

  /* RAM */
  . = 0x200000;
  .data : { *(.data) }
  .bss : { *(.bss) }
}

