# -*- conf -*-

define reconnect
       disconnect
       connect
end

## Some assembler stepping commands.  Note that the names for these
## commands are chosen so they can be invoked with their first 2
## characters.

# Run upto a certain code point using a temporary breakpoint.
# Remember that code addresses can be used as: "go *0x11FEDC"
define go 
       thbreak $arg0
       continue
end

# Step over the next instruction using a forced breakpoint, i.e. for
# conditional jumps that are part of loops.  This doesn't do the same
# as nexti which only steps over function calls, but will follow
# jumps.
define bnexti
       go *($pc+4)
end       

# Disassemble code surrounding the PC using the `x' command.  This
# doesn't require anything from the symbol table as is the case for
# the `disassemble' command.  A bit of context before the PC is nice,
# i.e. in the case of returning from a procedure call, or seeing if a
# conditional branch was not taken.  The default `x' command prints an
# arrow at the current PC, but it's hard to sync your eyes to just the
# arrow, so we print a separator that easier to track visually.
define dpc
       info registers
       x/2i $pc-8
       echo ---\n
       x/8i $pc
end

define dstepi
       stepi
       dpc
end

define dnexti
       nexti
       dpc
end

define dgo
       go $arg0
       dpc
end

define dbnexti
       bnexti
       dpc
end

# Some notes about ARM assembly.  Sometimes bugs only show up in
# optimized builds, so it's good to know a minimal of ASM to follow
# control flow.
#
# - The ARM has 16 working registers.
#
# - Calling conventions:
#  
#   r0-r3   arguments + return value
#   r4-r11  local variables (callee must save)
#   r12     scratch
#   r13     sp
#   r14     lr
#   r15     pc
#
# - BL performs a procedure call.  The LR is the link register.  It
#   contains the return address of a procedure call.
#
# - B performs an unconditional jump.
#
# - BNE performs a conditional jump (not equal).
#
# - MOV is inter-register move, STR and LDR are the store and load
#   instructions between registers and memory.
#
# - For arithmetic and logic ops, the destination register is on the
#   left.
#
# - The S suffix on operations updates the condition flags.
#
# - For the rest, refer to the quick ref cheat sheet:
#   http://infocenter.arm.com/help/topic/com.arm.doc.qrc0001m/QRC0001_UAL.pdf


# This is just annoying..
set confirm off


# OpenOCD specific
define connect
       target remote localhost:3333
end

define reset
       mon adapter_khz 100
       mon reset init
end

