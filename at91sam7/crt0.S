#define __ASSEMBLY__
#include TARGET_H
.global _reset0  /* Entry point */
.global _reset1  /* C Entry point */
        
/* The .vectors segment is placed at 0x100000 == ARM vector table. */
.section .vectors, "a"
.arm
.align
_v_reset:       b _reset0     /* Table at http://www.ethernut.de/en/documents/arm-exceptions.html */
_v_undef:       b _undef      /* Undefined instruction */
_v_swi:         b _swi        /* Software interrupt */
_v_pabt:        b _pabt       /* Prefetch abort */
_v_dabt:        b _dabt       /* Data abort */
_v_rsv:         nop           /* Reserved */
_v_irq:         b _irq        /* Interrupt Request */
_v_fiq:         b _fiq        /* Fast Interrupt Request */


.text
.arm
.align        

/* FIXME: disabled */
_undef:
_swi:
_pabt:
_dabt:
_fiq:
1:              b 1b


        /* It's probably better to use vectored interrupts, which enables
        faster handling of high-priority tasks.
        
_irq:   ldr pc, [pc, #-&F20] */
        
_irq:
        /* The AIC sits inbetween interrupt sources and the ARM, which
	has only 2 interrupt lines: nFIQ and nIRQ.  The routine here
	is for AIC -> nIRQ handling. */
        
        /* AIC allows priority-based nested interrupts, so save lr on
	the stack.  We need to also save SPSR (copy of CPSR).
        PC needs to be decremented by 4.  The real reason is probably
	a legacy issue related to the original ARM's architecture so
	let's not try to explain why. */
        sub     lr, lr, #4
        stmfd   sp!, {lr}
        mrs     lr, SPSR
        stmfd   sp!, {r0, rl}

        /* Read of IVR signals entry point of interrupt handling to
	AIC.  Write in the IVR to support Protect Mode */
        ldr     lr, =AT91C_BASE_AIC
        ldr     r0, [r14, #AIC_IVR]
        str     lr, [r14, #AIC_IVR]

        /* The interrupt handler is executed in Supervisor mode with
	ARM IRQ enabled, so the AIC can interrupt the ARM when a
	higher-priority interrupt occurs. */
        msr     CPSR_c, #ARM_MODE_SVC

        /* Call ISR routine, saving/restoring some context. */
        stmfd   sp!, {r1-r3, r4, r12, lr}
        mov     lr, pc
        bx      r0
        ldmia   sp!, {r1-r3, r4, r12, lr}

        /* Switch back to interrupt mode with interrupts disabled to
	finish up. */
        msr     CPSR_c, #ARM_MODE_IRQ | I_BIT

        /* Write to AIC_EIOCR signals AIC that this interrupt has been
	handled, such that interrupts of the same priority level can
	be singnalled on nIRQ again. */
        ldr     lr, =AT91C_BASE_AIC
        str     lr, [r14, #AIC_EOICR]

        /* Restore context. */
        ldmia   sp!, {r0, lr}
        msr     SPSR_cxsf, lr
        ldmia   sp!, {pc}^
        
        
        

 /* Standard definitions of Mode bits and Interrupt (I & F) flags in PSRs (program status registers) */
.set  ARM_MODE_USR, 0x10   /* Normal User Mode */
.set  ARM_MODE_FIQ, 0x11   /* FIQ Processing Fast Interrupts Mode */
.set  ARM_MODE_IRQ, 0x12   /* IRQ Processing Standard Interrupts Mode */
.set  ARM_MODE_SVC, 0x13   /* Supervisor Processing Software Interrupts Mode */
.set  ARM_MODE_ABT, 0x17   /* Abort Processing memory Faults Mode */
.set  ARM_MODE_UND, 0x1B   /* Undefined Processing Undefined Instructions Mode */
.set  ARM_MODE_SYS, 0x1F   /* System Running Priviledged Operating System Tasks  Mode */
.set  I_BIT, 0x80          /* when I bit is set, IRQ is disabled (program status registers) */
.set  F_BIT, 0x40          /* when F bit is set, FIQ is disabled (program status registers) */
       
        
        /* Minimal setup so we can jump into C code.  Each mode has
	banked registers which include a separate stack pointer.
        FIQ: R8-R14,
        Supervisor, Abort, IRQ, Undefined: R13(sp), R14(lr) */
        .macro alloc_stack mode ptr size
        msr CPSR_c, \mode | I_BIT | F_BIT
        mov sp, \ptr
        sub \ptr, \ptr, #\size
        .endm
        
_reset0:
        ldr r0, =_stack_end
        alloc_stack ARM_MODE_UND r0 0x10
        alloc_stack ARM_MODE_ABT r0 0x10
        alloc_stack ARM_MODE_SVC r0 0x10
        alloc_stack ARM_MODE_FIQ r0 0x100
        alloc_stack ARM_MODE_IRQ r0 0x400
        alloc_stack ARM_MODE_SYS r0 -1 /* Don't care: rest is for us */

	/* copy initialized variables .data section  (Copy from ROM to RAM) */
        ldr     R1, =_etext
        ldr     R2, =_data
        ldr     R3, =_edata
1:      cmp     R2, R3
        ldrlo   R0, [R1], #4
        strlo   R0, [R2], #4
        blo     1b

	/* Clear uninitialized variables .bss section (Zero init)  */
        mov     R0, #0
        ldr     R1, =_bss_start
        ldr     R2, =_bss_end
2:	cmp     R1, R2
        strlo   R0, [R1], #4
        blo     2b
        
        b _reset1
.end

        