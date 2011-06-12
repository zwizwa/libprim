all: sam7s.elf

clean:
	rm -rf *~ *.o *.elf

CC = arm-eabi-gcc  -mcpu=arm7tdmi -O0 -g
LD = arm-eabi-gcc  -mno-thumb-interwork -mcpu=arm7tdmi -Wl,-static -g -nostdlib
OBJDUMP = arm-eabi-objdump

%.o:	%.S
	$(CC) -o $@ -c $<

%.elf:	%.o %.ld
	$(LD) -T$*.ld -o $@ $*.o

%.dump: %.elf
	$(OBJDUMP) -h $< >$@
	cat $@

