#define _GNU_SOURCE
#include <dlfcn.h>
#include <link.h>
#include <stdio.h>
#include <mz/elfhacks.h>
 
// For info see:
//  - the header files in /usr/include/elf.h
//  - http://nullkey.ath.cx/git/elfhacks
//
// ElfW(Phdr) :

// typedef struct
// {
//   Elf32_Word	p_type;			/* Segment type */
//   Elf32_Off	p_offset;		/* Segment file offset */
//   Elf32_Addr	p_vaddr;		/* Segment virtual address */
//   Elf32_Addr	p_paddr;		/* Segment physical address */
//   Elf32_Word	p_filesz;		/* Segment size in file */
//   Elf32_Word	p_memsz;		/* Segment size in memory */
//   Elf32_Word	p_flags;		/* Segment flags */
//   Elf32_Word	p_align;		/* Segment alignment */
// } Elf32_Phdr;


/* Legal values for p_type (segment type).  */

// #define PT_NULL	0		/* Program header table entry unused */
// #define PT_LOAD	1		/* Loadable program segment */
// #define PT_DYNAMIC	2		/* Dynamic linking information */
// #define PT_INTERP	3		/* Program interpreter */
// #define PT_NOTE	4		/* Auxiliary information */
// #define PT_SHLIB	5		/* Reserved */
// #define PT_PHDR	6		/* Entry for header table itself */
// #define PT_TLS	7		/* Thread-local storage segment */
// #define PT_NUM	8		/* Number of defined types */
// #define PT_LOOS	0x60000000	/* Start of OS-specific */
// #define PT_GNU_EH_FRAME	0x6474e550	/* GCC .eh_frame_hdr segment */
// #define PT_GNU_STACK	0x6474e551	/* Indicates stack executability */
// #define PT_GNU_RELRO	0x6474e552	/* Read-only after relocation */
// #define PT_LOSUNW	0x6ffffffa
// #define PT_SUNWBSS	0x6ffffffa	/* Sun Specific segment */
// #define PT_SUNWSTACK	0x6ffffffb	/* Stack segment */
// #define PT_HISUNW	0x6fffffff
// #define PT_HIOS	0x6fffffff	/* End of OS-specific */
// #define PT_LOPROC	0x70000000	/* Start of processor-specific */
// #define PT_HIPROC	0x7fffffff	/* End of processor-specific */

static int
callback(struct dl_phdr_info *info, size_t size, void *data) {
    int j;
    printf("name=%s (%d segments)\n", info->dlpi_name,
        info->dlpi_phnum);
    for (j = 0; j < info->dlpi_phnum; j++) {
        if (info->dlpi_phdr[j].p_type == PT_DYNAMIC) {
            printf("\t\t header %2d: type=%d, address=%10p\n", j, info->dlpi_phdr[j].p_type,
                   (void *) (info->dlpi_addr + info->dlpi_phdr[j].p_vaddr));
        }
    }
    return 0;
}

int eh_syms(eh_sym_t *sym, void *arg) {
    printf(" %s\n",  sym->name);
}
int eh_objs(eh_obj_t *obj, void *arg) {
    eh_iterate_sym(obj, eh_syms, NULL);
}

int main(int argc, char **argv) {
    eh_obj_t obj;
    eh_find_obj(&obj, "libm.so.6");
    eh_iterate_sym(&obj, eh_syms, NULL);
    // eh_iterate_obj(eh_objs, NULL);

    // void *lib = dlopen(argv[1], RTLD_NOW);
    // dl_iterate_phdr(callback, NULL);
    return 0;
}
