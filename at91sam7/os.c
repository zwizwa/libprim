#include <stdio.h>

#include "target.h"

/* CONFIG */
#define MCK 18432000   // Set to Main Oscillator in init_osc()
#define BAUD  115200

/* Hardware */

void dbgu_write(int c) {
    while(!(AT91C_BASE_DBGU->DBGU_CSR & AT91C_US_TXRDY));
    AT91C_BASE_DBGU->DBGU_THR = c;
}
int dbgu_read(void) {
    while(!(AT91C_BASE_DBGU->DBGU_CSR & AT91C_US_RXRDY));
    return AT91C_BASE_DBGU->DBGU_RHR & 0xFF;
}
typedef unsigned long u32;

#if 0
void toggle_pioa(u32 mask) {
    /* Enable peripheral clock. */
    // AT91C_BASE_PMC->PMC_PCER = PID_PIOA;
    AT91C_BASE_PIOA->PIO_PER = mask;
    AT91C_BASE_PIOA->PIO_OER = mask;
    while(1) {
        AT91C_BASE_PIOA->PIO_SODR = mask; // set
        AT91C_BASE_PIOA->PIO_CODR = mask; // clear
    }
}
#endif

/* 10.4 multiplexing.  DBGU is on peripheral A. */
#define PIOA_DBGU_TX (1<<10)
#define PIOA_DBGU_RX (1<<9)

/* 10.2 Peripheral identifiers */
#define PID_SYSC (1<<1)
#define PID_PIOA (1<<2)

void init_dbgu(void) {
    /* Reset */
    AT91C_BASE_DBGU->DBGU_CR = AT91C_US_RSTRX | AT91C_US_RSTTX;

    /* Set Baud rate, reset and enable. (section 26) */
    AT91C_BASE_DBGU->DBGU_BRGR = MCK / (16 * BAUD);
    AT91C_BASE_DBGU->DBGU_MR = AT91C_US_PAR_NONE;

    /* Enable */
    AT91C_BASE_DBGU->DBGU_CR = AT91C_US_RXEN | AT91C_US_TXEN;

    /* Setup PIO multiplexing. */
    AT91C_BASE_PIOA->PIO_PDR = PIOA_DBGU_TX | PIOA_DBGU_RX;
    AT91C_BASE_PIOA->PIO_ASR = PIOA_DBGU_TX | PIOA_DBGU_RX;
}

void init_first(void) {
    // Set Flash Wait sate: 0 wait states.
    AT91C_BASE_MC->MC_FMR = ((AT91C_MC_FMCN)&(22 <<16)) | AT91C_MC_FWS_0FWS;

    // Disable watchdog.
    AT91C_BASE_WDTC->WDTC_WDMR= AT91C_WDTC_WDDIS;
}

void init_osc(void) {

    /* Start up main oscillator, 512 slow clock startup.  Note that 64
       is probably enough (according to eCos source code, info by Atmel). */
    AT91C_BASE_CKGR->CKGR_MOR = ((AT91C_CKGR_OSCOUNT & (0x40U << 8)) | AT91C_CKGR_MOSCEN);
    while(!(AT91C_BASE_PMC->PMC_SR & AT91C_PMC_MOSCS));

    /* Select master clock (MCK) = Main oscillator.  See section 25.7
       in the datasheet.  The order of these is important: first set
       clock source (CSS) to main clock, then set CPU clock prescaler
       (PRES).  For PLL clock the order is reversed. */
    AT91C_BASE_PMC->PMC_MCKR = AT91C_PMC_CSS_MAIN_CLK;
    while(!(AT91C_BASE_PMC->PMC_SR & AT91C_PMC_MCKRDY));

    /* Since AT91C_PMC_PRES_CLK == 0, the following is probably not
       necessary. */
    AT91C_BASE_PMC->PMC_MCKR = AT91C_PMC_CSS_MAIN_CLK | AT91C_PMC_PRES_CLK;
    while(!(AT91C_BASE_PMC->PMC_SR & AT91C_PMC_MCKRDY));
}

/* C entry point, called from _reset0 which sets up stacks and
   initializes memory. */
int main(int argc, char **argv);
void _reset1(void)  {
    init_first();
    init_osc();
    init_dbgu();
    printf("Starting main() for %s\r\n", TARGET);
    main(0, NULL);
}



