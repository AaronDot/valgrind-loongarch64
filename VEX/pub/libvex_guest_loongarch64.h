
/*---------------------------------------------------------------*/
/*--- begin                        libvex_guest_loongarch64.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2021-2022 Loongson Technology Corporation Limited

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __LIBVEX_PUB_GUEST_LOONGARCH64_H
#define __LIBVEX_PUB_GUEST_LOONGARCH64_H

#include "libvex_basictypes.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the LOONGARCH64 CPU state.      ---*/
/*---------------------------------------------------------------*/

typedef
   struct {
      /* Event check fail addr and counter. */
      ULong host_EvC_FAILADDR;
      UInt  host_EvC_COUNTER;
      UInt  _padding0;

      /* CPU Registers */
      ULong guest_R0;   /* Constant zero */
      ULong guest_R1;   /* Return address */
      ULong guest_R2;   /* Thread pointer */
      ULong guest_R3;   /* Stack pointer */
      ULong guest_R4;   /* Argument registers / Return value */
      ULong guest_R5;
      ULong guest_R6;   /* Argument registers */
      ULong guest_R7;
      ULong guest_R8;
      ULong guest_R9;
      ULong guest_R10;
      ULong guest_R11;
      ULong guest_R12;  /* Temporary registers */
      ULong guest_R13;
      ULong guest_R14;
      ULong guest_R15;
      ULong guest_R16;
      ULong guest_R17;
      ULong guest_R18;
      ULong guest_R19;
      ULong guest_R20;
      ULong guest_R21;  /* Reserved */
      ULong guest_R22;  /* Frame pointer / Static register */
      ULong guest_R23;  /* Static registers */
      ULong guest_R24;
      ULong guest_R25;
      ULong guest_R26;
      ULong guest_R27;
      ULong guest_R28;
      ULong guest_R29;
      ULong guest_R30;
      ULong guest_R31;

      ULong guest_PC;    /* Program counter */

      UChar guest_FCC0;  /* Condition Flag Registers */
      UChar guest_FCC1;
      UChar guest_FCC2;
      UChar guest_FCC3;
      UChar guest_FCC4;
      UChar guest_FCC5;
      UChar guest_FCC6;
      UChar guest_FCC7;
      UInt  guest_FCSR;  /* FP/SIMD Control and Status Register */

      /* Various pseudo-regs mandated by Vex or Valgrind. */
      /* Emulation notes */
      UInt  guest_EMNOTE;

      /* For clflush: record start and length of area to invalidate */
      ULong guest_CMSTART;
      ULong guest_CMLEN;

      /* Used to record the unredirected guest address at the start of
         a translation whose start has been redirected.  By reading
         this pseudo-register shortly afterwards, the translation can
         find out what the corresponding no-redirection address was.
         Note, this is only set for wrap-style redirects, not for
         replace-style ones. */
      ULong guest_NRADDR;

      /* Fallback LL/SC support. */
      ULong guest_LLSC_SIZE; /* 0==no transaction, else 4 or 8. */
      ULong guest_LLSC_ADDR; /* Address of the transaction. */
      ULong guest_LLSC_DATA; /* Original value at ADDR. */

      ULong _padding1;

      /* FPU/SIMD Registers */
      U256 guest_X0;
      U256 guest_X1;
      U256 guest_X2;
      U256 guest_X3;
      U256 guest_X4;
      U256 guest_X5;
      U256 guest_X6;
      U256 guest_X7;
      U256 guest_X8;
      U256 guest_X9;
      U256 guest_X10;
      U256 guest_X11;
      U256 guest_X12;
      U256 guest_X13;
      U256 guest_X14;
      U256 guest_X15;
      U256 guest_X16;
      U256 guest_X17;
      U256 guest_X18;
      U256 guest_X19;
      U256 guest_X20;
      U256 guest_X21;
      U256 guest_X22;
      U256 guest_X23;
      U256 guest_X24;
      U256 guest_X25;
      U256 guest_X26;
      U256 guest_X27;
      U256 guest_X28;
      U256 guest_X29;
      U256 guest_X30;
      U256 guest_X31;

      /* VexGuestLOONGARCH64State should have a 16-aligned size */
} VexGuestLOONGARCH64State;

/*---------------------------------------------------------------*/
/*--- Utility functions for LOONGARCH64 guest stuff.          ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT. */

/* Initialise all guest LOONGARCH64 state. */

extern
void LibVEX_GuestLOONGARCH64_initialise ( /*OUT*/
                                          VexGuestLOONGARCH64State* vex_state );

#endif /* ndef __LIBVEX_PUB_GUEST_LOONGARCH64_H */

/*---------------------------------------------------------------*/
/*---                              libvex_guest_loongarch64.h ---*/
/*---------------------------------------------------------------*/
