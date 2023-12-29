
/*---------------------------------------------------------------*/
/*--- begin                           host_loongarch64_defs.c ---*/
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
*/

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_loongarch64_defs.h"


/* --------- Local helpers. --------- */

static inline void mapReg ( HRegRemap* m, HReg* r )
{
   *r = lookupHRegRemap(m, *r);
}

static inline Int extend ( UInt imm, UInt size )
{
   UInt shift = 32 - size;
   return (((Int)imm << shift) >> shift);
}


/* --------- Registers. --------- */

const RRegUniverse* getRRegUniverse_LOONGARCH64 ( void )
{
   /* The real-register universe is a big constant, so we just want to
      initialise it once. */
   static RRegUniverse rRegUniverse_LOONGARCH64;
   static Bool         rRegUniverse_LOONGARCH64_initted = False;

   /* Handy shorthand, nothing more */
   RRegUniverse* ru = &rRegUniverse_LOONGARCH64;

   /* This isn't thread-safe.  Sigh. */
   if (LIKELY(rRegUniverse_LOONGARCH64_initted == True))
      return ru;

   RRegUniverse__init(ru);

   /* Add the registers.  The initial segment of this array must be
      those available for allocation by reg-alloc, and those that
      follow are not available for allocation. */
   ru->allocable_start[HRcInt64] = ru->size;
   ru->regs[ru->size++] = hregLOONGARCH64_R23();
   ru->regs[ru->size++] = hregLOONGARCH64_R24();
   ru->regs[ru->size++] = hregLOONGARCH64_R25();
   ru->regs[ru->size++] = hregLOONGARCH64_R26();
   ru->regs[ru->size++] = hregLOONGARCH64_R27();
   ru->regs[ru->size++] = hregLOONGARCH64_R28();
   ru->regs[ru->size++] = hregLOONGARCH64_R29();
   ru->regs[ru->size++] = hregLOONGARCH64_R30();
   // $r31 is used as guest stack pointer, not available to regalloc.

   // $r12 is used as a chaining/ProfInc/Cmove/genSpill/genReload temporary
   // $r13 is used as a ProfInc temporary
   ru->regs[ru->size++] = hregLOONGARCH64_R14();
   ru->regs[ru->size++] = hregLOONGARCH64_R15();
   ru->regs[ru->size++] = hregLOONGARCH64_R16();
   ru->regs[ru->size++] = hregLOONGARCH64_R17();
   ru->regs[ru->size++] = hregLOONGARCH64_R18();
   ru->regs[ru->size++] = hregLOONGARCH64_R19();
   ru->regs[ru->size++] = hregLOONGARCH64_R20();
   ru->allocable_end[HRcInt64] = ru->size - 1;

   ru->allocable_start[HRcFlt64] = ru->size;
   ru->regs[ru->size++] = hregLOONGARCH64_F24();
   ru->regs[ru->size++] = hregLOONGARCH64_F25();
   ru->regs[ru->size++] = hregLOONGARCH64_F26();
   ru->regs[ru->size++] = hregLOONGARCH64_F27();
   ru->regs[ru->size++] = hregLOONGARCH64_F28();
   ru->regs[ru->size++] = hregLOONGARCH64_F29();
   ru->regs[ru->size++] = hregLOONGARCH64_F30();
   ru->regs[ru->size++] = hregLOONGARCH64_F31();
   ru->allocable_end[HRcFlt64] = ru->size - 1;

   ru->allocable_start[HRcVec128] = ru->size;
   ru->regs[ru->size++] = hregLOONGARCH64_V24();
   ru->regs[ru->size++] = hregLOONGARCH64_V25();
   ru->regs[ru->size++] = hregLOONGARCH64_V26();
   ru->regs[ru->size++] = hregLOONGARCH64_V27();
   ru->regs[ru->size++] = hregLOONGARCH64_V28();
   ru->regs[ru->size++] = hregLOONGARCH64_V29();
   ru->regs[ru->size++] = hregLOONGARCH64_V30();
   ru->regs[ru->size++] = hregLOONGARCH64_V31();
   ru->allocable_end[HRcVec128] = ru->size - 1;

   ru->allocable = ru->size;

   /* And other regs, not available to the allocator. */
   ru->regs[ru->size++] = hregLOONGARCH64_R0();
   ru->regs[ru->size++] = hregLOONGARCH64_R1();
   ru->regs[ru->size++] = hregLOONGARCH64_R2();
   ru->regs[ru->size++] = hregLOONGARCH64_R3();
   ru->regs[ru->size++] = hregLOONGARCH64_R4();
   ru->regs[ru->size++] = hregLOONGARCH64_R5();
   ru->regs[ru->size++] = hregLOONGARCH64_R6();
   ru->regs[ru->size++] = hregLOONGARCH64_R7();
   ru->regs[ru->size++] = hregLOONGARCH64_R8();
   ru->regs[ru->size++] = hregLOONGARCH64_R9();
   ru->regs[ru->size++] = hregLOONGARCH64_R10();
   ru->regs[ru->size++] = hregLOONGARCH64_R11();
   ru->regs[ru->size++] = hregLOONGARCH64_R12();
   ru->regs[ru->size++] = hregLOONGARCH64_R13();
   ru->regs[ru->size++] = hregLOONGARCH64_R21();
   ru->regs[ru->size++] = hregLOONGARCH64_R22();
   ru->regs[ru->size++] = hregLOONGARCH64_R31();
   ru->regs[ru->size++] = hregLOONGARCH64_FCSR3();

   rRegUniverse_LOONGARCH64_initted = True;

   RRegUniverse__check_is_sane(ru);
   return ru;
}

UInt ppHRegLOONGARCH64 ( HReg reg )
{
   Int r;
   Int ret = 0;
   static const HChar* ireg_names[32] = {
      "$zero",
      "$ra",
      "$tp",
      "$sp",
      "$a0", "$a1", "$a2", "$a3", "$a4", "$a5", "$a6", "$a7",
      "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8",
      "$r21", /* Reserved */
      "$fp",
      "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7", "$s8"
   };
   static const HChar* freg_names[32] = {
      "$fa0",  "$fa1",  "$fa2",  "$fa3",  "$fa4",  "$fa5",  "$fa6",  "$fa7",
      "$ft0",  "$ft1",  "$ft2",  "$ft3",  "$ft4",  "$ft5",  "$ft6",  "$ft7",
      "$ft8",  "$ft9",  "$ft10", "$ft11", "$ft12", "$ft13", "$ft14", "$ft15",
      "$fs0",  "$fs1",  "$fs2",  "$fs3",  "$fs4",  "$fs5",  "$fs6",  "$fs7"
   };
   static const HChar* vreg_names[32] = {
      "$vr0",  "$vr1",  "$vr2",  "$vr3",  "$vr4",  "$vr5",  "$vr6",  "$vr7",
      "$vr8",  "$vr9",  "$vr10", "$vr11", "$vr12", "$vr13", "$vr14", "$vr15",
      "$vr16", "$vr17", "$vr18", "$vr19", "$vr20", "$vr21", "$vr22", "$vr23",
      "$vr24", "$vr25", "$vr26", "$vr27", "$vr28", "$vr29", "$vr30", "$vr31"
   };

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      return ppHReg(reg);
   }

   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt32:
         r = hregEncoding(reg);
         vassert(r < 4);
         ret = vex_printf("$fcsr%d", r);
         break;
      case HRcInt64:
         r = hregEncoding(reg);
         vassert(r < 32);
         ret = vex_printf("%s", ireg_names[r]);
         break;
      case HRcFlt64:
         r = hregEncoding(reg);
         vassert(r < 32);
         ret = vex_printf("%s", freg_names[r]);
         break;
      case HRcVec128:
         r = hregEncoding(reg);
         vassert(r < 32);
         ret = vex_printf("%s", vreg_names[r]);
         break;
      default:
         vpanic("ppHRegLOONGARCH64");
         break;
   }

   return ret;
}


/* --------- Condition codes, LOONGARCH64 encoding. --------- */

static inline const HChar* showLOONGARCH64CondCode ( LOONGARCH64CondCode cond )
{
   const HChar* ret;
   switch (cond) {
      case LAcc_EQ:
         ret = "eq";  /* equal */
         break;
      case LAcc_NE:
         ret = "ne";  /* not equal */
         break;
      case LAcc_LT:
         ret = "lt";  /* less than (signed) */
         break;
      case LAcc_GE:
         ret = "ge";  /* great equal (signed) */
         break;
      case LAcc_LTU:
         ret = "ltu"; /* less than (unsigned) */
         break;
      case LAcc_GEU:
         ret = "geu"; /* great equal (unsigned) */
         break;
      case LAcc_AL:
         ret = "al";  /* always (unconditional) */
         break;
      default:
         vpanic("showLOONGARCH64CondCode");
         break;
   }
   return ret;
}


/* --------- Memory address expressions (amodes). --------- */

LOONGARCH64AMode* LOONGARCH64AMode_RI ( HReg reg, UShort imm )
{
   LOONGARCH64AMode* am = LibVEX_Alloc_inline(sizeof(LOONGARCH64AMode));
   am->tag = LAam_RI;
   am->LAam.RI.base = reg;
   am->LAam.RI.index = imm;
   return am;
}

LOONGARCH64AMode* LOONGARCH64AMode_RR ( HReg base, HReg index )
{
   LOONGARCH64AMode* am = LibVEX_Alloc_inline(sizeof(LOONGARCH64AMode));
   am->tag = LAam_RR;
   am->LAam.RR.base = base;
   am->LAam.RR.index = index;
   return am;
}

static inline void ppLOONGARCH64AMode ( LOONGARCH64AMode* am )
{
   switch (am->tag) {
      case LAam_RI:
         ppHRegLOONGARCH64(am->LAam.RI.base);
         vex_printf(", ");
         vex_printf("%d", extend((UInt)am->LAam.RI.index, 12));
         break;
      case LAam_RR:
         ppHRegLOONGARCH64(am->LAam.RR.base);
         vex_printf(", ");
         ppHRegLOONGARCH64(am->LAam.RR.index);
         break;
      default:
         vpanic("ppLOONGARCH64AMode");
         break;
   }
}

static inline void addRegUsage_LOONGARCH64AMode( HRegUsage* u,
                                                 LOONGARCH64AMode* am )
{
   switch (am->tag) {
      case LAam_RI:
         addHRegUse(u, HRmRead, am->LAam.RI.base);
         break;
      case LAam_RR:
         addHRegUse(u, HRmRead, am->LAam.RR.base);
         addHRegUse(u, HRmRead, am->LAam.RR.index);
         break;
      default:
         vpanic("addRegUsage_LOONGARCH64AMode");
         break;
   }
}

static inline void mapRegs_LOONGARCH64AMode( HRegRemap* m,
                                             LOONGARCH64AMode* am )
{
   switch (am->tag) {
      case LAam_RI:
         mapReg(m, &am->LAam.RI.base);
         break;
      case LAam_RR:
         mapReg(m, &am->LAam.RR.base);
         mapReg(m, &am->LAam.RR.index);
         break;
      default:
         vpanic("mapRegs_LOONGARCH64AMode");
         break;
   }
}


/* --------- Operand, which can be reg or imm. --------- */

LOONGARCH64RI* LOONGARCH64RI_R ( HReg reg )
{
   LOONGARCH64RI* op = LibVEX_Alloc_inline(sizeof(LOONGARCH64RI));
   op->tag = LAri_Reg;
   op->LAri.R.reg = reg;
   return op;
}

LOONGARCH64RI* LOONGARCH64RI_I ( UShort imm, UChar size, Bool isSigned )
{
   LOONGARCH64RI* op = LibVEX_Alloc_inline(sizeof(LOONGARCH64RI));
   op->tag = LAri_Imm;
   op->LAri.I.imm = imm;
   op->LAri.I.size = size;
   op->LAri.I.isSigned = isSigned;
   vassert(imm < (1 << size));
   vassert(size == 1 || size == 2 || size == 3 || size == 4 ||
           size == 5 || size == 6 || size == 8 || size == 12);
   return op;
}

static inline void ppLOONGARCH64RI ( LOONGARCH64RI* ri )
{
   switch (ri->tag) {
      case LAri_Reg:
         ppHRegLOONGARCH64(ri->LAri.R.reg);
         break;
      case LAri_Imm:
         if (ri->LAri.I.isSigned) {
            vex_printf("%d", extend((UInt)ri->LAri.I.imm, ri->LAri.I.size));
         } else {
            vex_printf("%u", (UInt)ri->LAri.I.imm);
         }
         break;
      default:
         vpanic("ppLOONGARCH64RI");
         break;
   }
}

static inline void addRegUsage_LOONGARCH64RI( HRegUsage* u, LOONGARCH64RI* ri )
{
   switch (ri->tag) {
      case LAri_Reg:
         addHRegUse(u, HRmRead, ri->LAri.R.reg);
         break;
      case LAri_Imm:
         break;
      default:
         vpanic("addRegUsage_LOONGARCH64RI");
         break;
   }
}

static inline void mapRegs_LOONGARCH64RI( HRegRemap* m, LOONGARCH64RI* ri )
{
   switch (ri->tag) {
      case LAri_Reg:
         mapReg(m, &ri->LAri.R.reg);
         break;
      case LAri_Imm:
         break;
      default:
         vpanic("mapRegs_LOONGARCH64RI");
         break;
   }
}


/* --------- Instructions. --------- */

static inline const HChar* showLOONGARCH64UnOp ( LOONGARCH64UnOp op )
{
   switch (op) {
      case LAun_CLZ_W:
         return "clz.w";
      case LAun_CTZ_W:
         return "ctz.w";
      case LAun_CLZ_D:
         return "clz.d";
      case LAun_CTZ_D:
         return "ctz.w";
      case LAun_EXT_W_H:
         return "ext.w.h";
      case LAun_EXT_W_B:
         return "ext.w.b";
      default:
         vpanic("showLOONGARCH64UnOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64BinOp ( LOONGARCH64BinOp op )
{
   switch (op) {
      case LAbin_ADD_W:
         return "add.w";
      case LAbin_ADD_D:
         return "add.d";
      case LAbin_SUB_W:
         return "sub.w";
      case LAbin_SUB_D:
         return "sub.d";
      case LAbin_NOR:
         return "nor";
      case LAbin_AND:
         return "and";
      case LAbin_OR:
         return "or";
      case LAbin_XOR:
         return "xor";
      case LAbin_SLL_W:
         return "sll.w";
      case LAbin_SRL_W:
         return "srl.w";
      case LAbin_SRA_W:
         return "sra.w";
      case LAbin_SLL_D:
         return "sll.d";
      case LAbin_SRL_D:
         return "srl.d";
      case LAbin_SRA_D:
         return "sra.d";
      case LAbin_MUL_W:
         return "mul.w";
      case LAbin_MUL_D:
         return "mul.d";
      case LAbin_MULH_W:
         return "mulh.w";
      case LAbin_MULH_WU:
         return "mulh.wu";
      case LAbin_MULH_D:
         return "mulh.d";
      case LAbin_MULH_DU:
         return "mulh.du";
      case LAbin_MULW_D_W:
         return "mulw.d.w";
      case LAbin_MULW_D_WU:
         return "mulw.d.wu";
      case LAbin_DIV_W:
         return "div.w";
      case LAbin_MOD_W:
         return "mod.w";
      case LAbin_DIV_WU:
         return "div.wu";
      case LAbin_MOD_WU:
         return "mod.wu";
      case LAbin_DIV_D:
         return "div.d";
      case LAbin_MOD_D:
         return "mod.d";
      case LAbin_DIV_DU:
         return "div.du";
      case LAbin_MOD_DU:
         return "mod.du";
      case LAbin_SLLI_W:
         return "slli.w";
      case LAbin_SLLI_D:
         return "slli.d";
      case LAbin_SRLI_W:
         return "srli.w";
      case LAbin_SRLI_D:
         return "srli.d";
      case LAbin_SRAI_W:
         return "srai.w";
      case LAbin_SRAI_D:
         return "srai.d";
      case LAbin_ADDI_W:
         return "addi.w";
      case LAbin_ADDI_D:
         return "addi.d";
      case LAbin_ANDI:
         return "andi";
      case LAbin_ORI:
         return "ori";
      case LAbin_XORI:
         return "xori";
      default:
         vpanic("showLOONGARCH64BinOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64LoadOp ( LOONGARCH64LoadOp op )
{
   switch (op) {
      case LAload_LD_D:
         return "ld.d";
      case LAload_LD_BU:
         return "ld.bu";
      case LAload_LD_HU:
         return "ld.hu";
      case LAload_LD_WU:
         return "ld.wu";
      case LAload_LDX_D:
         return "ldx.d";
      case LAload_LDX_BU:
         return "ldx.bu";
      case LAload_LDX_HU:
         return "ldx.hu";
      case LAload_LDX_WU:
         return "ldx.wu";
      default:
         vpanic("LOONGARCH64LoadOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64StoreOp ( LOONGARCH64StoreOp op )
{
   switch (op) {
      case LAstore_ST_B:
         return "st.b";
      case LAstore_ST_H:
         return "st.h";
      case LAstore_ST_W:
         return "st.w";
      case LAstore_ST_D:
         return "st.d";
      case LAstore_STX_B:
         return "stx.b";
      case LAstore_STX_H:
         return "stx.h";
      case LAstore_STX_W:
         return "stx.w";
      case LAstore_STX_D:
         return "stx.d";
      default:
         vpanic("LOONGARCH64StoreOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64LLSCOp ( LOONGARCH64LLSCOp op )
{
   switch (op) {
      case LAllsc_LL_W:
         return "ll.w";
      case LAllsc_SC_W:
         return "sc.w";
      case LAllsc_LL_D:
         return "ll.d";
      case LAllsc_SC_D:
         return "sc.d";
      default:
         vpanic("LOONGARCH64LLSCOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64BarOp ( LOONGARCH64BarOp op )
{
   const HChar* ret;
   switch (op) {
      case LAbar_DBAR:
         return "dbar";
      case LAbar_IBAR:
         return "ibar";
      default:
         vpanic("showLOONGARCH64BarOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpUnOp ( LOONGARCH64FpUnOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpun_FABS_S:
         return "fabs.s";
      case LAfpun_FABS_D:
         return "fabs.d";
      case LAfpun_FNEG_S:
         return "fneg.s";
      case LAfpun_FNEG_D:
         return "fneg.d";
      case LAfpun_FLOGB_S:
         return "flogb.s";
      case LAfpun_FLOGB_D:
         return "flogb.d";
      case LAfpun_FSQRT_S:
         return "fsqrt.s";
      case LAfpun_FSQRT_D:
         return "fsqrt.d";
      case LAfpun_FRSQRT_S:
         return "frsqrt.s";
      case LAfpun_FRSQRT_D:
         return "frsqrt.d";
      case LAfpun_FCVT_S_D:
         return "fcvt.s.d";
      case LAfpun_FCVT_D_S:
         return "fcvt.d.s";
      case LAfpun_FTINT_W_S:
         return "ftint.w.s";
      case LAfpun_FTINT_W_D:
         return "ftint.w.d";
      case LAfpun_FTINT_L_S:
         return "ftint.l.s";
      case LAfpun_FTINT_L_D:
         return "ftint.l.d";
      case LAfpun_FFINT_S_W:
         return "ffint.s.w";
      case LAfpun_FFINT_S_L:
         return "ffint.s.l";
      case LAfpun_FFINT_D_W:
         return "ffint.d.w";
      case LAfpun_FFINT_D_L:
         return "ffint.d.l";
      case LAfpun_FRINT_S:
         return "frint.s";
      case LAfpun_FRINT_D:
         return "frint.d";
      default:
         vpanic("showLOONGARCH64FpUnOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpBinOp ( LOONGARCH64FpBinOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpbin_FADD_S:
         return "fadd.s";
      case LAfpbin_FADD_D:
         return "fadd.d";
      case LAfpbin_FSUB_S:
         return "fsub.s";
      case LAfpbin_FSUB_D:
         return "fsub.d";
      case LAfpbin_FMUL_S:
         return "fmul.s";
      case LAfpbin_FMUL_D:
         return "fmul.d";
      case LAfpbin_FDIV_S:
         return "fdiv.s";
      case LAfpbin_FDIV_D:
         return "fdiv.d";
      case LAfpbin_FMAX_S:
         return "fmax.s";
      case LAfpbin_FMAX_D:
         return "fmax.d";
      case LAfpbin_FMIN_S:
         return "fmin.s";
      case LAfpbin_FMIN_D:
         return "fmin.d";
      case LAfpbin_FMAXA_S:
         return "fmaxa.s";
      case LAfpbin_FMAXA_D:
         return "fmaxa.d";
      case LAfpbin_FMINA_S:
         return "fmina.s";
      case LAfpbin_FMINA_D:
         return "fmina.d";
      case LAfpbin_FSCALEB_S:
         return "fscaleb.s";
      case LAfpbin_FSCALEB_D:
         return "fscaleb.d";
      default:
         vpanic("showLOONGARCH64FpBinOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpTriOp ( LOONGARCH64FpTriOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpbin_FMADD_S:
         return "fmadd.s";
      case LAfpbin_FMADD_D:
         return "fmadd.d";
      case LAfpbin_FMSUB_S:
         return "fmsub.s";
      case LAfpbin_FMSUB_D:
         return "fmsub.d";
      default:
         vpanic("showLOONGARCH64FpTriOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpLoadOp ( LOONGARCH64FpLoadOp op )
{
   switch (op) {
      case LAfpload_FLD_S:
         return "fld.s";
      case LAfpload_FLD_D:
         return "fld.d";
      case LAfpload_FLDX_S:
         return "fldx.s";
      case LAfpload_FLDX_D:
         return "fldx.d";
      default:
         vpanic("LOONGARCH64FpLoadOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64FpStoreOp ( LOONGARCH64FpStoreOp op )
{
   switch (op) {
      case LAfpstore_FST_S:
         return "fst.s";
      case LAfpstore_FST_D:
         return "fst.d";
      case LAfpstore_FSTX_S:
         return "fstx.s";
      case LAfpstore_FSTX_D:
         return "fstx.d";
      default:
         vpanic("LOONGARCH64FpStoreOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64FpMoveOp ( LOONGARCH64FpMoveOp op )
{
   switch (op) {
      case LAfpmove_FMOV_S:
         return "fmov.s";
      case LAfpmove_FMOV_D:
         return "fmov.d";
      case LAfpmove_MOVGR2FR_W:
         return "movgr2fr.w";
      case LAfpmove_MOVGR2FR_D:
         return "movgr2fr.d";
      case LAfpmove_MOVFR2GR_S:
         return "movfr2gr.s";
      case LAfpmove_MOVFR2GR_D:
         return "movfr2gr.d";
      case LAfpmove_MOVGR2FCSR:
         return "movgr2fcsr";
      case LAfpmove_MOVFCSR2GR:
         return "movfcsr2gr";
      default:
         vpanic("showLOONGARCH64FpMoveOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64FpCmpOp ( LOONGARCH64FpCmpOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpcmp_FCMP_CLT_S:
         return "fcmp.clt.s";
      case LAfpcmp_FCMP_CLT_D:
         return "fcmp.clt.d";
      case LAfpcmp_FCMP_CEQ_S:
         return "fcmp.ceq.s";
      case LAfpcmp_FCMP_CEQ_D:
         return "fcmp.ceq.d";
      case LAfpcmp_FCMP_CUN_S:
         return "fcmp.cun.s";
      case LAfpcmp_FCMP_CUN_D:
         return "fcmp.cun.d";
      default:
         vpanic("showLOONGARCH64FpCmpOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64VecUnOp ( LOONGARCH64VecUnOp op )
{
   switch (op) {
      case LAvecun_VCLO_B:          return "vclo.b";
      case LAvecun_VCLO_H:          return "vclo.h";
      case LAvecun_VCLO_W:          return "vclo.w";
      case LAvecun_VCLO_D:          return "vclo.d";
      case LAvecun_VCLZ_B:          return "vclz.b";
      case LAvecun_VCLZ_H:          return "vclz.h";
      case LAvecun_VCLZ_W:          return "vclz.w";
      case LAvecun_VCLZ_D:          return "vclz.d";
      case LAvecun_VPCNT_B:         return "vpcnt.b";
      case LAvecun_VPCNT_H:         return "vpcnt.h";
      case LAvecun_VPCNT_W:         return "vpcnt.w";
      case LAvecun_VPCNT_D:         return "vpcnt.d";
      case LAvecun_VNEG_B:          return "vneg.b";
      case LAvecun_VNEG_H:          return "vneg.h";
      case LAvecun_VNEG_W:          return "vneg.w";
      case LAvecun_VNEG_D:          return "vneg.d";
      case LAvecun_VMSKLTZ_B:       return "vmskltz.b";
      case LAvecun_VMSKLTZ_H:       return "vmskltz.h";
      case LAvecun_VMSKLTZ_W:       return "vmskltz.w";
      case LAvecun_VMSKLTZ_D:       return "vmskltz.d";
      case LAvecun_VMSKGEZ_B:       return "vmskgez.b";
      case LAvecun_VMSKNZ_B:        return "vmsknz.b";
      case LAvecun_VSETEQZ_V:       return "vseteqz.v";
      case LAvecun_VSETNEZ_V:       return "vsetnez.v";
      case LAvecun_VSETANYEQZ_B:    return "vsetanyeqz.b";
      case LAvecun_VSETANYEQZ_H:    return "vsetanyeqz.h";
      case LAvecun_VSETANYEQZ_W:    return "vsetanyeqz.w";
      case LAvecun_VSETANYEQZ_D:    return "vsetanyeqz.d";
      case LAvecun_VSETALLNEZ_B:    return "vsetallnez.b";
      case LAvecun_VSETALLNEZ_H:    return "vsetallnez.h";
      case LAvecun_VSETALLNEZ_W:    return "vsetallnez.w";
      case LAvecun_VSETALLNEZ_D:    return "vsetallnez.d";
      case LAvecun_VFLOGB_S:        return "vglogb.s";
      case LAvecun_VFLOGB_D:        return "vglogb.d";
      case LAvecun_VFCLASS_S:       return "vfclass.s";
      case LAvecun_VFCLASS_D:       return "vfclass.d";
      case LAvecun_VFSQRT_S:        return "vfsqrt.s";
      case LAvecun_VFSQRT_D:        return "vfsqrt.d";
      case LAvecun_VFRECIP_S:       return "vfrecip.s";
      case LAvecun_VFRECIP_D:       return "vfrecip.d";
      case LAvecun_VFRSQRT_S:       return "vfrsqrt.s";
      case LAvecun_VFRSQRT_D:       return "vfrsqrt.d";
      case LAvecun_VFRINT_S:        return "vfrint.s";
      case LAvecun_VFRINT_D:        return "vfrint.d";
      case LAvecun_VFRINTRM_S:      return "vfrintrm.s";
      case LAvecun_VFRINTRM_D:      return "vfrintrm.d";
      case LAvecun_VFRINTRP_S:      return "vfrintrp.s";
      case LAvecun_VFRINTRP_D:      return "vfrintrp.d";
      case LAvecun_VFRINTRZ_S:      return "vfrintrz.s";
      case LAvecun_VFRINTRZ_D:      return "vfrintrz.d";
      case LAvecun_VFRINTRNZ_S:     return "vfrintrnz.s";
      case LAvecun_VFRINTRNZ_D:     return "vfrintrnz.d";
      case LAvecun_VFCVTL_S_H:      return "vfcvtl.s.h";
      case LAvecun_VFCVTH_S_H:      return "vfcvth.s.h";
      case LAvecun_VFCVTL_D_S:      return "vfcvtl.d.s";
      case LAvecun_VFCVTH_D_S:      return "vfcvth.d.s";
      case LAvecun_VFFINT_S_W:      return "vffint.s.w";
      case LAvecun_VFFINT_S_WU:     return "vffint.s.wu";
      case LAvecun_VFFINT_D_L:      return "vffint.d.l";
      case LAvecun_VFFINT_D_LU:     return "vffint.d.lu";
      case LAvecun_VFFINTL_D_W:     return "vffintl.d.w";
      case LAvecun_VFFINTH_D_W:     return "vffinth.d.w";
      case LAvecun_VFTINT_W_S:      return "vftint.w.s";
      case LAvecun_VFTINT_L_D:      return "vftint.l.d";
      case LAvecun_VFTINTRM_W_S:    return "vftintrm.w.s";
      case LAvecun_VFTINTRM_L_D:    return "vftintrm.l.d";
      case LAvecun_VFTINTRP_W_S:    return "vftintrp.w.s";
      case LAvecun_VFTINTRP_L_D:    return "vftintrp.l.d";
      case LAvecun_VFTINTRZ_W_S:    return "vftintrz.w.s";
      case LAvecun_VFTINTRZ_L_D:    return "vftintrz.l.d";
      case LAvecun_VFTINTRNE_W_S:   return "vftintrne.w.s";
      case LAvecun_VFTINTRNE_L_D:   return "vftintrne.l.d";
      case LAvecun_VFTINT_WU_S:     return "vftint.wu.s";
      case LAvecun_VFTINT_LU_D:     return "vftint.lu.d";
      case LAvecun_VFTINTRZ_WU_S:   return "vftintrz.wu.s";
      case LAvecun_VFTINTRZ_LU_D:   return "vftintrz.lu.d";
      case LAvecun_VFTINTL_L_S:     return "vftintl.l.s";
      case LAvecun_VFTINTH_L_S:     return "vftinth.l.s";
      case LAvecun_VFTINTRML_L_S:   return "vftintrml.l.s";
      case LAvecun_VFTINTRMH_L_S:   return "vftintrmh.l.s";
      case LAvecun_VFTINTRPL_L_S:   return "vftintrpl.l.s";
      case LAvecun_VFTINTRPH_L_S:   return "vftintrph.l.s";
      case LAvecun_VFTINTRZL_L_S:   return "vftintrzl.l.s";
      case LAvecun_VFTINTRZH_L_S:   return "vftintrzh.l.s";
      case LAvecun_VFTINTRNEL_L_S:  return "vftintrnel.l.s";
      case LAvecun_VFTINTRNEH_L_S:  return "vftintrneh.l.s";
      case LAvecun_VEXTH_H_B:       return "vexth.h.b";
      case LAvecun_VEXTH_W_H:       return "vexth.w.h";
      case LAvecun_VEXTH_D_W:       return "vexth.d.w";
      case LAvecun_VEXTH_Q_D:       return "vexth.q.d";
      case LAvecun_VEXTH_HU_BU:     return "vexth.hu.bu";
      case LAvecun_VEXTH_WU_HU:     return "vexth.wu.hu";
      case LAvecun_VEXTH_DU_WU:     return "vexth.du.wu";
      case LAvecun_VEXTH_QU_DU:     return "vexth.qu.du";
      case LAvecun_VREPLGR2VR_B:    return "vreplgr2vr.b";
      case LAvecun_VREPLGR2VR_H:    return "vreplgr2vr.h";
      case LAvecun_VREPLGR2VR_W:    return "vreplgr2vr.w";
      case LAvecun_VREPLGR2VR_D:    return "vreplgr2vr.d";
      case LAvecun_VEXTL_Q_D:       return "vextl.q.d";
      case LAvecun_VEXTL_QU_DU:     return "vextl.qu.du";
      default:                      vpanic("showLOONGARCH64VecUnOp");
   }
}

static inline const HChar* showLOONGARCH64VecBinOp ( LOONGARCH64VecBinOp op )
{
   switch (op) {
      case LAvecbin_VSEQ_B:            return "vseq.b";
      case LAvecbin_VSEQ_H:            return "vseq.h";
      case LAvecbin_VSEQ_W:            return "vseq.w";
      case LAvecbin_VSEQ_D:            return "vseq.d";
      case LAvecbin_VSLE_B:            return "vsle.b";
      case LAvecbin_VSLE_H:            return "vsle.h";
      case LAvecbin_VSLE_W:            return "vsle.w";
      case LAvecbin_VSLE_D:            return "vsle.d";
      case LAvecbin_VSLE_BU:           return "vsle.bu";
      case LAvecbin_VSLE_HU:           return "vsle.hu";
      case LAvecbin_VSLE_WU:           return "vsle.wu";
      case LAvecbin_VSLE_DU:           return "vsle.du";
      case LAvecbin_VSLT_B:            return "vslt.b";
      case LAvecbin_VSLT_H:            return "vslt.h";
      case LAvecbin_VSLT_W:            return "vslt.w";
      case LAvecbin_VSLT_D:            return "vslt.d";
      case LAvecbin_VSLT_BU:           return "vslt.bu";
      case LAvecbin_VSLT_HU:           return "vslt.hu";
      case LAvecbin_VSLT_WU:           return "vslt.wu";
      case LAvecbin_VSLT_DU:           return "vslt.du";
      case LAvecbin_VADD_W:            return "vadd.w";
      case LAvecbin_VADD_D:            return "vadd.d";
      case LAvecbin_VSUB_B:            return "vsub.b";
      case LAvecbin_VSUB_H:            return "vsub.h";
      case LAvecbin_VSUB_W:            return "vsub.w";
      case LAvecbin_VSUB_D:            return "vsub.d";
      case LAvecbin_VADDWEV_H_B:       return "vaddwev.h.b";
      case LAvecbin_VADDWEV_W_H:       return "vaddwev.w.h";
      case LAvecbin_VADDWEV_D_W:       return "vaddwev.d.w";
      case LAvecbin_VADDWEV_Q_D:       return "vaddwev.q.d";
      case LAvecbin_VSUBWEV_H_B:       return "vsubwev.h.b";
      case LAvecbin_VSUBWEV_W_H:       return "vsubwev.w.h";
      case LAvecbin_VSUBWEV_D_W:       return "vsubwev.d.w";
      case LAvecbin_VSUBWEV_Q_D:       return "vsubwev.q.d";
      case LAvecbin_VADDWOD_H_B:       return "vaddwod.h.b";
      case LAvecbin_VADDWOD_W_H:       return "vaddwod.w.h";
      case LAvecbin_VADDWOD_D_W:       return "vaddwod.d.w";
      case LAvecbin_VADDWOD_Q_D:       return "vaddwod.q.d";
      case LAvecbin_VSUBWOD_H_B:       return "vsubwod.h.b";
      case LAvecbin_VSUBWOD_W_H:       return "vsubwod.w.h";
      case LAvecbin_VSUBWOD_D_W:       return "vsubwod.d.w";
      case LAvecbin_VSUBWOD_Q_D:       return "vsubwod.q.d";
      case LAvecbin_VADDWEV_H_BU:      return "vaddwev.h.bu";
      case LAvecbin_VADDWEV_W_HU:      return "vaddwev.w.hu";
      case LAvecbin_VADDWEV_D_WU:      return "vaddwev.d.wu";
      case LAvecbin_VADDWEV_Q_DU:      return "vaddwev.q.du";
      case LAvecbin_VSUBWEV_H_BU:      return "vsubwev.h.bu";
      case LAvecbin_VSUBWEV_W_HU:      return "vsubwev.w.hu";
      case LAvecbin_VSUBWEV_D_WU:      return "vsubwev.d.wu";
      case LAvecbin_VSUBWEV_Q_DU:      return "vsubwev.q.du";
      case LAvecbin_VADDWOD_H_BU:      return "vaddwod.h.bu";
      case LAvecbin_VADDWOD_W_HU:      return "vaddwod.w.hu";
      case LAvecbin_VADDWOD_D_WU:      return "vaddwod.d.wu";
      case LAvecbin_VADDWOD_Q_DU:      return "vaddwod.q.du";
      case LAvecbin_VSUBWOD_H_BU:      return "vsubwod.h.bu";
      case LAvecbin_VSUBWOD_W_HU:      return "vsubwod.w.hu";
      case LAvecbin_VSUBWOD_D_WU:      return "vsubwod.d.wu";
      case LAvecbin_VSUBWOD_Q_DU:      return "vsubwod.q.du";
      case LAvecbin_VADDWEV_H_BU_B:    return "vaddwev.h.bu.b";
      case LAvecbin_VADDWEV_W_HU_H:    return "vaddwev.w.hu.h";
      case LAvecbin_VADDWEV_D_WU_W:    return "vaddwev.d.wu.w";
      case LAvecbin_VADDWEV_Q_DU_D:    return "vaddwev.q.du.d";
      case LAvecbin_VADDWOD_H_BU_B:    return "vaddwod.h.bu.b";
      case LAvecbin_VADDWOD_W_HU_H:    return "vaddwod.w.hu.h";
      case LAvecbin_VADDWOD_D_WU_W:    return "vaddwod.d.wu.w";
      case LAvecbin_VADDWOD_Q_DU_D:    return "vaddwod.q.du.d";
      case LAvecbin_VSADD_B:           return "vsadd.b";
      case LAvecbin_VSADD_H:           return "vsadd.h";
      case LAvecbin_VSADD_W:           return "vsadd.w";
      case LAvecbin_VSADD_D:           return "vsadd.d";
      case LAvecbin_VSSUB_B:           return "vssub.b";
      case LAvecbin_VSSUB_H:           return "vssub.h";
      case LAvecbin_VSSUB_W:           return "vssub.w";
      case LAvecbin_VSSUB_D:           return "vssub.d";
      case LAvecbin_VSADD_BU:          return "vsadd.bu";
      case LAvecbin_VSADD_HU:          return "vsadd.hu";
      case LAvecbin_VSADD_WU:          return "vsadd.wu";
      case LAvecbin_VSADD_DU:          return "vsadd.du";
      case LAvecbin_VSSUB_BU:          return "vssub.bu";
      case LAvecbin_VSSUB_HU:          return "vssub.hu";
      case LAvecbin_VSSUB_WU:          return "vssub.wu";
      case LAvecbin_VSSUB_DU:          return "vssub.du";
      case LAvecbin_VHADDW_H_B:        return "vhaddw.h.b";
      case LAvecbin_VHADDW_W_H:        return "vhaddw.w.h";
      case LAvecbin_VHADDW_D_W:        return "vhaddw.d.w";
      case LAvecbin_VHADDW_Q_D:        return "vhaddw.q.d";
      case LAvecbin_VHSUBW_H_B:        return "vhsubw.h.b";
      case LAvecbin_VHSUBW_W_H:        return "vhsubw.w.h";
      case LAvecbin_VHSUBW_D_W:        return "vhsubw.d.w";
      case LAvecbin_VHSUBW_Q_D:        return "vhsubw.q.d";
      case LAvecbin_VHADDW_HU_BU:      return "vhaddw.hu.bu";
      case LAvecbin_VHADDW_WU_HU:      return "vhaddw.wu.hu";
      case LAvecbin_VHADDW_DU_WU:      return "vhaddw.du.wu";
      case LAvecbin_VHADDW_QU_DU:      return "vhaddw.qu.du";
      case LAvecbin_VHSUBW_HU_BU:      return "vhsubw.hu.bu";
      case LAvecbin_VHSUBW_WU_HU:      return "vhsubw.wu.hu";
      case LAvecbin_VHSUBW_DU_WU:      return "vhsubw.du.wu";
      case LAvecbin_VHSUBW_QU_DU:      return "vhsubw.qu.du";
      case LAvecbin_VADDA_B:           return "vadda.b";
      case LAvecbin_VADDA_H:           return "vadda.h";
      case LAvecbin_VADDA_W:           return "vadda.w";
      case LAvecbin_VADDA_D:           return "vadda.d";
      case LAvecbin_VABSD_B:           return "vabsd.b";
      case LAvecbin_VABSD_H:           return "vabsd.h";
      case LAvecbin_VABSD_W:           return "vabsd.w";
      case LAvecbin_VABSD_D:           return "vabsd.d";
      case LAvecbin_VABSD_BU:          return "vabsd.bu";
      case LAvecbin_VABSD_HU:          return "vabsd.hu";
      case LAvecbin_VABSD_WU:          return "vabsd.wu";
      case LAvecbin_VABSD_DU:          return "vabsd.du";
      case LAvecbin_VAVG_B:            return "vavg.b";
      case LAvecbin_VAVG_H:            return "vavg.h";
      case LAvecbin_VAVG_W:            return "vavg.w";
      case LAvecbin_VAVG_D:            return "vavg.d";
      case LAvecbin_VAVG_BU:           return "vavg.bu";
      case LAvecbin_VAVG_HU:           return "vavg.hu";
      case LAvecbin_VAVG_WU:           return "vavg.wu";
      case LAvecbin_VAVG_DU:           return "vavg.du";
      case LAvecbin_VAVGR_B:           return "vavgr.b";
      case LAvecbin_VAVGR_H:           return "vavgr.h";
      case LAvecbin_VAVGR_W:           return "vavgr.w";
      case LAvecbin_VAVGR_D:           return "vavgr.d";
      case LAvecbin_VAVGR_BU:          return "vavgr.bu";
      case LAvecbin_VAVGR_HU:          return "vavgr.hu";
      case LAvecbin_VAVGR_WU:          return "vavgr.wu";
      case LAvecbin_VAVGR_DU:          return "vavgr.du";
      case LAvecbin_VMAX_B:            return "vmax.b";
      case LAvecbin_VMAX_H:            return "vmax.h";
      case LAvecbin_VMAX_W:            return "vmax.w";
      case LAvecbin_VMAX_D:            return "vmax.d";
      case LAvecbin_VMIN_B:            return "vmin.b";
      case LAvecbin_VMIN_H:            return "vmin.h";
      case LAvecbin_VMIN_W:            return "vmin.w";
      case LAvecbin_VMIN_D:            return "vmin.d";
      case LAvecbin_VMAX_BU:           return "vmax.bu";
      case LAvecbin_VMAX_HU:           return "vmax.hu";
      case LAvecbin_VMAX_WU:           return "vmax.wu";
      case LAvecbin_VMAX_DU:           return "vmax.du";
      case LAvecbin_VMIN_BU:           return "vmin.bu";
      case LAvecbin_VMIN_HU:           return "vmin.hu";
      case LAvecbin_VMIN_WU:           return "vmin.wu";
      case LAvecbin_VMIN_DU:           return "vmin.du";
      case LAvecbin_VMUL_B:            return "vmul.b";
      case LAvecbin_VMUL_H:            return "vmul.h";
      case LAvecbin_VMUL_W:            return "vmul.w";
      case LAvecbin_VMUL_D:            return "vmul.d";
      case LAvecbin_VMUH_B:            return "vmuh.b";
      case LAvecbin_VMUH_H:            return "vmuh.h";
      case LAvecbin_VMUH_W:            return "vmuh.w";
      case LAvecbin_VMUH_D:            return "vmuh.d";
      case LAvecbin_VMUH_BU:           return "vmuh.bu";
      case LAvecbin_VMUH_HU:           return "vmuh.hu";
      case LAvecbin_VMUH_WU:           return "vmuh.wu";
      case LAvecbin_VMUH_DU:           return "vmuh.du";
      case LAvecbin_VMULWEV_H_B:       return "vmulwev.h.b";
      case LAvecbin_VMULWEV_W_H:       return "vmulwev.w.h";
      case LAvecbin_VMULWEV_D_W:       return "vmulwev.d.w";
      case LAvecbin_VMULWEV_Q_D:       return "vmulwev.q.d";
      case LAvecbin_VMULWOD_H_B:       return "vmulwod.h.b";
      case LAvecbin_VMULWOD_W_H:       return "vmulwod.w.h";
      case LAvecbin_VMULWOD_D_W:       return "vmulwod.d.w";
      case LAvecbin_VMULWOD_Q_D:       return "vmulwod.q.d";
      case LAvecbin_VMULWEV_H_BU:      return "vmulwev.h.bu";
      case LAvecbin_VMULWEV_W_HU:      return "vmulwev.w.hu";
      case LAvecbin_VMULWEV_D_WU:      return "vmulwev.d.wu";
      case LAvecbin_VMULWEV_Q_DU:      return "vmulwev.q.du";
      case LAvecbin_VMULWOD_H_BU:      return "vmulwod.h.bu";
      case LAvecbin_VMULWOD_W_HU:      return "vmulwod.w.hu";
      case LAvecbin_VMULWOD_D_WU:      return "vmulwod.d.wu";
      case LAvecbin_VMULWOD_Q_DU:      return "vmulwod.q.du";
      case LAvecbin_VMULWEV_H_BU_B:    return "vmulwev.h.bu.b";
      case LAvecbin_VMULWEV_W_HU_H:    return "vmulwev.w.hu.h";
      case LAvecbin_VMULWEV_D_WU_W:    return "vmulwev.d.wu.w";
      case LAvecbin_VMULWEV_Q_DU_D:    return "vmulwev.q.du.d";
      case LAvecbin_VMULWOD_H_BU_B:    return "vmulwod.h.bu.b";
      case LAvecbin_VMULWOD_W_HU_H:    return "vmulwod.w.hu.h";
      case LAvecbin_VMULWOD_D_WU_W:    return "vmulwod.d.wu.w";
      case LAvecbin_VMULWOD_Q_DU_D:    return "vmulwod.q.du.d";
      case LAvecbin_VMADD_B:           return "vmadd.b";
      case LAvecbin_VMADD_H:           return "vmadd.h";
      case LAvecbin_VMADD_W:           return "vmadd.w";
      case LAvecbin_VMADD_D:           return "vmadd.d";
      case LAvecbin_VMSUB_B:           return "vmsub.b";
      case LAvecbin_VMSUB_H:           return "vmsub.h";
      case LAvecbin_VMSUB_W:           return "vmsub.w";
      case LAvecbin_VMSUB_D:           return "vmsub.d";
      case LAvecbin_VMADDWEV_H_B:      return "vmaddwev.h.b";
      case LAvecbin_VMADDWEV_W_H:      return "vmaddwev.w.h";
      case LAvecbin_VMADDWEV_D_W:      return "vmaddwev.d.w";
      case LAvecbin_VMADDWEV_Q_D:      return "vmaddwev.q.d";
      case LAvecbin_VMADDWOD_H_B:      return "vmaddwod.h.b";
      case LAvecbin_VMADDWOD_W_H:      return "vmaddwod.w.h";
      case LAvecbin_VMADDWOD_D_W:      return "vmaddwod.d.w";
      case LAvecbin_VMADDWOD_Q_D:      return "vmaddwod.q.d";
      case LAvecbin_VMADDWEV_H_BU:     return "vmaddwev.h.bu";
      case LAvecbin_VMADDWEV_W_HU:     return "vmaddwev.w.hu";
      case LAvecbin_VMADDWEV_D_WU:     return "vmaddwev.d.wu";
      case LAvecbin_VMADDWEV_Q_DU:     return "vmaddwev.q.du";
      case LAvecbin_VMADDWOD_H_BU:     return "vmaddwod.h.bu";
      case LAvecbin_VMADDWOD_W_HU:     return "vmaddwod.w.hu";
      case LAvecbin_VMADDWOD_D_WU:     return "vmaddwod.d.wu";
      case LAvecbin_VMADDWOD_Q_DU:     return "vmaddwod.q.du";
      case LAvecbin_VMADDWEV_H_BU_B:   return "vmaddwev.h.bu.b";
      case LAvecbin_VMADDWEV_W_HU_H:   return "vmaddwev.w.hu.h";
      case LAvecbin_VMADDWEV_D_WU_W:   return "vmaddwev.d.wu.w";
      case LAvecbin_VMADDWEV_Q_DU_D:   return "vmaddwev.q.du.d";
      case LAvecbin_VMADDWOD_H_BU_B:   return "vmaddwod.h.bu.b";
      case LAvecbin_VMADDWOD_W_HU_H:   return "vmaddwod.w.hu.h";
      case LAvecbin_VMADDWOD_D_WU_W:   return "vmaddwod.d.wu.w";
      case LAvecbin_VMADDWOD_Q_DU_D:   return "vmaddwod.q.du.d";
      case LAvecbin_VDIV_B:            return "vdiv.b";
      case LAvecbin_VDIV_H:            return "vdiv.h";
      case LAvecbin_VDIV_W:            return "vdiv.w";
      case LAvecbin_VDIV_D:            return "vdiv.d";
      case LAvecbin_VMOD_B:            return "vmod.b";
      case LAvecbin_VMOD_H:            return "vmod.h";
      case LAvecbin_VMOD_W:            return "vmod.w";
      case LAvecbin_VMOD_D:            return "vmod.d";
      case LAvecbin_VDIV_BU:           return "vdiv.bu";
      case LAvecbin_VDIV_HU:           return "vdiv.hu";
      case LAvecbin_VDIV_WU:           return "vdiv.wu";
      case LAvecbin_VDIV_DU:           return "vdiv.du";
      case LAvecbin_VMOD_BU:           return "vmod.bu";
      case LAvecbin_VMOD_HU:           return "vmod.hu";
      case LAvecbin_VMOD_WU:           return "vmod.wu";
      case LAvecbin_VMOD_DU:           return "vmod.du";
      case LAvecbin_VSLL_B:            return "vsll.b";
      case LAvecbin_VSLL_H:            return "vsll.h";
      case LAvecbin_VSLL_W:            return "vsll.w";
      case LAvecbin_VSLL_D:            return "vsll.d";
      case LAvecbin_VSRL_B:            return "vsrl.b";
      case LAvecbin_VSRL_H:            return "vsrl.h";
      case LAvecbin_VSRL_W:            return "vsrl.w";
      case LAvecbin_VSRL_D:            return "vsrl.d";
      case LAvecbin_VSRA_B:            return "vsra.b";
      case LAvecbin_VSRA_H:            return "vsra.h";
      case LAvecbin_VSRA_W:            return "vsra.w";
      case LAvecbin_VSRA_D:            return "vsra.d";
      case LAvecbin_VROTR_B:           return "vrotr.b";
      case LAvecbin_VROTR_H:           return "vrotr.h";
      case LAvecbin_VROTR_W:           return "vrotr.w";
      case LAvecbin_VROTR_D:           return "vrotr.d";
      case LAvecbin_VSRLR_B:           return "vsrlr.b";
      case LAvecbin_VSRLR_H:           return "vsrlr.h";
      case LAvecbin_VSRLR_W:           return "vsrlr.w";
      case LAvecbin_VSRLR_D:           return "vsrlr.d";
      case LAvecbin_VSRAR_B:           return "vsrar.b";
      case LAvecbin_VSRAR_H:           return "vsrar.h";
      case LAvecbin_VSRAR_W:           return "vsrar.w";
      case LAvecbin_VSRAR_D:           return "vsrar.d";
      case LAvecbin_VSRLN_B_H:         return "vsrln.b.h";
      case LAvecbin_VSRLN_H_W:         return "vsrln.h.w";
      case LAvecbin_VSRLN_W_D:         return "vsrln.w.d";
      case LAvecbin_VSRAN_B_H:         return "vsran.b.h";
      case LAvecbin_VSRAN_H_W:         return "vsran.h.w";
      case LAvecbin_VSRAN_W_D:         return "vsran.w.d";
      case LAvecbin_VSRLRN_B_H:        return "vsrlrn.b.h";
      case LAvecbin_VSRLRN_H_W:        return "vsrlrn.h.w";
      case LAvecbin_VSRLRN_W_D:        return "vsrlrn.w.d";
      case LAvecbin_VSRARN_B_H:        return "vsrarn.b.h";
      case LAvecbin_VSRARN_H_W:        return "vsrarn.h.w";
      case LAvecbin_VSRARN_W_D:        return "vsrarn.w.d";
      case LAvecbin_VSSRLN_B_H:        return "vssrln.b.h";
      case LAvecbin_VSSRLN_H_W:        return "vssrln.h.w";
      case LAvecbin_VSSRLN_W_D:        return "vssrln.w.d";
      case LAvecbin_VSSRAN_B_H:        return "vssran.b.h";
      case LAvecbin_VSSRAN_H_W:        return "vssran.h.w";
      case LAvecbin_VSSRAN_W_D:        return "vssran.w.d";
      case LAvecbin_VSSRLRN_B_H:       return "vssrlrn.b.h";
      case LAvecbin_VSSRLRN_H_W:       return "vssrlrn.h.w";
      case LAvecbin_VSSRLRN_W_D:       return "vssrlrn.w.d";
      case LAvecbin_VSSRARN_B_H:       return "vssrarn.b.h";
      case LAvecbin_VSSRARN_H_W:       return "vssrarn.h.w";
      case LAvecbin_VSSRARN_W_D:       return "vssrarn.w.d";
      case LAvecbin_VSSRLN_BU_H:       return "vssrln.bu.h";
      case LAvecbin_VSSRLN_HU_W:       return "vssrln.hu.w";
      case LAvecbin_VSSRLN_WU_D:       return "vssrln.wu.d";
      case LAvecbin_VSSRAN_BU_H:       return "vssran.bu.h";
      case LAvecbin_VSSRAN_HU_W:       return "vssran.hu.w";
      case LAvecbin_VSSRAN_WU_D:       return "vssran.wu.d";
      case LAvecbin_VSSRLRN_BU_H:      return "vssrlrn.bu.h";
      case LAvecbin_VSSRLRN_HU_W:      return "vssrlrn.hu.w";
      case LAvecbin_VSSRLRN_WU_D:      return "vssrlrn.wu.d";
      case LAvecbin_VSSRARN_BU_H:      return "vssrarn.bu.h";
      case LAvecbin_VSSRARN_HU_W:      return "vssrarn.hu.w";
      case LAvecbin_VSSRARN_WU_D:      return "vssrarn.wu.d";
      case LAvecbin_VBITCLR_B:         return "vbitclr.b";
      case LAvecbin_VBITCLR_H:         return "vbitclr.h";
      case LAvecbin_VBITCLR_W:         return "vbitclr.w";
      case LAvecbin_VBITCLR_D:         return "vbitclr.d";
      case LAvecbin_VBITSET_B:         return "vbitset.b";
      case LAvecbin_VBITSET_H:         return "vbitset.h";
      case LAvecbin_VBITSET_W:         return "vbitset.w";
      case LAvecbin_VBITSET_D:         return "vbitset.d";
      case LAvecbin_VBITREV_B:         return "vbitrev.b";
      case LAvecbin_VBITREV_H:         return "vbitrev.h";
      case LAvecbin_VBITREV_W:         return "vbitrev.w";
      case LAvecbin_VBITREV_D:         return "vbitrev.d";
      case LAvecbin_VPACKEV_B:         return "vpackev.b";
      case LAvecbin_VPACKEV_H:         return "vpackev.h";
      case LAvecbin_VPACKEV_W:         return "vpackev.w";
      case LAvecbin_VPACKEV_D:         return "vpackev.d";
      case LAvecbin_VPACKOD_B:         return "vpackod.b";
      case LAvecbin_VPACKOD_H:         return "vpackod.h";
      case LAvecbin_VPACKOD_W:         return "vpackod.w";
      case LAvecbin_VPACKOD_D:         return "vpackod.d";
      case LAvecbin_VILVL_B:           return "vilvl.b";
      case LAvecbin_VILVL_H:           return "vilvl.h";
      case LAvecbin_VILVL_W:           return "vilvl.w";
      case LAvecbin_VILVL_D:           return "vilvl.d";
      case LAvecbin_VILVH_B:           return "vilvh.b";
      case LAvecbin_VILVH_H:           return "vilvh.h";
      case LAvecbin_VILVH_W:           return "vilvh.w";
      case LAvecbin_VILVH_D:           return "vilvh.d";
      case LAvecbin_VPICKEV_B:         return "vpickev.b";
      case LAvecbin_VPICKEV_H:         return "vpickev.h";
      case LAvecbin_VPICKEV_W:         return "vpickev.w";
      case LAvecbin_VPICKEV_D:         return "vpickev.d";
      case LAvecbin_VPICKOD_B:         return "vpickod.b";
      case LAvecbin_VPICKOD_H:         return "vpickod.h";
      case LAvecbin_VPICKOD_W:         return "vpickod.w";
      case LAvecbin_VPICKOD_D:         return "vpickod.d";
      case LAvecbin_VREPLVE_B:         return "vreplve.b";
      case LAvecbin_VREPLVE_H:         return "vreplve.h";
      case LAvecbin_VREPLVE_W:         return "vreplve.w";
      case LAvecbin_VREPLVE_D:         return "vreplve.d";
      case LAvecbin_VAND_V:            return "vand.v";
      case LAvecbin_VOR_V:             return "vor.v";
      case LAvecbin_VXOR_V:            return "vxor.v";
      case LAvecbin_VNOR_V:            return "vnor.v";
      case LAvecbin_VANDN_V:           return "vandn.v";
      case LAvecbin_VORN_V:            return "vorn.v";
      case LAvecbin_VFRSTP_B:          return "vfrstp.b";
      case LAvecbin_VFRSTP_H:          return "vfrstp.h";
      case LAvecbin_VADD_Q:            return "vadd.q";
      case LAvecbin_VSUB_Q:            return "vsub.q";
      case LAvecbin_VSIGNCOV_B:        return "vsigncov.b";
      case LAvecbin_VSIGNCOV_H:        return "vsigncov.h";
      case LAvecbin_VSIGNCOV_W:        return "vsigncov.w";
      case LAvecbin_VSIGNCOV_D:        return "vsigncov.d";
      case LAvecbin_VFADD_S:           return "vfadd.s";
      case LAvecbin_VFADD_D:           return "vfadd.d";
      case LAvecbin_VFSUB_S:           return "vfsub.s";
      case LAvecbin_VFSUB_D:           return "vfsub.d";
      case LAvecbin_VFMUL_S:           return "vfmul.s";
      case LAvecbin_VFMUL_D:           return "vfmul.d";
      case LAvecbin_VFDIV_S:           return "vfdiv.s";
      case LAvecbin_VFDIV_D:           return "vfdiv.d";
      case LAvecbin_VFMAX_S:           return "vfmax.s";
      case LAvecbin_VFMAX_D:           return "vfmax.d";
      case LAvecbin_VFMIN_S:           return "vfmin.s";
      case LAvecbin_VFMIN_D:           return "vfmin.d";
      case LAvecbin_VFMAXA_S:          return "vfmaxa.s";
      case LAvecbin_VFMAXA_D:          return "vfmaxa.d";
      case LAvecbin_VFMINA_S:          return "vfmina.s";
      case LAvecbin_VFMINA_D:          return "vfmina.d";
      case LAvecbin_VFCVT_H_S:         return "vfcvt.h.s";
      case LAvecbin_VFCVT_S_D:         return "vfcvt.s.d";
      case LAvecbin_VFFINT_S_L:        return "vffint.s.l";
      case LAvecbin_VFTINT_W_D:        return "vffint.w.d";
      case LAvecbin_VFTINTRM_W_D:      return "vftintrm.w.d";
      case LAvecbin_VFTINTRP_W_D:      return "vftintrp.w.d";
      case LAvecbin_VFTINTRZ_W_D:      return "vftintrz.w.d";
      case LAvecbin_VFTINTRNE_W_D:     return "vftintrne.w.d";
      case LAvecbin_VSHUF_H:           return "vshuf.h";
      case LAvecbin_VSHUF_W:           return "vshuf.w";
      case LAvecbin_VSHUF_D:           return "vshuf.d";
      case LAvecbin_VSEQI_B:           return "vseqi.b";
      case LAvecbin_VSEQI_H:           return "vseqi.h";
      case LAvecbin_VSEQI_W:           return "vseqi.w";
      case LAvecbin_VSEQI_D:           return "vseqi.d";
      case LAvecbin_VSLEI_B:           return "vslei.b";
      case LAvecbin_VSLEI_H:           return "vslei.h";
      case LAvecbin_VSLEI_W:           return "vslei.w";
      case LAvecbin_VSLEI_D:           return "vslei.d";
      case LAvecbin_VSLEI_BU:          return "vslei.bu";
      case LAvecbin_VSLEI_HU:          return "vslei.hu";
      case LAvecbin_VSLEI_WU:          return "vslei.wu";
      case LAvecbin_VSLEI_DU:          return "vslei.du";
      case LAvecbin_VSLTI_B:           return "vslti.b";
      case LAvecbin_VSLTI_H:           return "vslti.h";
      case LAvecbin_VSLTI_W:           return "vslti.w";
      case LAvecbin_VSLTI_D:           return "vslti.d";
      case LAvecbin_VSLTI_BU:          return "vslti.bu";
      case LAvecbin_VSLTI_HU:          return "vslti.hu";
      case LAvecbin_VSLTI_WU:          return "vslti.wu";
      case LAvecbin_VSLTI_DU:          return "vslti.du";
      case LAvecbin_VADDI_BU:          return "vaddi.bu";
      case LAvecbin_VADDI_HU:          return "vaddi.hu";
      case LAvecbin_VADDI_WU:          return "vaddi.wu";
      case LAvecbin_VADDI_DU:          return "vaddi.du";
      case LAvecbin_VSUBI_BU:          return "vsubi.bu";
      case LAvecbin_VSUBI_HU:          return "vsubi.hu";
      case LAvecbin_VSUBI_WU:          return "vsubi.wu";
      case LAvecbin_VSUBI_DU:          return "vsubi.du";
      case LAvecbin_VBSLL_V:           return "vbsll.v";
      case LAvecbin_VBSRL_V:           return "vbsrl.v";
      case LAvecbin_VMAXI_B:           return "vmaxi.b";
      case LAvecbin_VMAXI_H:           return "vmaxi.h";
      case LAvecbin_VMAXI_W:           return "vmaxi.w";
      case LAvecbin_VMAXI_D:           return "vmaxi.d";
      case LAvecbin_VMINI_B:           return "vmini.b";
      case LAvecbin_VMINI_H:           return "vmini.h";
      case LAvecbin_VMINI_W:           return "vmini.w";
      case LAvecbin_VMINI_D:           return "vmini.d";
      case LAvecbin_VMAXI_BU:          return "vmaxi.bu";
      case LAvecbin_VMAXI_HU:          return "vmaxi.hu";
      case LAvecbin_VMAXI_WU:          return "vmaxi.wu";
      case LAvecbin_VMAXI_DU:          return "vmaxi.du";
      case LAvecbin_VMINI_BU:          return "vmini.bu";
      case LAvecbin_VMINI_HU:          return "vmini.hu";
      case LAvecbin_VMINI_WU:          return "vmini.wu";
      case LAvecbin_VMINI_DU:          return "vmini.du";
      case LAvecbin_VFRSTPI_B:         return "vfrstpi.b";
      case LAvecbin_VFRSTPI_H:         return "vfrstpi.h";
      case LAvecbin_VROTRI_B:          return "vrotri.b";
      case LAvecbin_VROTRI_H:          return "vrotri.h";
      case LAvecbin_VROTRI_W:          return "vrotri.w";
      case LAvecbin_VROTRI_D:          return "vrotri.d";
      case LAvecbin_VSRLRI_B:          return "vsrlri.b";
      case LAvecbin_VSRLRI_H:          return "vsrlri.h";
      case LAvecbin_VSRLRI_W:          return "vsrlri.w";
      case LAvecbin_VSRLRI_D:          return "vsrlri.d";
      case LAvecbin_VSRARI_B:          return "vsrari.b";
      case LAvecbin_VSRARI_H:          return "vsrari.h";
      case LAvecbin_VSRARI_W:          return "vsrari.w";
      case LAvecbin_VSRARI_D:          return "vsrari.d";
      case LAvecbin_VINSGR2VR_B:       return "vinsgr2vr.b";
      case LAvecbin_VINSGR2VR_H:       return "vinsgr2vr.h";
      case LAvecbin_VINSGR2VR_W:       return "vinsgr2vr.w";
      case LAvecbin_VINSGR2VR_D:       return "vinsgr2vr.d";
      case LAvecbin_VPICKVE2GR_B:      return "vpickve2gr.b";
      case LAvecbin_VPICKVE2GR_H:      return "vpickve2gr.h";
      case LAvecbin_VPICKVE2GR_W:      return "vpickve2gr.w";
      case LAvecbin_VPICKVE2GR_D:      return "vpickve2gr.d";
      case LAvecbin_VPICKVE2GR_BU:     return "vpickve2gr.bu";
      case LAvecbin_VPICKVE2GR_HU:     return "vpickve2gr.hu";
      case LAvecbin_VPICKVE2GR_WU:     return "vpickve2gr.wu";
      case LAvecbin_VPICKVE2GR_DU:     return "vpickve2gr.du";
      case LAvecbin_VREPLVEI_B:        return "vreplvei.b";
      case LAvecbin_VREPLVEI_H:        return "vreplvei.h";
      case LAvecbin_VREPLVEI_W:        return "vreplvei.w";
      case LAvecbin_VREPLVEI_D:        return "vreplvei.d";
      case LAvecbin_VSLLWIL_H_B:       return "vsllwil.h.b";
      case LAvecbin_VSLLWIL_W_H:       return "vsllwil.w.h";
      case LAvecbin_VSLLWIL_D_W:       return "vsllwil.d.w";
      case LAvecbin_VSLLWIL_HU_BU:     return "vsllwil.hu.bu";
      case LAvecbin_VSLLWIL_WU_HU:     return "vsllwil.wu.hu";
      case LAvecbin_VSLLWIL_DU_WU:     return "vsllwil.du.wu";
      case LAvecbin_VBITCLRI_B:        return "vbitclri.b";
      case LAvecbin_VBITCLRI_H:        return "vbitclri.h";
      case LAvecbin_VBITCLRI_W:        return "vbitclri.w";
      case LAvecbin_VBITCLRI_D:        return "vbitclri.d";
      case LAvecbin_VBITSETI_B:        return "vbitseti.b";
      case LAvecbin_VBITSETI_H:        return "vbitseti.h";
      case LAvecbin_VBITSETI_W:        return "vbitseti.w";
      case LAvecbin_VBITSETI_D:        return "vbitseti.d";
      case LAvecbin_VBITREVI_B:        return "vbitrevi.b";
      case LAvecbin_VBITREVI_H:        return "vbitrevi.h";
      case LAvecbin_VBITREVI_W:        return "vbitrevi.w";
      case LAvecbin_VBITREVI_D:        return "vbitrevi.d";
      case LAvecbin_VSAT_B:            return "vsat.b";
      case LAvecbin_VSAT_H:            return "vsat.h";
      case LAvecbin_VSAT_W:            return "vsat.w";
      case LAvecbin_VSAT_D:            return "vsat.d";
      case LAvecbin_VSAT_BU:           return "vsat.bu";
      case LAvecbin_VSAT_HU:           return "vsat.hu";
      case LAvecbin_VSAT_WU:           return "vsat.wu";
      case LAvecbin_VSAT_DU:           return "vsat.du";
      case LAvecbin_VSLLI_B:           return "vslli.b";
      case LAvecbin_VSLLI_H:           return "vslli.h";
      case LAvecbin_VSLLI_W:           return "vslli.w";
      case LAvecbin_VSLLI_D:           return "vslli.d";
      case LAvecbin_VSRLI_B:           return "vsrli.b";
      case LAvecbin_VSRLI_H:           return "vsrli.h";
      case LAvecbin_VSRLI_W:           return "vsrli.w";
      case LAvecbin_VSRLI_D:           return "vsrli.d";
      case LAvecbin_VSRAI_B:           return "vsrai.b";
      case LAvecbin_VSRAI_H:           return "vsrai.h";
      case LAvecbin_VSRAI_W:           return "vsrai.w";
      case LAvecbin_VSRAI_D:           return "vsrai.d";
      case LAvecbin_VSRLNI_B_H:        return "vsrlni.b.h";
      case LAvecbin_VSRLNI_H_W:        return "vsrlni.h.w";
      case LAvecbin_VSRLNI_W_D:        return "vsrlni.w.d";
      case LAvecbin_VSRLNI_D_Q:        return "vsrlni.d.q";
      case LAvecbin_VSRLRNI_B_H:       return "vsrlrni.b.h";
      case LAvecbin_VSRLRNI_H_W:       return "vsrlrni.h.w";
      case LAvecbin_VSRLRNI_W_D:       return "vsrlrni.w.d";
      case LAvecbin_VSRLRNI_D_Q:       return "vsrlrni.d.q";
      case LAvecbin_VSSRLNI_B_H:       return "vssrlni.b.h";
      case LAvecbin_VSSRLNI_H_W:       return "vssrlni.h.w";
      case LAvecbin_VSSRLNI_W_D:       return "vssrlni.w.d";
      case LAvecbin_VSSRLNI_D_Q:       return "vssrlni.d.q";
      case LAvecbin_VSSRLNI_BU_H:      return "vssrlni.bu.h";
      case LAvecbin_VSSRLNI_HU_W:      return "vssrlni.hu.w";
      case LAvecbin_VSSRLNI_WU_D:      return "vssrlni.wu.d";
      case LAvecbin_VSSRLNI_DU_Q:      return "vssrlni.du.q";
      case LAvecbin_VSSRLRNI_B_H:      return "vssrlrni.b.h";
      case LAvecbin_VSSRLRNI_H_W:      return "vssrlrni.h.w";
      case LAvecbin_VSSRLRNI_W_D:      return "vssrlrni.w.d";
      case LAvecbin_VSSRLRNI_D_Q:      return "vssrlrni.d.q";
      case LAvecbin_VSSRLRNI_BU_H:     return "vssrlrni.bu.h";
      case LAvecbin_VSSRLRNI_HU_W:     return "vssrlrni.hu.w";
      case LAvecbin_VSSRLRNI_WU_D:     return "vssrlrni.wu.d";
      case LAvecbin_VSSRLRNI_DU_Q:     return "vssrlrni.du.q";
      case LAvecbin_VSRANI_B_H:        return "vsrani.b.h";
      case LAvecbin_VSRANI_H_W:        return "vsrani.h.w";
      case LAvecbin_VSRANI_W_D:        return "vsrani.w.d";
      case LAvecbin_VSRANI_D_Q:        return "vsrani.d.q";
      case LAvecbin_VSRARNI_B_H:       return "vsrarni.b.h";
      case LAvecbin_VSRARNI_H_W:       return "vsrarni.h.w";
      case LAvecbin_VSRARNI_W_D:       return "vsrarni.w.d";
      case LAvecbin_VSRARNI_D_Q:       return "vsrarni.d.q";
      case LAvecbin_VSSRANI_B_H:       return "vssrani.b.h";
      case LAvecbin_VSSRANI_H_W:       return "vssrani.h.w";
      case LAvecbin_VSSRANI_W_D:       return "vssrani.w.d";
      case LAvecbin_VSSRANI_D_Q:       return "vssrani.d.q";
      case LAvecbin_VSSRANI_BU_H:      return "vssrani.bu.h";
      case LAvecbin_VSSRANI_HU_W:      return "vssrani.hu.w";
      case LAvecbin_VSSRANI_WU_D:      return "vssrani.wu.d";
      case LAvecbin_VSSRANI_DU_Q:      return "vssrani.du.q";
      case LAvecbin_VSSRARNI_B_H:      return "vssrarni.b.h";
      case LAvecbin_VSSRARNI_H_W:      return "vssrarni.h.w";
      case LAvecbin_VSSRARNI_W_D:      return "vssrarni.w.d";
      case LAvecbin_VSSRARNI_D_Q:      return "vssrarni.d.q";
      case LAvecbin_VSSRARNI_BU_H:     return "vssrarni.bu.h";
      case LAvecbin_VSSRARNI_HU_W:     return "vssrarni.hu.w";
      case LAvecbin_VSSRARNI_WU_D:     return "vssrarni.wu.d";
      case LAvecbin_VSSRARNI_DU_Q:     return "vssrarni.du.q";
      case LAvecbin_VEXTRINS_D:        return "vextrins.d";
      case LAvecbin_VEXTRINS_W:        return "vextrins.w";
      case LAvecbin_VEXTRINS_H:        return "vextrins.h";
      case LAvecbin_VEXTRINS_B:        return "vextrins.b";
      case LAvecbin_VSHUF4I_B:         return "vshug4i.b";
      case LAvecbin_VSHUF4I_H:         return "vshug4i.h";
      case LAvecbin_VSHUF4I_W:         return "vshug4i.w";
      case LAvecbin_VSHUF4I_D:         return "vshug4i.d";
      case LAvecbin_VBITSELI_B:        return "vbitseli.b";
      case LAvecbin_VADDI_B:           return "vaddi.b";
      case LAvecbin_VORI_B:            return "vori.b";
      case LAvecbin_VXORI_B:           return "vxori.b";
      case LAvecbin_VNORI_B:           return "vnori.b";
      case LAvecbin_VPERMI_W:          return "vpermi.w";
      default:                         vpanic("showLOONGARCH64VecBinOp");
   }
}

static inline const HChar* showLOONGARCH64VecTriOp ( LOONGARCH64VecTriOp op )
{
   switch (op) {
      case LAvectri_VFMADD_S:    return "vfmadd.s";
      case LAvectri_VFMADD_D:    return "vfmadd.d";
      case LAvectri_VFMSUB_S:    return "vfmsub.s";
      case LAvectri_VFMSUB_D:    return "vfmsub.d";
      case LAvectri_VFNMADD_S:   return "vfnmadd.s";
      case LAvectri_VFNMADD_D:   return "vfnmadd.d";
      case LAvectri_VFNMSUB_S:   return "vfnmsub.s";
      case LAvectri_VFNMSUB_D:   return "vfnmsub.d";
      case LAvectri_VBITSEL_V:   return "vbitsel.v";
      case LAvectri_VSHUF_B:     return "vshuf.b";
      default:                   vpanic("showLOONGARCH64VecTriOp");
   }
}

static inline const HChar* showLOONGARCH64VecLoadOp ( LOONGARCH64VecLoadOp op )
{
   switch (op) {
      case LAvecload_VLD:        return "vld";
      case LAvecload_VLDREPL_D:  return "vldrepl.b";
      case LAvecload_VLDREPL_W:  return "vldrepl.w";
      case LAvecload_VLDREPL_H:  return "vldrepl.h";
      case LAvecload_VLDREPL_B:  return "vldrepl.b";
      case LAvecload_VLDX:       return "vldx";
      case LAvecload_VLDI:       return "vldi";
      default:                   vpanic("showLOONGARCH64VecLoadOp");
   }
}

static inline const HChar* showLOONGARCH64VecStoreOp ( LOONGARCH64VecStoreOp op )
{
   switch (op) {
      case LAvecstore_VST:       return "vst";
      case LAvecstore_VSTX:      return "vstx";
      default:                   vpanic("showLOONGARCH64VecStoreOp");

   }
}

static inline const HChar* showLOONGARCH64VecFpCmpOp ( LOONGARCH64VecFpCmpOp op )
{
   switch (op) {
      case LAveccmp_VFCMP_CLT_S: return "vfcmp.clt.s";
      case LAveccmp_VFCMP_CLT_D: return "vfcmp.clt.d";
      case LAveccmp_VFCMP_CEQ_S: return "vfcmp.ceq.s";
      case LAveccmp_VFCMP_CEQ_D: return "vfcmp.ceq.d";
      case LAveccmp_VFCMP_CUN_S: return "vfcmp.cun.s";
      case LAveccmp_VFCMP_CUN_D: return "vfcmp.cun.d";
      default:                   vpanic("showLOONGARCH64VecFpCmpOp");

   }
}

LOONGARCH64Instr* LOONGARCH64Instr_LI ( ULong imm, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_LI;
   i->LAin.LI.imm      = imm;
   i->LAin.LI.dst      = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Unary ( LOONGARCH64UnOp op,
                                           HReg src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Un;
   i->LAin.Unary.op    = op;
   i->LAin.Unary.src   = src;
   i->LAin.Unary.dst   = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Binary ( LOONGARCH64BinOp op,
                                            LOONGARCH64RI* src2,
                                            HReg src1, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Bin;
   i->LAin.Binary.op   = op;
   i->LAin.Binary.src2 = src2;
   i->LAin.Binary.src1 = src1;
   i->LAin.Binary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Load ( LOONGARCH64LoadOp op,
                                          LOONGARCH64AMode* src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Load;
   i->LAin.Load.op     = op;
   i->LAin.Load.src    = src;
   i->LAin.Load.dst    = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Store ( LOONGARCH64StoreOp op,
                                           LOONGARCH64AMode* dst, HReg src )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Store;
   i->LAin.Store.op    = op;
   i->LAin.Store.dst   = dst;
   i->LAin.Store.src   = src;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_LLSC ( LOONGARCH64LLSCOp op, Bool isLoad,
                                          LOONGARCH64AMode* addr, HReg val )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_LLSC;
   i->LAin.LLSC.op     = op;
   i->LAin.LLSC.isLoad = isLoad;
   i->LAin.LLSC.addr   = addr;
   i->LAin.LLSC.val    = val;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Bar ( LOONGARCH64BarOp op, UShort hint )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Bar;
   i->LAin.Bar.op      = op;
   i->LAin.Bar.hint    = hint;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpUnary ( LOONGARCH64FpUnOp op,
                                             HReg src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpUn;
   i->LAin.FpUnary.op  = op;
   i->LAin.FpUnary.src = src;
   i->LAin.FpUnary.dst = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpBinary ( LOONGARCH64FpBinOp op, HReg src2,
                                              HReg src1, HReg dst )
{
   LOONGARCH64Instr* i   = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                = LAin_FpBin;
   i->LAin.FpBinary.op   = op;
   i->LAin.FpBinary.src2 = src2;
   i->LAin.FpBinary.src1 = src1;
   i->LAin.FpBinary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpTrinary ( LOONGARCH64FpTriOp op,
                                               HReg src3, HReg src2,
                                               HReg src1, HReg dst )
{
   LOONGARCH64Instr* i    = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                 = LAin_FpTri;
   i->LAin.FpTrinary.op   = op;
   i->LAin.FpTrinary.src3 = src3;
   i->LAin.FpTrinary.src2 = src2;
   i->LAin.FpTrinary.src1 = src1;
   i->LAin.FpTrinary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpLoad ( LOONGARCH64FpLoadOp op,
                                            LOONGARCH64AMode* src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpLoad;
   i->LAin.FpLoad.op   = op;
   i->LAin.FpLoad.src  = src;
   i->LAin.FpLoad.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpStore ( LOONGARCH64FpStoreOp op,
                                             LOONGARCH64AMode* dst, HReg src )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpStore;
   i->LAin.FpStore.op  = op;
   i->LAin.FpStore.dst = dst;
   i->LAin.FpStore.src = src;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpMove ( LOONGARCH64FpMoveOp op,
                                            HReg src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpMove;
   i->LAin.FpMove.op   = op;
   i->LAin.FpMove.src  = src;
   i->LAin.FpMove.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpCmp ( LOONGARCH64FpCmpOp op, HReg src2,
                                           HReg src1, HReg dst )
{
   LOONGARCH64Instr* i   = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                = LAin_FpCmp;
   i->LAin.FpCmp.op      = op;
   i->LAin.FpCmp.src2    = src2;
   i->LAin.FpCmp.src1    = src1;
   i->LAin.FpCmp.dst     = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_VecUnary ( LOONGARCH64VecUnOp op,
                                              HReg src, HReg dst )
{
   LOONGARCH64Instr* i    = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                 = LAin_VecUn;
   i->LAin.VecUnary.op    = op;
   i->LAin.VecUnary.src   = src;
   i->LAin.VecUnary.dst   = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_VecBinary ( LOONGARCH64VecBinOp op,
                                               LOONGARCH64RI* src2,
                                               HReg src1, HReg dst )
{
   LOONGARCH64Instr* i    = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                 = LAin_VecBin;
   i->LAin.VecBinary.op   = op;
   i->LAin.VecBinary.src2 = src2;
   i->LAin.VecBinary.src1 = src1;
   i->LAin.VecBinary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_VecTrinary ( LOONGARCH64VecTriOp op,
                                                HReg src3, HReg src2,
                                                HReg src1, HReg dst )
{
   LOONGARCH64Instr* i    = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                 = LAin_VecTri;
   i->LAin.VecTrinary.op   = op;
   i->LAin.VecTrinary.src3 = src3;
   i->LAin.VecTrinary.src2 = src2;
   i->LAin.VecTrinary.src1 = src1;
   i->LAin.VecTrinary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_VecLoad ( LOONGARCH64VecLoadOp op,
                                             LOONGARCH64AMode* src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_VecLoad;
   i->LAin.VecLoad.op  = op;
   i->LAin.VecLoad.src = src;
   i->LAin.VecLoad.dst = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_VecStore ( LOONGARCH64VecStoreOp op,
                                              LOONGARCH64AMode* dst, HReg src )
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_VecStore;
   i->LAin.VecStore.op  = op;
   i->LAin.VecStore.dst = dst;
   i->LAin.VecStore.src = src;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_VecFpCmp ( LOONGARCH64VecFpCmpOp op,
                                              HReg dst, HReg src1,
                                              HReg src2)
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_VecStore;
   i->LAin.VecFpCmp.op  = op;
   i->LAin.VecFpCmp.dst = dst;
   i->LAin.VecFpCmp.src1 = src1;
   i->LAin.VecFpCmp.src2 = src2;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Cas ( HReg old, HReg addr, HReg expd,
                                         HReg data, Bool size64 )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Cas;
   i->LAin.Cas.old     = old;
   i->LAin.Cas.addr    = addr;
   i->LAin.Cas.expd    = expd;
   i->LAin.Cas.data    = data;
   i->LAin.Cas.size64  = size64;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Cmp ( LOONGARCH64CondCode cond,
                                         HReg src2, HReg src1, HReg dst )
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_Cmp;
   i->LAin.Cmp.cond     = cond;
   i->LAin.Cmp.src2     = src2;
   i->LAin.Cmp.src1     = src1;
   i->LAin.Cmp.dst      = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_CMove ( HReg cond, HReg r0, HReg r1,
                                           HReg dst, Bool isInt )
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_CMove;
   i->LAin.CMove.cond   = cond;
   i->LAin.CMove.r0     = r0;
   i->LAin.CMove.r1     = r1;
   i->LAin.CMove.dst    = dst;
   i->LAin.CMove.isInt  = isInt;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Call ( HReg cond, Addr64 target,
                                          UInt nArgRegs, RetLoc rloc )
{
   LOONGARCH64Instr* i   = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                = LAin_Call;
   i->LAin.Call.cond     = cond;
   i->LAin.Call.target   = target;
   i->LAin.Call.nArgRegs = nArgRegs;
   i->LAin.Call.rloc     = rloc;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_XDirect ( Addr64 dstGA,
                                             LOONGARCH64AMode* amPC,
                                             HReg cond, Bool toFastEP )
{
   LOONGARCH64Instr* i      = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                   = LAin_XDirect;
   i->LAin.XDirect.dstGA    = dstGA;
   i->LAin.XDirect.amPC     = amPC;
   i->LAin.XDirect.cond     = cond;
   i->LAin.XDirect.toFastEP = toFastEP;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_XIndir ( HReg dstGA, LOONGARCH64AMode* amPC,
                                            HReg cond )
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_XIndir;
   i->LAin.XIndir.dstGA = dstGA;
   i->LAin.XIndir.amPC  = amPC;
   i->LAin.XIndir.cond  = cond;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_XAssisted ( HReg dstGA,
                                               LOONGARCH64AMode* amPC,
                                               HReg cond, IRJumpKind jk )
{
   LOONGARCH64Instr* i     = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                  = LAin_XAssisted;
   i->LAin.XAssisted.dstGA = dstGA;
   i->LAin.XAssisted.amPC  = amPC;
   i->LAin.XAssisted.cond  = cond;
   i->LAin.XAssisted.jk    = jk;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_EvCheck ( LOONGARCH64AMode* amCounter,
                                             LOONGARCH64AMode* amFailAddr )
{
   LOONGARCH64Instr* i        = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                     = LAin_EvCheck;
   i->LAin.EvCheck.amCounter  = amCounter;
   i->LAin.EvCheck.amFailAddr = amFailAddr;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_ProfInc ( void )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_ProfInc;
   return i;
}


/* -------- Pretty Print instructions ------------- */

static inline void ppLI ( ULong imm, HReg dst )
{
   vex_printf("li ");
   ppHRegLOONGARCH64(dst);
   vex_printf(", 0x%llx", imm);
}

static inline void ppUnary ( LOONGARCH64UnOp op, HReg src, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64UnOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src);
}

static inline void ppBinary ( LOONGARCH64BinOp op, LOONGARCH64RI* src2,
                              HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64BinOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppLOONGARCH64RI(src2);
}

static inline void ppLoad ( LOONGARCH64LoadOp op, LOONGARCH64AMode* src,
                            HReg dst )
{
   vex_printf("%s ", showLOONGARCH64LoadOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppLOONGARCH64AMode(src);
}

static inline void ppStore ( LOONGARCH64StoreOp op, LOONGARCH64AMode* dst,
                             HReg src )
{
   vex_printf("%s ", showLOONGARCH64StoreOp(op));
   ppHRegLOONGARCH64(src);
   vex_printf(", ");
   ppLOONGARCH64AMode(dst);
}

static inline void ppLLSC ( LOONGARCH64LLSCOp op, LOONGARCH64AMode* addr,
                            HReg val )
{
   vex_printf("%s ", showLOONGARCH64LLSCOp(op));
   ppHRegLOONGARCH64(val);
   vex_printf(", ");
   ppLOONGARCH64AMode(addr);
}

static inline void ppBar ( LOONGARCH64BarOp op, UShort hint )
{
   vex_printf("%s %u", showLOONGARCH64BarOp(op), (UInt)hint);
}

static inline void ppFpUnary ( LOONGARCH64FpUnOp op, HReg src, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpUnOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src);
}

static inline void ppFpBinary ( LOONGARCH64FpBinOp op, HReg src2,
                                HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpBinOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
}

static inline void ppFpTrinary ( LOONGARCH64FpTriOp op, HReg src3,
                                HReg src2, HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpTriOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
   vex_printf(", ");
   ppHRegLOONGARCH64(src3);
}

static inline void ppFpLoad ( LOONGARCH64FpLoadOp op, LOONGARCH64AMode* src,
                              HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpLoadOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppLOONGARCH64AMode(src);
}

static inline void ppFpStore ( LOONGARCH64FpStoreOp op, LOONGARCH64AMode* dst,
                               HReg src )
{
   vex_printf("%s ", showLOONGARCH64FpStoreOp(op));
   ppHRegLOONGARCH64(src);
   vex_printf(", ");
   ppLOONGARCH64AMode(dst);
}

static inline void ppFpMove ( LOONGARCH64FpMoveOp op, HReg src, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpMoveOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src);
}

static inline void ppFpCmp ( LOONGARCH64FpCmpOp op, HReg src2,
                             HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpCmpOp(op));
   vex_printf("$fcc0, ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
   vex_printf("; movcf2gr ");
   ppHRegLOONGARCH64(dst);
   vex_printf(", $fcc0");
}

static inline void ppVecUnary ( LOONGARCH64VecUnOp op, HReg src, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64VecUnOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src);
}

static inline void ppVecBinary ( LOONGARCH64VecBinOp op, LOONGARCH64RI* src2,
                                HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64VecBinOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppLOONGARCH64RI(src2);
}

static inline void ppVecTrinary ( LOONGARCH64VecTriOp op, HReg src3,
                                  HReg src2, HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64VecTriOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
   vex_printf(", ");
   ppHRegLOONGARCH64(src3);
}

static inline void ppVecLoad ( LOONGARCH64VecLoadOp op, LOONGARCH64AMode* src,
                              HReg dst )
{
   vex_printf("%s ", showLOONGARCH64VecLoadOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppLOONGARCH64AMode(src);
}

static inline void ppVecStore ( LOONGARCH64VecStoreOp op, LOONGARCH64AMode* dst,
                               HReg src )
{
   vex_printf("%s ", showLOONGARCH64VecStoreOp(op));
   ppHRegLOONGARCH64(src);
   vex_printf(", ");
   ppLOONGARCH64AMode(dst);
}

static inline void ppVecFpCmp ( LOONGARCH64VecFpCmpOp op,
                                HReg src2, HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64VecFpCmpOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
}

static inline void ppCas ( HReg old, HReg addr, HReg expd,
                           HReg data, Bool size64)
{
   ppHRegLOONGARCH64(old);
   vex_printf(" = cas(%dbit)(", size64 ? 64 : 32);
   ppHRegLOONGARCH64(expd);
   vex_printf(", ");
   ppHRegLOONGARCH64(data);
   vex_printf(" -> ");
   ppHRegLOONGARCH64(addr);
   vex_printf(")");
}

static inline void ppCmp ( LOONGARCH64CondCode cond, HReg src2,
                           HReg src1, HReg dst )
{
   ppHRegLOONGARCH64(dst);
   vex_printf(" = cmp%s(", showLOONGARCH64CondCode(cond));
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
   vex_printf(")");
}

static inline void ppCMove ( HReg cond, HReg r0, HReg r1,
                             HReg dst, Bool isInt )
{
   if (isInt) {
      vex_printf("masknez $t0, ");
      ppHRegLOONGARCH64(r0);
      vex_printf(", ");
      ppHRegLOONGARCH64(cond);
      vex_printf("; maskeqz ");
      ppHRegLOONGARCH64(dst);
      vex_printf(", ");
      ppHRegLOONGARCH64(r1);
      vex_printf(", ");
      ppHRegLOONGARCH64(cond);
      vex_printf("; or ");
      ppHRegLOONGARCH64(dst);
      vex_printf(", $t0, ");
      ppHRegLOONGARCH64(dst);
   } else {
      vex_printf("movgr2cf ");
      ppHRegLOONGARCH64(cond);
      vex_printf(", $fcc0; fsel ");
      ppHRegLOONGARCH64(dst);
      vex_printf(", ");
      ppHRegLOONGARCH64(r0);
      vex_printf(", ");
      ppHRegLOONGARCH64(r1);
      vex_printf(", $fcc0");
   }
}

static inline void ppCall ( HReg cond, Addr64 target,
                            UInt nArgRegs, RetLoc rloc )
{
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("call 0x%llx [nArgRegs=%u, ", target, nArgRegs);
   ppRetLoc(rloc);
   vex_printf("]");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppXDirect ( Addr64 dstGA, LOONGARCH64AMode* amPC,
                               HReg cond, Bool toFastEP )
{
   vex_printf("(xDirect) ");
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("li $t0, 0x%llx; ", (ULong)dstGA);
   vex_printf("st.w $t0, ");
   ppLOONGARCH64AMode(amPC);
   vex_printf("; li $t0, $disp_cp_chain_me_to_%sEP; ",
              toFastEP ? "fast" : "slow");
   vex_printf("jirl $ra, $t0, 0");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppXIndir ( HReg dstGA, LOONGARCH64AMode* amPC,
                              HReg cond )
{
   vex_printf("(xIndir) ");
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("st.w ");
   ppHRegLOONGARCH64(dstGA);
   vex_printf(", ");
   ppLOONGARCH64AMode(amPC);
   vex_printf("; la $t0, disp_indir; ");
   vex_printf("jirl $ra, $t0, 0");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppXAssisted ( HReg dstGA, LOONGARCH64AMode* amPC,
                                 HReg cond, IRJumpKind jk)
{
   vex_printf("(xAssisted) ");
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("st.w ");
   ppHRegLOONGARCH64(dstGA);
   vex_printf(", ");
   ppLOONGARCH64AMode(amPC);
   vex_printf("; li.w $s8, IRJumpKind_to_TRCVAL(%d); ", (Int)jk);
   vex_printf("la $t0, disp_assisted; ");
   vex_printf("jirl $ra, $t0, 0");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppEvCheck ( LOONGARCH64AMode* amCounter,
                               LOONGARCH64AMode* amFailAddr )
{
   vex_printf("(evCheck) ");
   vex_printf("ld.w $t0, ");
   ppLOONGARCH64AMode(amCounter);
   vex_printf("; addi.d $t0, $t0, -1; ");
   vex_printf("st.w $t0, ");
   ppLOONGARCH64AMode(amCounter);
   vex_printf("; bge $t0, $zero, nofail; ");
   vex_printf("ld.d $t0, ");
   ppLOONGARCH64AMode(amFailAddr);
   vex_printf("; jirl $ra, $t0, 0");
   vex_printf("; nofail:");
}

static inline void ppProfInc ( void )
{
   vex_printf("(profInc) ");
   vex_printf("li $t0, NotKnownYet; ");
   vex_printf("ld.d $t1, $t0, 0; ");
   vex_printf("addi.d $t1, $t1, 1; ");
   vex_printf("st.d $t1, $t0, 0;");
}

void ppLOONGARCH64Instr ( const LOONGARCH64Instr* i, Bool mode64 )
{
   vassert(mode64 == True);
   switch (i->tag) {
      case LAin_LI:
         ppLI(i->LAin.LI.imm, i->LAin.LI.dst);
         break;
      case LAin_Un:
         ppUnary(i->LAin.Unary.op, i->LAin.Unary.src, i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         ppBinary(i->LAin.Binary.op, i->LAin.Binary.src2,
                  i->LAin.Binary.src1, i->LAin.Binary.dst);
         break;
      case LAin_Load:
         ppLoad(i->LAin.Load.op, i->LAin.Load.src, i->LAin.Load.dst);
         break;
      case LAin_Store:
         ppStore(i->LAin.Store.op, i->LAin.Store.dst, i->LAin.Store.src);
         break;
      case LAin_LLSC:
         ppLLSC(i->LAin.LLSC.op, i->LAin.LLSC.addr, i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         ppBar(i->LAin.Bar.op, i->LAin.Bar.hint);
         break;
      case LAin_FpUn:
         ppFpUnary(i->LAin.FpUnary.op, i->LAin.FpUnary.src,
                   i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         ppFpBinary(i->LAin.FpBinary.op, i->LAin.FpBinary.src2,
                    i->LAin.FpBinary.src1, i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         ppFpTrinary(i->LAin.FpTrinary.op, i->LAin.FpTrinary.src3,
                     i->LAin.FpTrinary.src2, i->LAin.FpTrinary.src1,
                     i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         ppFpLoad(i->LAin.FpLoad.op, i->LAin.FpLoad.src, i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         ppFpStore(i->LAin.FpStore.op, i->LAin.FpStore.dst,
                   i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         ppFpMove(i->LAin.FpMove.op, i->LAin.FpMove.src,
                   i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         ppFpCmp(i->LAin.FpCmp.op, i->LAin.FpCmp.src2,
                 i->LAin.FpCmp.src1, i->LAin.FpCmp.dst);
         break;
      case LAin_VecUn:
         ppVecUnary(i->LAin.VecUnary.op, i->LAin.VecUnary.src,
                    i->LAin.VecUnary.dst);
         break;
      case LAin_VecBin:
         ppVecBinary(i->LAin.VecBinary.op, i->LAin.VecBinary.src2,
                     i->LAin.VecBinary.src1, i->LAin.VecBinary.dst);
         break;
      case LAin_VecTri:
         ppVecTrinary(i->LAin.VecTrinary.op, i->LAin.VecTrinary.src3,
                      i->LAin.VecTrinary.src2, i->LAin.VecTrinary.src1,
                      i->LAin.VecTrinary.dst);
         break;
      case LAin_VecLoad:
         ppVecLoad(i->LAin.VecLoad.op, i->LAin.VecLoad.src,
                   i->LAin.VecLoad.dst);
         break;
      case LAin_VecStore:
         ppVecStore(i->LAin.VecStore.op, i->LAin.VecStore.dst,
                    i->LAin.VecStore.src);
         break;
      case LAin_VecFpCmp:
         ppVecFpCmp(i->LAin.VecFpCmp.op, i->LAin.VecFpCmp.src2,
                    i->LAin.VecFpCmp.src1, i->LAin.VecFpCmp.dst);
         break;
      case LAin_Cas:
         ppCas(i->LAin.Cas.old, i->LAin.Cas.addr, i->LAin.Cas.expd,
               i->LAin.Cas.data, i->LAin.Cas.size64);
         break;
      case LAin_Cmp:
         ppCmp(i->LAin.Cmp.cond, i->LAin.Cmp.src2,
               i->LAin.Cmp.src1, i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         ppCMove(i->LAin.CMove.cond, i->LAin.CMove.r0,
                 i->LAin.CMove.r1, i->LAin.CMove.dst,
                 i->LAin.CMove.isInt);
         break;
      case LAin_Call:
         ppCall(i->LAin.Call.cond, i->LAin.Call.target,
                i->LAin.Call.nArgRegs, i->LAin.Call.rloc);
         break;
      case LAin_XDirect:
         ppXDirect(i->LAin.XDirect.dstGA, i->LAin.XDirect.amPC,
                   i->LAin.XDirect.cond, i->LAin.XDirect.toFastEP);
         break;
      case LAin_XIndir:
         ppXIndir(i->LAin.XIndir.dstGA, i->LAin.XIndir.amPC,
                  i->LAin.XIndir.cond);
         break;
      case LAin_XAssisted:
         ppXAssisted(i->LAin.XAssisted.dstGA, i->LAin.XAssisted.amPC,
                     i->LAin.XAssisted.cond, i->LAin.XAssisted.jk);
         break;
      case LAin_EvCheck:
         ppEvCheck(i->LAin.EvCheck.amCounter, i->LAin.EvCheck.amFailAddr);
         break;
      case LAin_ProfInc:
         ppProfInc();
         break;
      default:
         vpanic("ppLOONGARCH64Instr");
         break;
   }
}


/* --------- Helpers for register allocation. --------- */

void getRegUsage_LOONGARCH64Instr ( HRegUsage* u, const LOONGARCH64Instr* i,
                                    Bool mode64 )
{
   vassert(mode64 == True);
   initHRegUsage(u);
   switch (i->tag) {
      case LAin_LI:
         addHRegUse(u, HRmWrite, i->LAin.LI.dst);
         break;
      case LAin_Un:
         addHRegUse(u, HRmRead, i->LAin.Unary.src);
         addHRegUse(u, HRmWrite, i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         addRegUsage_LOONGARCH64RI(u, i->LAin.Binary.src2);
         addHRegUse(u, HRmRead, i->LAin.Binary.src1);
         addHRegUse(u, HRmWrite, i->LAin.Binary.dst);
         break;
      case LAin_Load:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.Load.src);
         addHRegUse(u, HRmWrite, i->LAin.Load.dst);
         break;
      case LAin_Store:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.Store.dst);
         addHRegUse(u, HRmRead, i->LAin.Store.src);
         break;
      case LAin_LLSC:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.LLSC.addr);
         if (i->LAin.LLSC.isLoad)
            addHRegUse(u, HRmWrite, i->LAin.LLSC.val);
         else
            addHRegUse(u, HRmRead, i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         /* No regs. */
         break;
      case LAin_FpUn:
         addHRegUse(u, HRmRead, i->LAin.FpUnary.src);
         addHRegUse(u, HRmWrite, i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         addHRegUse(u, HRmRead, i->LAin.FpBinary.src2);
         addHRegUse(u, HRmRead, i->LAin.FpBinary.src1);
         addHRegUse(u, HRmWrite, i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src3);
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src2);
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src1);
         addHRegUse(u, HRmWrite, i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.FpLoad.src);
         addHRegUse(u, HRmWrite, i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.FpStore.dst);
         addHRegUse(u, HRmRead, i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         addHRegUse(u, HRmRead, i->LAin.FpMove.src);
         addHRegUse(u, HRmWrite, i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         addHRegUse(u, HRmRead, i->LAin.FpCmp.src2);
         addHRegUse(u, HRmRead, i->LAin.FpCmp.src1);
         addHRegUse(u, HRmWrite, i->LAin.FpCmp.dst);
         break;
      case LAin_VecUn:
         addHRegUse(u, HRmRead, i->LAin.VecUnary.src);
         addHRegUse(u, HRmWrite, i->LAin.VecUnary.dst);
         break;
      case LAin_VecBin:
         addRegUsage_LOONGARCH64RI(u, i->LAin.VecBinary.src2);
         addHRegUse(u, HRmRead, i->LAin.VecBinary.src1);
         addHRegUse(u, HRmWrite, i->LAin.VecBinary.dst);
         break;
      case LAin_VecTri:
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src3);
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src2);
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src1);
         addHRegUse(u, HRmWrite, i->LAin.FpTrinary.dst);
         break;
      case LAin_VecLoad:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.VecLoad.src);
         addHRegUse(u, HRmWrite, i->LAin.VecLoad.dst);
         break;
      case LAin_VecStore:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.VecStore.dst);
         addHRegUse(u, HRmRead, i->LAin.VecStore.src);
         break;
      case LAin_VecFpCmp:
         addHRegUse(u, HRmRead, i->LAin.VecFpCmp.src2);
         addHRegUse(u, HRmRead, i->LAin.VecFpCmp.src1);
         addHRegUse(u, HRmWrite, i->LAin.VecFpCmp.dst);
         break;
      case LAin_Cas:
         addHRegUse(u, HRmWrite, i->LAin.Cas.old);
         addHRegUse(u, HRmRead, i->LAin.Cas.addr);
         addHRegUse(u, HRmRead, i->LAin.Cas.expd);
         addHRegUse(u, HRmModify, i->LAin.Cas.data);
         break;
      case LAin_Cmp:
         addHRegUse(u, HRmRead, i->LAin.Cmp.src2);
         addHRegUse(u, HRmRead, i->LAin.Cmp.src1);
         addHRegUse(u, HRmWrite, i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         addHRegUse(u, HRmRead, i->LAin.CMove.cond);
         addHRegUse(u, HRmRead, i->LAin.CMove.r0);
         addHRegUse(u, HRmRead, i->LAin.CMove.r1);
         addHRegUse(u, HRmWrite, i->LAin.CMove.dst);
         break;
      case LAin_Call:
         /* logic and comments copied/modified from mips and arm64 back end */
         /* This is a bit subtle. */
         /* First off, we need to consider the cond register. */
         if (!hregIsInvalid(i->LAin.Call.cond))
            addHRegUse(u, HRmRead, i->LAin.Call.cond);
         /* Then, claim it trashes all the caller-saved regs
            which fall within the register allocator's jurisdiction. */
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R14());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R15());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R16());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R17());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R18());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R19());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R20());
         /* Now we have to state any parameter-carrying registers
            which might be read.  This depends on nArgRegs. */
            switch (i->LAin.Call.nArgRegs) {
            case 8: addHRegUse(u, HRmRead, hregLOONGARCH64_R11()); /* fallthrough */
            case 7: addHRegUse(u, HRmRead, hregLOONGARCH64_R10()); /* fallthrough */
            case 6: addHRegUse(u, HRmRead, hregLOONGARCH64_R9());  /* fallthrough */
            case 5: addHRegUse(u, HRmRead, hregLOONGARCH64_R8());  /* fallthrough */
            case 4: addHRegUse(u, HRmRead, hregLOONGARCH64_R7());  /* fallthrough */
            case 3: addHRegUse(u, HRmRead, hregLOONGARCH64_R6());  /* fallthrough */
            case 2: addHRegUse(u, HRmRead, hregLOONGARCH64_R5());  /* fallthrough */
            case 1: addHRegUse(u, HRmRead, hregLOONGARCH64_R4());  /* fallthrough */
            case 0: break;
            default: vpanic("getRegUsage_LOONGARCH64:Call:regparms"); break;
         }
         /* Finally, there is the issue that the insn trashes a
            register because the literal target address has to be
            loaded into a register.  However, we reserve $t0 for that
            purpose so there's no further complexity here.  Stating $t0
            as trashed is pointless since it's not under the control
            of the allocator, but what the hell. */
         addHRegUse(u, HRmWrite, hregT0());
         break;
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case LAin_XDirect:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.XDirect.amPC);
         if (!hregIsInvalid(i->LAin.XDirect.cond))
            addHRegUse(u, HRmRead, i->LAin.XDirect.cond);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_XIndir:
         addHRegUse(u, HRmRead, i->LAin.XIndir.dstGA);
         addRegUsage_LOONGARCH64AMode(u, i->LAin.XIndir.amPC);
         if (!hregIsInvalid(i->LAin.XIndir.cond))
            addHRegUse(u, HRmRead, i->LAin.XIndir.cond);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_XAssisted:
         addHRegUse(u, HRmRead, i->LAin.XAssisted.dstGA);
         addRegUsage_LOONGARCH64AMode(u, i->LAin.XAssisted.amPC);
         if (!hregIsInvalid(i->LAin.XAssisted.cond))
            addHRegUse(u, HRmRead, i->LAin.XAssisted.cond);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_EvCheck:
         /* We expect both amodes only to mention $r31, so this is in
            fact pointless, since $r31 isn't allocatable, but anyway.. */
         addRegUsage_LOONGARCH64AMode(u, i->LAin.EvCheck.amCounter);
         addRegUsage_LOONGARCH64AMode(u, i->LAin.EvCheck.amFailAddr);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_ProfInc:
         /* Again, pointless to actually state these since neither
            is available to RA. */
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         addHRegUse(u, HRmWrite, hregT1()); /* unavail to RA */
         break;
      default:
         ppLOONGARCH64Instr(i, mode64);
         vpanic("getRegUsage_LOONGARCH64Instr");
         break;
   }
}

void mapRegs_LOONGARCH64Instr ( HRegRemap* m, LOONGARCH64Instr* i,
                                Bool mode64 )
{
   vassert(mode64 == True);
   switch (i->tag) {
      case LAin_LI:
         mapReg(m, &i->LAin.LI.dst);
         break;
      case LAin_Un:
         mapReg(m, &i->LAin.Unary.src);
         mapReg(m, &i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         mapRegs_LOONGARCH64RI(m, i->LAin.Binary.src2);
         mapReg(m, &i->LAin.Binary.src1);
         mapReg(m, &i->LAin.Binary.dst);
         break;
      case LAin_Load:
         mapRegs_LOONGARCH64AMode(m, i->LAin.Load.src);
         mapReg(m, &i->LAin.Load.dst);
         break;
      case LAin_Store:
         mapRegs_LOONGARCH64AMode(m, i->LAin.Store.dst);
         mapReg(m, &i->LAin.Store.src);
         break;
      case LAin_LLSC:
         mapRegs_LOONGARCH64AMode(m, i->LAin.LLSC.addr);
         mapReg(m, &i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         /* No regs. */
         break;
      case LAin_FpUn:
         mapReg(m, &i->LAin.FpUnary.src);
         mapReg(m, &i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         mapReg(m, &i->LAin.FpBinary.src2);
         mapReg(m, &i->LAin.FpBinary.src1);
         mapReg(m, &i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         mapReg(m, &i->LAin.FpTrinary.src3);
         mapReg(m, &i->LAin.FpTrinary.src2);
         mapReg(m, &i->LAin.FpTrinary.src1);
         mapReg(m, &i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         mapRegs_LOONGARCH64AMode(m, i->LAin.FpLoad.src);
         mapReg(m, &i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         mapRegs_LOONGARCH64AMode(m, i->LAin.FpStore.dst);
         mapReg(m, &i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         mapReg(m, &i->LAin.FpMove.src);
         mapReg(m, &i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         mapReg(m, &i->LAin.FpCmp.src2);
         mapReg(m, &i->LAin.FpCmp.src1);
         mapReg(m, &i->LAin.FpCmp.dst);
         break;
      case LAin_VecUn:
         mapReg(m, &i->LAin.VecUnary.src);
         mapReg(m, &i->LAin.VecUnary.dst);
         break;
      case LAin_VecBin:
         mapRegs_LOONGARCH64RI(m, i->LAin.VecBinary.src2);
         mapReg(m, &i->LAin.VecBinary.src1);
         mapReg(m, &i->LAin.VecBinary.dst);
         break;
      case LAin_VecTri:
         mapReg(m, &i->LAin.VecTrinary.src3);
         mapReg(m, &i->LAin.VecTrinary.src2);
         mapReg(m, &i->LAin.VecTrinary.src1);
         mapReg(m, &i->LAin.VecTrinary.dst);
         break;
      case LAin_VecLoad:
         mapRegs_LOONGARCH64AMode(m, i->LAin.VecLoad.src);
         mapReg(m, &i->LAin.VecLoad.dst);
         break;
      case LAin_VecStore:
         mapRegs_LOONGARCH64AMode(m, i->LAin.VecStore.dst);
         mapReg(m, &i->LAin.VecStore.src);
         break;
      case LAin_VecFpCmp:
         mapReg(m, &i->LAin.VecFpCmp.src2);
         mapReg(m, &i->LAin.VecFpCmp.src1);
         mapReg(m, &i->LAin.VecFpCmp.dst);
         break;
      case LAin_Cas:
         mapReg(m, &i->LAin.Cas.old);
         mapReg(m, &i->LAin.Cas.addr);
         mapReg(m, &i->LAin.Cas.expd);
         mapReg(m, &i->LAin.Cas.data);
         break;
      case LAin_Cmp:
         mapReg(m, &i->LAin.Cmp.src2);
         mapReg(m, &i->LAin.Cmp.src1);
         mapReg(m, &i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         mapReg(m, &i->LAin.CMove.cond);
         mapReg(m, &i->LAin.CMove.r0);
         mapReg(m, &i->LAin.CMove.r1);
         mapReg(m, &i->LAin.CMove.dst);
         break;
      case LAin_Call:
         if (!hregIsInvalid(i->LAin.Call.cond))
            mapReg(m, &i->LAin.Call.cond);
         /* Hardwires $r12. */
         break;
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case LAin_XDirect:
         mapRegs_LOONGARCH64AMode(m, i->LAin.XDirect.amPC);
         if (!hregIsInvalid(i->LAin.XDirect.cond))
            mapReg(m, &i->LAin.XDirect.cond);
         break;
      case LAin_XIndir:
         mapReg(m, &i->LAin.XIndir.dstGA);
         mapRegs_LOONGARCH64AMode(m, i->LAin.XIndir.amPC);
         if (!hregIsInvalid(i->LAin.XIndir.cond))
            mapReg(m, &i->LAin.XIndir.cond);
         break;
      case LAin_XAssisted:
         mapReg(m, &i->LAin.XAssisted.dstGA);
         mapRegs_LOONGARCH64AMode(m, i->LAin.XAssisted.amPC);
         if (!hregIsInvalid(i->LAin.XAssisted.cond))
            mapReg(m, &i->LAin.XAssisted.cond);
         break;
      case LAin_EvCheck:
         /* We expect both amodes only to mention $r31, so this is in
            fact pointless, since $r31 isn't allocatable, but anyway.. */
         mapRegs_LOONGARCH64AMode(m, i->LAin.EvCheck.amCounter);
         mapRegs_LOONGARCH64AMode(m, i->LAin.EvCheck.amFailAddr);
         break;
      case LAin_ProfInc:
         /* Hardwires $r12 and $r13 -- nothing to modify. */
         break;
      default:
         ppLOONGARCH64Instr(i, mode64);
         vpanic("mapRegs_LOONGARCH64Instr");
         break;
   }
}

/* Generate loongarch64 spill instructions under the direction of the
   register allocator. */
void genSpill_LOONGARCH64 ( /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2,
                            HReg rreg, Int offsetB, Bool mode64 )
{
   vassert(mode64 == True);
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));

   LOONGARCH64AMode* am;
   *i1 = *i2 = NULL;

   switch (hregClass(rreg)) {
      case HRcInt64:
         if (offsetB < 1024) {
            am = LOONGARCH64AMode_RI(hregGSP(), offsetB);
            *i1 = LOONGARCH64Instr_Store(LAstore_ST_D, am, rreg);
         } else {
            am = LOONGARCH64AMode_RR(hregGSP(), hregT0());
            *i1 = LOONGARCH64Instr_LI(offsetB, hregT0());
            *i2 = LOONGARCH64Instr_Store(LAstore_STX_D, am, rreg);
         }
         break;
      case HRcFlt64:
         if (offsetB < 1024) {
            am = LOONGARCH64AMode_RI(hregGSP(), offsetB);
            *i1 = LOONGARCH64Instr_FpStore(LAfpstore_FST_D, am, rreg);
         } else {
            am = LOONGARCH64AMode_RR(hregGSP(), hregT0());
            *i1 = LOONGARCH64Instr_LI(offsetB, hregT0());
            *i2 = LOONGARCH64Instr_FpStore(LAfpstore_FSTX_D, am, rreg);
         }
         break;
      case HRcVec128:
         if (offsetB < 1024) {
            am = LOONGARCH64AMode_RI(hregGSP(), offsetB);
            *i1 = LOONGARCH64Instr_VecStore(LAvecstore_VST, am, rreg);
         } else {
            am = LOONGARCH64AMode_RR(hregGSP(), hregT0());
            *i1 = LOONGARCH64Instr_LI(offsetB, hregT0());
            *i2 = LOONGARCH64Instr_VecStore(LAvecstore_VSTX, am, rreg);
         }
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_LOONGARCH64: unimplemented regclass");
         break;
   }
}

/* Generate loongarch64 reload instructions under the direction of the
   register allocator. */
void genReload_LOONGARCH64 ( /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2,
                             HReg rreg, Int offsetB, Bool mode64 )
{
   vassert(mode64 == True);
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));

   LOONGARCH64AMode* am;
   *i1 = *i2 = NULL;

   switch (hregClass(rreg)) {
      case HRcInt64:
         if (offsetB < 1024) {
            am = LOONGARCH64AMode_RI(hregGSP(), offsetB);
            *i1 = LOONGARCH64Instr_Load(LAload_LD_D, am, rreg);
         } else {
            am = LOONGARCH64AMode_RR(hregGSP(), hregT0());
            *i1 = LOONGARCH64Instr_LI(offsetB, hregT0());
            *i2 = LOONGARCH64Instr_Load(LAload_LDX_D, am, rreg);
         }
         break;
      case HRcFlt64:
         if (offsetB < 1024) {
            am = LOONGARCH64AMode_RI(hregGSP(), offsetB);
            *i1 = LOONGARCH64Instr_FpLoad(LAfpload_FLD_D, am, rreg);
         } else {
            am = LOONGARCH64AMode_RR(hregGSP(), hregT0());
            *i1 = LOONGARCH64Instr_LI(offsetB, hregT0());
            *i2 = LOONGARCH64Instr_FpLoad(LAfpload_FLDX_D, am, rreg);
         }
         break;
      case HRcVec128:
         if (offsetB < 1024) {
            am = LOONGARCH64AMode_RI(hregGSP(), offsetB);
            *i1 = LOONGARCH64Instr_VecLoad(LAvecload_VLD, am, rreg);
         } else {
            am = LOONGARCH64AMode_RR(hregGSP(), hregT0());
            *i1 = LOONGARCH64Instr_LI(offsetB, hregT0());
            *i2 = LOONGARCH64Instr_VecLoad(LAvecload_VLDX, am, rreg);
         }
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_LOONGARCH64: unimplemented regclass");
         break;
   }
}

/* Generate loongarch64 move instructions under the direction of the
   register allocator. */
LOONGARCH64Instr* genMove_LOONGARCH64 ( HReg from, HReg to, Bool mode64 )
{
   vassert(mode64 == True);
   switch (hregClass(from)) {
      case HRcInt64:
         return LOONGARCH64Instr_Binary(LAbin_OR,
                                        LOONGARCH64RI_R(hregZERO()),
                                        from, to);
      case HRcFlt64:
         return LOONGARCH64Instr_FpMove(LAfpmove_FMOV_D, from, to);
      case HRcVec128:
         return LOONGARCH64Instr_VecBinary(LAvecbin_VORI_B,
                                           LOONGARCH64RI_I(0, 8, False),
                                           from, to);
      default:
         ppHRegClass(hregClass(from));
         vpanic("genMove_LOONGARCH64: unimplemented regclass");
   }
}


/* --------- The loongarch64 assembler --------- */

static inline UInt iregEnc ( HReg r )
{
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt fregEnc ( HReg r )
{
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt vregEnc ( HReg r )
{
   vassert(hregClass(r) == HRcVec128);
   vassert(!hregIsVirtual(r));
   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt fcsrEnc ( HReg r )
{
   vassert(hregClass(r) == HRcInt32);
   vassert(!hregIsVirtual(r));
   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt emit_op_rj_rd ( UInt op, UInt rj, UInt rd )
{
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (rj << 5) | rd;
}

static inline UInt emit_op_rk_rj_rd ( UInt op, UInt rk, UInt rj, UInt rd )
{
   vassert(rk < (1 << 5));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (rk << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_fj_fd ( UInt op, UInt fj, UInt fd )
{
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (fj << 5) | fd;
}

static inline UInt emit_op_fa_fk_fj_fd ( UInt op, UInt fa, UInt fk, UInt fj, UInt fd )
{
   vassert(fa < (1 << 5));
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (fa << 15) | (fk << 10) | (fj << 5) | fd;
}

static inline UInt emit_op_fk_fj_fd ( UInt op, UInt fk, UInt fj, UInt fd )
{
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (fk << 10) | (fj << 5) | fd;
}

static inline UInt emit_op_ca_fk_fj_fd ( UInt op, UInt ca, UInt fk, UInt fj, UInt fd )
{
   vassert(ca < (1 << 3));
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (ca << 15) | (fk << 10) | (fj << 5) | fd;
}

static inline UInt emit_op_fk_fj_cd ( UInt op, UInt fk, UInt fj, UInt cd )
{
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(cd < (1 << 3));
   return op | (fk << 10) | (fj << 5) | cd;
}

static inline UInt emit_op_cj_rd ( UInt op, UInt cj, UInt rd )
{
   vassert(cj < (1 << 3));
   vassert(rd < (1 << 5));
   return op | (cj << 5) | rd;
}

static inline UInt emit_op_rj_cd ( UInt op, UInt rj, UInt cd )
{
   vassert(rj < (1 << 5));
   vassert(cd < (1 << 3));
   return op | (rj << 5) | cd;
}

static inline UInt emit_op_rj_fd ( UInt op, UInt rj, UInt fd )
{
   vassert(rj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (rj << 5) | fd;
}

static inline UInt emit_op_fj_rd ( UInt op, UInt fj, UInt rd )
{
   vassert(fj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (fj << 5) | rd;
}

static inline UInt emit_op_rj_fcsr ( UInt op, UInt rj, UInt fcsr )
{
   vassert(rj < (1 << 5));
   vassert(fcsr < (1 << 5));
   return op | (rj << 5) | fcsr;
}

static inline UInt emit_op_fcsr_rd ( UInt op, UInt fcsr, UInt rd )
{
   vassert(fcsr < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (fcsr << 5) | rd;
}

static inline UInt emit_op_ui5_rj_rd ( UInt op, UInt ui5, UInt rj, UInt rd )
{
   vassert(ui5 < (1 << 5));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (ui5 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_ui6_rj_rd ( UInt op, UInt ui6, UInt rj, UInt rd )
{
   vassert(ui6 < (1 << 6));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (ui6 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_ui12_rj_rd ( UInt op, UInt ui12, UInt rj, UInt rd )
{
   vassert(ui12 < (1 << 12));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (ui12 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_si12_rj_rd ( UInt op, UInt si12, UInt rj, UInt rd )
{
   vassert(si12 < (1 << 12));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (si12 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_si14_rj_rd ( UInt op, UInt si14, UInt rj, UInt rd )
{
   vassert(si14 < (1 << 14));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (si14 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_si20_rd ( UInt op, UInt si20, UInt rd )
{
   vassert(si20 < (1 << 20));
   vassert(rd < (1 << 5));
   return op | (si20 << 5) | rd;
}

static inline UInt emit_op_offs16_rj_rd ( UInt op, UInt offs16, UInt rj, UInt rd )
{
   vassert(offs16 < (1 << 16));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (offs16 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_offs26 ( UInt op, UInt offs26 )
{
   vassert(offs26 < (1 << 26));
   return op | ((offs26 & 0xffff) << 10) | (offs26 >> 16);
}

static inline UInt emit_op_hint15 ( UInt op, UInt hint )
{
   vassert(hint < (1 << 15));
   return op | hint;
}

static inline UInt emit_op_si13_vd ( UInt op, UInt si13, UInt dst )
{
   vassert(si13 < (1 << 13));
   vassert(dst < (1 << 5));
   return op | (si13 << 5) | dst;
}

static inline UInt emit_unop_r ( UInt op, Int src, UInt dst, UInt dst_size)
{
   vassert(src < (1 << 5));
   vassert(dst < (1 << dst_size));
   return op | (src << 5) | dst;
}

static inline UInt emit_binop_rr ( UInt op, UInt src2, UInt src1, UInt dst )
{
   vassert(src2 < (1 << 5));
   vassert(src1 < (1 << 5));
   vassert(dst < (1 << 5));
   return op | (src2 << 10) | (src1 << 5) | dst;
}

static inline UInt emit_binop_ri ( UInt op, UInt imm, UInt size, UInt src, UInt dst )
{
   vassert(imm < (1 << size));
   vassert(src < (1 << 5));
   vassert(dst < (1 << 5));
   return op | (imm << 10) | (src << 5) | dst;
}

static inline UInt emit_storeop_rii ( UInt op, UInt idx, UInt idx_size,
                                      UInt imm, UInt imm_size, UInt src, UInt dst )
{
   vassert(idx < (1 << idx_size));
   vassert(imm < (1 << imm_size));
   vassert(src < (1 << 5));
   vassert(dst < (1 << 5));
   return op | (idx << (10 + imm_size )) |(imm << 10) | (src << 5) | dst;
}

static inline UInt emit_triop_rrr ( UInt op, UInt src3, UInt src2, UInt src1, UInt dst )
{
   vassert(src3 < (1 << 5));
   vassert(src2 < (1 << 5));
   vassert(src1 < (1 << 5));
   vassert(dst < (1 << 5));
   return op | (src3 << 15) | (src2 << 10) | (src1 << 5) | dst;
}

static UInt* mkLoadImm_EXACTLY4 ( UInt* p, HReg dst, ULong imm )
{
   /*
      lu12i.w dst, imm[31:12]
      ori     dst, dst, imm[11:0]
      lu32i.d dst, imm[51:32]
      lu52i.d dst, dst, imm[63:52]
    */
   UInt d = iregEnc(dst);
   *p++ = emit_op_si20_rd(LAextra_LU12I_W, (imm >> 12) & 0xfffff, d);
   *p++ = emit_op_si12_rj_rd(LAbin_ORI, imm & 0xfff, d, d);
   *p++ = emit_op_si20_rd(LAextra_LU32I_D, (imm >> 32) & 0xfffff, d);
   *p++ = emit_op_si12_rj_rd(LAextra_LU52I_D, (imm >> 52) & 0xfff, d, d);
   return p;
}

static inline UInt* mkLoadImm_EXACTLY2 ( UInt* p, HReg dst, ULong imm )
{
   /*
      lu12i.w dst, imm[31:12]
      ori     dst, dst, imm[11:0]
    */
   UInt d = iregEnc(dst);
   *p++ = emit_op_si20_rd(LAextra_LU12I_W, (imm >> 12) & 0xfffff, d);
   *p++ = emit_op_si12_rj_rd(LAbin_ORI, imm & 0xfff, d, d);
   return p;
}

static inline UInt* mkLoadImm_EXACTLY1 ( UInt* p, HReg dst, ULong imm )
{
   /* ori dst, $zero, imm[11:0] */
   *p++ = emit_op_si12_rj_rd(LAbin_ORI, imm, 0, iregEnc(dst));
   return p;
}

static UInt* mkLoadImm ( UInt* p, HReg dst, ULong imm )
{
   if ((imm >> 12) == 0)
      p = mkLoadImm_EXACTLY1(p, dst, imm);
   else if (imm < 0x80000000 || (imm >> 31) == 0x1ffffffffUL)
      p = mkLoadImm_EXACTLY2(p, dst, imm);
   else
      p = mkLoadImm_EXACTLY4(p, dst, imm);
   return p;
}

static Bool is_LoadImm_EXACTLY4 ( UInt* p, HReg dst, ULong imm )
{
   UInt expect[4];
   mkLoadImm_EXACTLY4(expect, dst, imm);
   return toBool(p[0] == expect[0] && p[1] == expect[1] &&
                 p[2] == expect[2] && p[3] == expect[3]);
}

static inline UInt* mkUnary ( UInt* p, LOONGARCH64UnOp op, HReg src, HReg dst )
{
   switch (op) {
      case LAun_CLZ_W:
      case LAun_CTZ_W:
      case LAun_CLZ_D:
      case LAun_CTZ_D:
      case LAun_EXT_W_H:
      case LAun_EXT_W_B:
         *p++ = emit_op_rj_rd(op, iregEnc(src), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkBinary ( UInt* p, LOONGARCH64BinOp op,
                               LOONGARCH64RI* src2, HReg src1, HReg dst )
{
   switch (op) {
      case LAbin_ADD_W:
      case LAbin_ADD_D:
      case LAbin_SUB_W:
      case LAbin_SUB_D:
      case LAbin_NOR:
      case LAbin_AND:
      case LAbin_OR:
      case LAbin_XOR:
      case LAbin_SLL_W:
      case LAbin_SRL_W:
      case LAbin_SRA_W:
      case LAbin_SLL_D:
      case LAbin_SRL_D:
      case LAbin_SRA_D:
      case LAbin_MUL_W:
      case LAbin_MUL_D:
      case LAbin_MULH_W:
      case LAbin_MULH_WU:
      case LAbin_MULH_D:
      case LAbin_MULH_DU:
      case LAbin_MULW_D_W:
      case LAbin_MULW_D_WU:
      case LAbin_DIV_W:
      case LAbin_MOD_W:
      case LAbin_DIV_WU:
      case LAbin_MOD_WU:
      case LAbin_DIV_D:
      case LAbin_MOD_D:
      case LAbin_DIV_DU:
      case LAbin_MOD_DU:
         vassert(src2->tag == LAri_Reg);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(src2->LAri.R.reg),
                                 iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_SLLI_W:
      case LAbin_SRLI_W:
      case LAbin_SRAI_W:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_ui5_rj_rd(op, src2->LAri.I.imm,
                                  iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_SLLI_D:
      case LAbin_SRLI_D:
      case LAbin_SRAI_D:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_ui6_rj_rd(op, src2->LAri.I.imm,
                                  iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_ADDI_W:
      case LAbin_ADDI_D:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_si12_rj_rd(op, src2->LAri.I.imm,
                                   iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_ANDI:
      case LAbin_ORI:
      case LAbin_XORI:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_ui12_rj_rd(op, src2->LAri.I.imm,
                                   iregEnc(src1), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static UInt* mkLoad ( UInt* p, LOONGARCH64LoadOp op,
                      LOONGARCH64AMode* src, HReg dst )
{
   switch (op) {
      case LAload_LD_W:
      case LAload_LD_D:
      case LAload_LD_BU:
      case LAload_LD_HU:
      case LAload_LD_WU:
         vassert(src->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, src->LAam.RI.index,
                                   iregEnc(src->LAam.RI.base), iregEnc(dst));
         return p;
      case LAload_LDX_D:
      case LAload_LDX_BU:
      case LAload_LDX_HU:
      case LAload_LDX_WU:
         vassert(src->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(src->LAam.RR.index),
                                 iregEnc(src->LAam.RR.base), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static UInt* mkStore ( UInt* p, LOONGARCH64StoreOp op,
                       LOONGARCH64AMode* dst, HReg src )
{
   switch (op) {
      case LAstore_ST_B:
      case LAstore_ST_H:
      case LAstore_ST_W:
      case LAstore_ST_D:
         vassert(dst->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, dst->LAam.RI.index,
                                   iregEnc(dst->LAam.RI.base), iregEnc(src));
         return p;
      case LAstore_STX_B:
      case LAstore_STX_H:
      case LAstore_STX_W:
      case LAstore_STX_D:
         vassert(dst->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(dst->LAam.RR.index),
                                 iregEnc(dst->LAam.RR.base), iregEnc(src));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkLLSC ( UInt* p, LOONGARCH64LLSCOp op,
                             LOONGARCH64AMode* addr, HReg val )
{
   switch (op) {
      case LAllsc_LL_W:
      case LAllsc_SC_W:
      case LAllsc_LL_D:
      case LAllsc_SC_D:
         vassert(addr->tag == LAam_RI);
         *p++ = emit_op_si14_rj_rd(op, addr->LAam.RI.index,
                                   iregEnc(addr->LAam.RI.base), iregEnc(val));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkBar ( UInt* p, LOONGARCH64BarOp op, UShort hint )
{
   switch (op) {
      case LAbar_DBAR:
      case LAbar_IBAR:
         *p++ = emit_op_hint15(op, hint);
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpUnary ( UInt* p, LOONGARCH64FpUnOp op, HReg src, HReg dst )
{
   switch (op) {
      case LAfpun_FABS_S:
      case LAfpun_FABS_D:
      case LAfpun_FNEG_S:
      case LAfpun_FNEG_D:
      case LAfpun_FLOGB_S:
      case LAfpun_FLOGB_D:
      case LAfpun_FSQRT_S:
      case LAfpun_FSQRT_D:
      case LAfpun_FRSQRT_S:
      case LAfpun_FRSQRT_D:
      case LAfpun_FCVT_S_D:
      case LAfpun_FCVT_D_S:
      case LAfpun_FTINT_W_S:
      case LAfpun_FTINT_W_D:
      case LAfpun_FTINT_L_S:
      case LAfpun_FTINT_L_D:
      case LAfpun_FFINT_S_W:
      case LAfpun_FFINT_S_L:
      case LAfpun_FFINT_D_W:
      case LAfpun_FFINT_D_L:
      case LAfpun_FRINT_S:
      case LAfpun_FRINT_D:
         *p++ = emit_op_fj_fd(op, fregEnc(src), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpBinary ( UInt* p, LOONGARCH64FpBinOp op, HReg src2,
                                 HReg src1, HReg dst )
{
   switch (op) {
      case LAfpbin_FADD_S:
      case LAfpbin_FADD_D:
      case LAfpbin_FSUB_S:
      case LAfpbin_FSUB_D:
      case LAfpbin_FMUL_S:
      case LAfpbin_FMUL_D:
      case LAfpbin_FDIV_S:
      case LAfpbin_FDIV_D:
      case LAfpbin_FMAX_S:
      case LAfpbin_FMAX_D:
      case LAfpbin_FMIN_S:
      case LAfpbin_FMIN_D:
      case LAfpbin_FMAXA_S:
      case LAfpbin_FMAXA_D:
      case LAfpbin_FMINA_S:
      case LAfpbin_FMINA_D:
      case LAfpbin_FSCALEB_S:
      case LAfpbin_FSCALEB_D:
         *p++ = emit_op_fk_fj_fd(op, fregEnc(src2), fregEnc(src1), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpTrinary ( UInt* p, LOONGARCH64FpTriOp op, HReg src3,
                                  HReg src2, HReg src1, HReg dst )
{
   switch (op) {
      case LAfpbin_FMADD_S:
      case LAfpbin_FMADD_D:
      case LAfpbin_FMSUB_S:
      case LAfpbin_FMSUB_D:
         *p++ = emit_op_fa_fk_fj_fd(op, fregEnc(src3), fregEnc(src2),
                                    fregEnc(src1), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpLoad ( UInt* p, LOONGARCH64FpLoadOp op,
                               LOONGARCH64AMode* src, HReg dst )
{
   switch (op) {
      case LAfpload_FLD_S:
      case LAfpload_FLD_D:
         vassert(src->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, src->LAam.RI.index,
                                   iregEnc(src->LAam.RI.base), fregEnc(dst));
         return p;
      case LAfpload_FLDX_S:
      case LAfpload_FLDX_D:
         vassert(src->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(src->LAam.RR.index),
                                 iregEnc(src->LAam.RR.base), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpStore ( UInt* p, LOONGARCH64FpStoreOp op,
                                LOONGARCH64AMode* dst, HReg src )
{
   switch (op) {
      case LAfpstore_FST_S:
      case LAfpstore_FST_D:
         vassert(dst->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, dst->LAam.RI.index,
                                   iregEnc(dst->LAam.RI.base), fregEnc(src));
         return p;
      case LAfpstore_FSTX_S:
      case LAfpstore_FSTX_D:
         vassert(dst->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(dst->LAam.RR.index),
                                 iregEnc(dst->LAam.RR.base), fregEnc(src));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpMove ( UInt* p, LOONGARCH64FpMoveOp op, HReg src, HReg dst )
{
   switch (op) {
      case LAfpmove_FMOV_S:
      case LAfpmove_FMOV_D:
         *p++ = emit_op_fj_fd(op, fregEnc(src), fregEnc(dst));
         return p;
      case LAfpmove_MOVGR2FR_W:
      case LAfpmove_MOVGR2FR_D:
         *p++ = emit_op_rj_fd(op, iregEnc(src), fregEnc(dst));
         return p;
      case LAfpmove_MOVFR2GR_S:
      case LAfpmove_MOVFR2GR_D:
         *p++ = emit_op_fj_rd(op, fregEnc(src), iregEnc(dst));
         return p;
      case LAfpmove_MOVGR2FCSR:
         *p++ = emit_op_rj_fcsr(op, iregEnc(src), fcsrEnc(dst));
         return p;
      case LAfpmove_MOVFCSR2GR:
         *p++ = emit_op_fcsr_rd(op, fcsrEnc(src), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpCmp ( UInt* p, LOONGARCH64FpCmpOp op, HReg src2,
                              HReg src1, HReg dst )
{
   /*
      fcmp.cond.[sd] $fcc0, src1, src2
      movcf2gr       dst, $fcc0
    */
   switch (op) {
      case LAfpcmp_FCMP_CLT_S:
      case LAfpcmp_FCMP_CLT_D:
      case LAfpcmp_FCMP_CEQ_S:
      case LAfpcmp_FCMP_CEQ_D:
      case LAfpcmp_FCMP_CUN_S:
      case LAfpcmp_FCMP_CUN_D:
         *p++ = emit_op_fk_fj_cd(op, fregEnc(src2), fregEnc(src1), 0);
         *p++ = emit_op_cj_rd(LAextra_MOVCF2GR, 0, iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkVecUnary ( UInt* p, LOONGARCH64VecUnOp op, HReg src, HReg dst )
{
   switch (op) {
      case LAvecun_VCLO_B: case LAvecun_VCLO_H: case LAvecun_VCLO_W: case LAvecun_VCLO_D:
      case LAvecun_VCLZ_B: case LAvecun_VCLZ_H: case LAvecun_VCLZ_W: case LAvecun_VCLZ_D:
      case LAvecun_VPCNT_B: case LAvecun_VPCNT_H: case LAvecun_VPCNT_W: case LAvecun_VPCNT_D:
      case LAvecun_VNEG_B: case LAvecun_VNEG_H: case LAvecun_VNEG_W: case LAvecun_VNEG_D:
      case LAvecun_VMSKLTZ_B: case LAvecun_VMSKLTZ_H: case LAvecun_VMSKLTZ_W: case LAvecun_VMSKLTZ_D:
      case LAvecun_VMSKGEZ_B: case LAvecun_VMSKNZ_B:
      case LAvecun_VFLOGB_S: case LAvecun_VFLOGB_D:
      case LAvecun_VFCLASS_S: case LAvecun_VFCLASS_D:
      case LAvecun_VFSQRT_S: case LAvecun_VFSQRT_D:
      case LAvecun_VFRECIP_S: case LAvecun_VFRECIP_D:
      case LAvecun_VFRSQRT_S: case LAvecun_VFRSQRT_D:
      case LAvecun_VFRINT_S: case LAvecun_VFRINT_D:
      case LAvecun_VFRINTRM_S: case LAvecun_VFRINTRM_D:
      case LAvecun_VFRINTRP_S: case LAvecun_VFRINTRP_D:
      case LAvecun_VFRINTRZ_S: case LAvecun_VFRINTRZ_D:
      case LAvecun_VFRINTRNZ_S: case LAvecun_VFRINTRNZ_D:
      case LAvecun_VFCVTL_S_H: case LAvecun_VFCVTH_S_H:
      case LAvecun_VFCVTL_D_S: case LAvecun_VFCVTH_D_S:
      case LAvecun_VFFINT_S_W: case LAvecun_VFFINT_S_WU:
      case LAvecun_VFFINT_D_L: case LAvecun_VFFINT_D_LU:
      case LAvecun_VFFINTL_D_W: case LAvecun_VFFINTH_D_W:
      case LAvecun_VFTINT_W_S: case LAvecun_VFTINT_L_D:
      case LAvecun_VFTINTRM_W_S: case LAvecun_VFTINTRM_L_D:
      case LAvecun_VFTINTRP_W_S: case LAvecun_VFTINTRP_L_D:
      case LAvecun_VFTINTRZ_W_S: case LAvecun_VFTINTRZ_L_D:
      case LAvecun_VFTINTRNE_W_S: case LAvecun_VFTINTRNE_L_D:
      case LAvecun_VFTINT_WU_S: case LAvecun_VFTINT_LU_D:
      case LAvecun_VFTINTRZ_WU_S: case LAvecun_VFTINTRZ_LU_D:
      case LAvecun_VFTINTL_L_S: case LAvecun_VFTINTH_L_S:
      case LAvecun_VFTINTRML_L_S: case LAvecun_VFTINTRMH_L_S:
      case LAvecun_VFTINTRPL_L_S: case LAvecun_VFTINTRPH_L_S:
      case LAvecun_VFTINTRZH_L_S:
      case LAvecun_VFTINTRNEL_L_S: case LAvecun_VFTINTRNEH_L_S:
      case LAvecun_VEXTH_H_B: case LAvecun_VEXTH_W_H: case LAvecun_VEXTH_D_W: case LAvecun_VEXTH_Q_D:
      case LAvecun_VEXTH_HU_BU: case LAvecun_VEXTH_WU_HU: case LAvecun_VEXTH_DU_WU: case LAvecun_VEXTH_QU_DU:
      case LAvecun_VEXTL_Q_D: case LAvecun_VEXTL_QU_DU:
         *p++ = emit_unop_r(op, vregEnc(src), vregEnc(dst), 5);
         return p;
      case LAvecun_VREPLGR2VR_B: case LAvecun_VREPLGR2VR_H: case LAvecun_VREPLGR2VR_W: case LAvecun_VREPLGR2VR_D:
         *p++ = emit_unop_r(op, iregEnc(src), vregEnc(dst), 5);
         return p;
      case LAvecun_VSETEQZ_V:
      case LAvecun_VSETNEZ_V:
      case LAvecun_VSETANYEQZ_B: case LAvecun_VSETANYEQZ_H: case LAvecun_VSETANYEQZ_W: case LAvecun_VSETANYEQZ_D:
      case LAvecun_VSETALLNEZ_B: case LAvecun_VSETALLNEZ_H: case LAvecun_VSETALLNEZ_W: case LAvecun_VSETALLNEZ_D:
         *p++ = emit_unop_r(op, vregEnc(src), vregEnc(dst), 2);
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkVecBinary ( UInt* p, LOONGARCH64VecBinOp op,
                                  LOONGARCH64RI* src2, HReg src1, HReg dst )
{
   switch (op) {
      case LAvecbin_VSEQ_B: case LAvecbin_VSEQ_H: case LAvecbin_VSEQ_W: case LAvecbin_VSEQ_D:
      case LAvecbin_VSLE_B: case LAvecbin_VSLE_H: case LAvecbin_VSLE_W: case LAvecbin_VSLE_D:
      case LAvecbin_VSLE_BU: case LAvecbin_VSLE_HU: case LAvecbin_VSLE_WU: case LAvecbin_VSLE_DU:
      case LAvecbin_VSLT_B: case LAvecbin_VSLT_H: case LAvecbin_VSLT_W: case LAvecbin_VSLT_D:
      case LAvecbin_VSLT_BU: case LAvecbin_VSLT_HU: case LAvecbin_VSLT_WU: case LAvecbin_VSLT_DU:
      case LAvecbin_VADD_B: case LAvecbin_VADD_H: case LAvecbin_VADD_D: case LAvecbin_VADD_W:
      case LAvecbin_VSUB_B: case LAvecbin_VSUB_H: case LAvecbin_VSUB_W: case LAvecbin_VSUB_D:
      case LAvecbin_VADDWEV_H_B: case LAvecbin_VADDWEV_W_H: case LAvecbin_VADDWEV_D_W: case LAvecbin_VADDWEV_Q_D:
      case LAvecbin_VSUBWEV_H_B: case LAvecbin_VSUBWEV_W_H: case LAvecbin_VSUBWEV_D_W: case LAvecbin_VSUBWEV_Q_D:
      case LAvecbin_VADDWOD_H_B: case LAvecbin_VADDWOD_W_H: case LAvecbin_VADDWOD_D_W: case LAvecbin_VADDWOD_Q_D:
      case LAvecbin_VSUBWOD_H_B: case LAvecbin_VSUBWOD_W_H: case LAvecbin_VSUBWOD_D_W: case LAvecbin_VSUBWOD_Q_D:
      case LAvecbin_VADDWEV_H_BU: case LAvecbin_VADDWEV_W_HU: case LAvecbin_VADDWEV_D_WU: case LAvecbin_VADDWEV_Q_DU:
      case LAvecbin_VSUBWEV_H_BU: case LAvecbin_VSUBWEV_W_HU: case LAvecbin_VSUBWEV_D_WU: case LAvecbin_VSUBWEV_Q_DU:
      case LAvecbin_VADDWOD_H_BU: case LAvecbin_VADDWOD_W_HU: case LAvecbin_VADDWOD_D_WU: case LAvecbin_VADDWOD_Q_DU:
      case LAvecbin_VSUBWOD_H_BU: case LAvecbin_VSUBWOD_W_HU: case LAvecbin_VSUBWOD_D_WU: case LAvecbin_VSUBWOD_Q_DU:
      case LAvecbin_VADDWEV_H_BU_B: case LAvecbin_VADDWEV_W_HU_H: case LAvecbin_VADDWEV_D_WU_W: case LAvecbin_VADDWEV_Q_DU_D:
      case LAvecbin_VADDWOD_H_BU_B: case LAvecbin_VADDWOD_W_HU_H: case LAvecbin_VADDWOD_D_WU_W: case LAvecbin_VADDWOD_Q_DU_D:
      case LAvecbin_VSADD_B: case LAvecbin_VSADD_H: case LAvecbin_VSADD_W: case LAvecbin_VSADD_D:
      case LAvecbin_VSSUB_B: case LAvecbin_VSSUB_H: case LAvecbin_VSSUB_W: case LAvecbin_VSSUB_D:
      case LAvecbin_VSADD_BU: case LAvecbin_VSADD_HU: case LAvecbin_VSADD_WU: case LAvecbin_VSADD_DU:
      case LAvecbin_VSSUB_BU: case LAvecbin_VSSUB_HU: case LAvecbin_VSSUB_WU: case LAvecbin_VSSUB_DU:
      case LAvecbin_VHADDW_H_B: case LAvecbin_VHADDW_W_H: case LAvecbin_VHADDW_D_W: case LAvecbin_VHADDW_Q_D:
      case LAvecbin_VHSUBW_H_B: case LAvecbin_VHSUBW_W_H: case LAvecbin_VHSUBW_D_W: case LAvecbin_VHSUBW_Q_D:
      case LAvecbin_VHADDW_HU_BU: case LAvecbin_VHADDW_WU_HU: case LAvecbin_VHADDW_DU_WU: case LAvecbin_VHADDW_QU_DU:
      case LAvecbin_VHSUBW_HU_BU: case LAvecbin_VHSUBW_WU_HU: case LAvecbin_VHSUBW_DU_WU: case LAvecbin_VHSUBW_QU_DU:
      case LAvecbin_VADDA_B: case LAvecbin_VADDA_H: case LAvecbin_VADDA_W: case LAvecbin_VADDA_D:
      case LAvecbin_VABSD_B: case LAvecbin_VABSD_H: case LAvecbin_VABSD_W: case LAvecbin_VABSD_D:
      case LAvecbin_VABSD_BU: case LAvecbin_VABSD_HU: case LAvecbin_VABSD_WU: case LAvecbin_VABSD_DU:
      case LAvecbin_VAVG_B: case LAvecbin_VAVG_H: case LAvecbin_VAVG_W: case LAvecbin_VAVG_D:
      case LAvecbin_VAVG_BU: case LAvecbin_VAVG_HU: case LAvecbin_VAVG_WU: case LAvecbin_VAVG_DU:
      case LAvecbin_VAVGR_B: case LAvecbin_VAVGR_H: case LAvecbin_VAVGR_W: case LAvecbin_VAVGR_D:
      case LAvecbin_VAVGR_BU: case LAvecbin_VAVGR_HU: case LAvecbin_VAVGR_WU: case LAvecbin_VAVGR_DU:
      case LAvecbin_VMAX_B: case LAvecbin_VMAX_H: case LAvecbin_VMAX_W: case LAvecbin_VMAX_D:
      case LAvecbin_VMIN_B: case LAvecbin_VMIN_H: case LAvecbin_VMIN_W: case LAvecbin_VMIN_D:
      case LAvecbin_VMAX_BU: case LAvecbin_VMAX_HU: case LAvecbin_VMAX_WU: case LAvecbin_VMAX_DU:
      case LAvecbin_VMIN_BU: case LAvecbin_VMIN_HU: case LAvecbin_VMIN_WU: case LAvecbin_VMIN_DU:
      case LAvecbin_VMUL_B: case LAvecbin_VMUL_H: case LAvecbin_VMUL_W: case LAvecbin_VMUL_D:
      case LAvecbin_VMUH_B: case LAvecbin_VMUH_H: case LAvecbin_VMUH_W: case LAvecbin_VMUH_D:
      case LAvecbin_VMUH_BU: case LAvecbin_VMUH_HU: case LAvecbin_VMUH_WU: case LAvecbin_VMUH_DU:
      case LAvecbin_VMULWEV_H_B: case LAvecbin_VMULWEV_W_H: case LAvecbin_VMULWEV_D_W: case LAvecbin_VMULWEV_Q_D:
      case LAvecbin_VMULWOD_H_B: case LAvecbin_VMULWOD_W_H: case LAvecbin_VMULWOD_D_W: case LAvecbin_VMULWOD_Q_D:
      case LAvecbin_VMULWEV_H_BU: case LAvecbin_VMULWEV_W_HU: case LAvecbin_VMULWEV_D_WU: case LAvecbin_VMULWEV_Q_DU:
      case LAvecbin_VMULWOD_H_BU: case LAvecbin_VMULWOD_W_HU: case LAvecbin_VMULWOD_D_WU: case LAvecbin_VMULWOD_Q_DU:
      case LAvecbin_VMULWEV_H_BU_B: case LAvecbin_VMULWEV_W_HU_H: case LAvecbin_VMULWEV_D_WU_W: case LAvecbin_VMULWEV_Q_DU_D:
      case LAvecbin_VMULWOD_H_BU_B:case LAvecbin_VMULWOD_W_HU_H: case LAvecbin_VMULWOD_D_WU_W: case LAvecbin_VMULWOD_Q_DU_D:
      case LAvecbin_VMADD_B: case LAvecbin_VMADD_H: case LAvecbin_VMADD_W: case LAvecbin_VMADD_D:
      case LAvecbin_VMSUB_B: case LAvecbin_VMSUB_H: case LAvecbin_VMSUB_W: case LAvecbin_VMSUB_D:
      case LAvecbin_VMADDWEV_H_B: case LAvecbin_VMADDWEV_W_H: case LAvecbin_VMADDWEV_D_W: case LAvecbin_VMADDWEV_Q_D:
      case LAvecbin_VMADDWOD_H_B: case LAvecbin_VMADDWOD_W_H: case LAvecbin_VMADDWOD_D_W: case LAvecbin_VMADDWOD_Q_D:
      case LAvecbin_VMADDWEV_H_BU: case LAvecbin_VMADDWEV_W_HU: case LAvecbin_VMADDWEV_D_WU: case LAvecbin_VMADDWEV_Q_DU:
      case LAvecbin_VMADDWOD_H_BU: case LAvecbin_VMADDWOD_W_HU: case LAvecbin_VMADDWOD_D_WU: case LAvecbin_VMADDWOD_Q_DU:
      case LAvecbin_VMADDWEV_H_BU_B: case LAvecbin_VMADDWEV_W_HU_H: case LAvecbin_VMADDWEV_D_WU_W: case LAvecbin_VMADDWEV_Q_DU_D:
      case LAvecbin_VMADDWOD_H_BU_B: case LAvecbin_VMADDWOD_W_HU_H: case LAvecbin_VMADDWOD_D_WU_W: case LAvecbin_VMADDWOD_Q_DU_D:
      case LAvecbin_VDIV_B: case LAvecbin_VDIV_H: case LAvecbin_VDIV_W: case LAvecbin_VDIV_D:
      case LAvecbin_VMOD_B: case LAvecbin_VMOD_H: case LAvecbin_VMOD_W: case LAvecbin_VMOD_D:
      case LAvecbin_VDIV_BU: case LAvecbin_VDIV_HU: case LAvecbin_VDIV_WU: case LAvecbin_VDIV_DU:
      case LAvecbin_VMOD_BU: case LAvecbin_VMOD_HU: case LAvecbin_VMOD_WU: case LAvecbin_VMOD_DU:
      case LAvecbin_VSLL_B: case LAvecbin_VSLL_H: case LAvecbin_VSLL_W: case LAvecbin_VSLL_D:
      case LAvecbin_VSRL_B: case LAvecbin_VSRL_H: case LAvecbin_VSRL_W: case LAvecbin_VSRL_D:
      case LAvecbin_VSRA_B: case LAvecbin_VSRA_H: case LAvecbin_VSRA_W: case LAvecbin_VSRA_D:
      case LAvecbin_VROTR_B: case LAvecbin_VROTR_H: case LAvecbin_VROTR_W: case LAvecbin_VROTR_D:
      case LAvecbin_VSRLR_B: case LAvecbin_VSRLR_H: case LAvecbin_VSRLR_W: case LAvecbin_VSRLR_D:
      case LAvecbin_VSRAR_B: case LAvecbin_VSRAR_H: case LAvecbin_VSRAR_W: case LAvecbin_VSRAR_D:
      case LAvecbin_VSRLN_B_H: case LAvecbin_VSRLN_H_W: case LAvecbin_VSRLN_W_D:
      case LAvecbin_VSRAN_B_H: case LAvecbin_VSRAN_H_W: case LAvecbin_VSRAN_W_D:
      case LAvecbin_VSRLRN_B_H: case LAvecbin_VSRLRN_H_W: case LAvecbin_VSRLRN_W_D:
      case LAvecbin_VSRARN_B_H: case LAvecbin_VSRARN_H_W: case LAvecbin_VSRARN_W_D:
      case LAvecbin_VSSRLN_B_H: case LAvecbin_VSSRLN_H_W: case LAvecbin_VSSRLN_W_D:
      case LAvecbin_VSSRAN_B_H: case LAvecbin_VSSRAN_H_W: case LAvecbin_VSSRAN_W_D:
      case LAvecbin_VSSRLRN_B_H: case LAvecbin_VSSRLRN_H_W: case LAvecbin_VSSRLRN_W_D:
      case LAvecbin_VSSRARN_B_H: case LAvecbin_VSSRARN_H_W: case LAvecbin_VSSRARN_W_D:
      case LAvecbin_VSSRLN_BU_H: case LAvecbin_VSSRLN_HU_W: case LAvecbin_VSSRLN_WU_D:
      case LAvecbin_VSSRAN_BU_H: case LAvecbin_VSSRAN_HU_W: case LAvecbin_VSSRAN_WU_D:
      case LAvecbin_VSSRLRN_BU_H: case LAvecbin_VSSRLRN_HU_W: case LAvecbin_VSSRLRN_WU_D:
      case LAvecbin_VSSRARN_BU_H: case LAvecbin_VSSRARN_HU_W: case LAvecbin_VSSRARN_WU_D:
      case LAvecbin_VBITCLR_B: case LAvecbin_VBITCLR_H: case LAvecbin_VBITCLR_W: case LAvecbin_VBITCLR_D:
      case LAvecbin_VBITSET_B: case LAvecbin_VBITSET_H: case LAvecbin_VBITSET_W: case LAvecbin_VBITSET_D:
      case LAvecbin_VBITREV_B: case LAvecbin_VBITREV_H: case LAvecbin_VBITREV_W: case LAvecbin_VBITREV_D:
      case LAvecbin_VPACKEV_B: case LAvecbin_VPACKEV_H: case LAvecbin_VPACKEV_W: case LAvecbin_VPACKEV_D:
      case LAvecbin_VPACKOD_B: case LAvecbin_VPACKOD_H: case LAvecbin_VPACKOD_W: case LAvecbin_VPACKOD_D:
      case LAvecbin_VILVL_B: case LAvecbin_VILVL_H: case LAvecbin_VILVL_W: case LAvecbin_VILVL_D:
      case LAvecbin_VILVH_B: case LAvecbin_VILVH_H: case LAvecbin_VILVH_W: case LAvecbin_VILVH_D:
      case LAvecbin_VPICKEV_B: case LAvecbin_VPICKEV_H: case LAvecbin_VPICKEV_W: case LAvecbin_VPICKEV_D:
      case LAvecbin_VPICKOD_B: case LAvecbin_VPICKOD_H: case LAvecbin_VPICKOD_W: case LAvecbin_VPICKOD_D:
      case LAvecbin_VAND_V:
      case LAvecbin_VOR_V:
      case LAvecbin_VXOR_V:
      case LAvecbin_VNOR_V:
      case LAvecbin_VANDN_V:
      case LAvecbin_VORN_V:
      case LAvecbin_VFRSTP_B: case LAvecbin_VFRSTP_H:
      case LAvecbin_VADD_Q:
      case LAvecbin_VSUB_Q:
      case LAvecbin_VSIGNCOV_B: case LAvecbin_VSIGNCOV_H: case LAvecbin_VSIGNCOV_W: case LAvecbin_VSIGNCOV_D:
      case LAvecbin_VFADD_S: case LAvecbin_VFADD_D:
      case LAvecbin_VFSUB_S: case LAvecbin_VFSUB_D:
      case LAvecbin_VFMUL_S: case LAvecbin_VFMUL_D:
      case LAvecbin_VFDIV_S: case LAvecbin_VFDIV_D:
      case LAvecbin_VFMAX_S: case LAvecbin_VFMAX_D:
      case LAvecbin_VFMIN_S: case LAvecbin_VFMIN_D:
      case LAvecbin_VFMAXA_S: case LAvecbin_VFMAXA_D:
      case LAvecbin_VFMINA_S: case LAvecbin_VFMINA_D:
      case LAvecbin_VFCVT_H_S: case LAvecbin_VFCVT_S_D:
         vassert(src2->tag == LAri_Reg);
         *p++ = emit_binop_rr(op, vregEnc(src2->LAri.R.reg), vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VREPLVE_B: case LAvecbin_VREPLVE_H: case LAvecbin_VREPLVE_W: case LAvecbin_VREPLVE_D:
         vassert(src2->tag == LAri_Reg);
         *p++ = emit_binop_rr(op, iregEnc(src2->LAri.R.reg), vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VREPLVEI_D:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 1, vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VINSGR2VR_D:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 1, iregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VPICKVE2GR_D:
      case LAvecbin_VPICKVE2GR_DU:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 1, vregEnc(src1), iregEnc(dst));
         return p;
      case LAvecbin_VREPLVEI_W:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 2, vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VINSGR2VR_W:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 2, iregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VPICKVE2GR_W:
      case LAvecbin_VPICKVE2GR_WU:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 2, vregEnc(src1), iregEnc(dst));
         return p;
      case LAvecbin_VROTRI_B:
      case LAvecbin_VSRLRI_B:
      case LAvecbin_VSRARI_B:
      case LAvecbin_VREPLVEI_H:
      case LAvecbin_VSLLWIL_H_B:
      case LAvecbin_VSLLWIL_HU_BU:
      case LAvecbin_VBITCLRI_B:
      case LAvecbin_VBITSETI_B:
      case LAvecbin_VBITREVI_B:
      case LAvecbin_VSAT_B:
      case LAvecbin_VSAT_BU:
      case LAvecbin_VSLLI_B:
      case LAvecbin_VSRLI_B:
      case LAvecbin_VSRAI_B:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 3, vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VINSGR2VR_H:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 3, iregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VPICKVE2GR_H:
      case LAvecbin_VPICKVE2GR_HU:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 3, vregEnc(src1), iregEnc(dst));
         return p;
      case LAvecbin_VROTRI_H:
      case LAvecbin_VSRLRI_H:
      case LAvecbin_VSRARI_H:
      case LAvecbin_VREPLVEI_B:
      case LAvecbin_VSLLWIL_W_H:
      case LAvecbin_VSLLWIL_WU_HU:
      case LAvecbin_VBITCLRI_H:
      case LAvecbin_VBITSETI_H:
      case LAvecbin_VBITREVI_H:
      case LAvecbin_VSAT_H:
      case LAvecbin_VSAT_HU:
      case LAvecbin_VSLLI_H:
      case LAvecbin_VSRLI_H:
      case LAvecbin_VSRAI_H:
      case LAvecbin_VSRLNI_B_H:
      case LAvecbin_VSRLRNI_B_H:
      case LAvecbin_VSSRLNI_B_H:
      case LAvecbin_VSSRLNI_BU_H:
      case LAvecbin_VSSRLRNI_B_H:
      case LAvecbin_VSSRLRNI_BU_H:
      case LAvecbin_VSRANI_B_H:
      case LAvecbin_VSRARNI_B_H:
      case LAvecbin_VSSRANI_B_H:
      case LAvecbin_VSSRANI_BU_H:
      case LAvecbin_VSSRARNI_B_H:
      case LAvecbin_VSSRARNI_BU_H:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 4, vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VINSGR2VR_B:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 4, iregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VPICKVE2GR_B:
      case LAvecbin_VPICKVE2GR_BU:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 4, vregEnc(src1), iregEnc(dst));
         return p;
      case LAvecbin_VSEQI_B: case LAvecbin_VSEQI_H: case LAvecbin_VSEQI_W: case LAvecbin_VSEQI_D:
      case LAvecbin_VSLEI_B: case LAvecbin_VSLEI_H: case LAvecbin_VSLEI_W: case LAvecbin_VSLEI_D:
      case LAvecbin_VSLEI_BU: case LAvecbin_VSLEI_HU: case LAvecbin_VSLEI_WU: case LAvecbin_VSLEI_DU:
      case LAvecbin_VSLTI_B: case LAvecbin_VSLTI_H: case LAvecbin_VSLTI_W: case LAvecbin_VSLTI_D:
      case LAvecbin_VSLTI_BU: case LAvecbin_VSLTI_HU: case LAvecbin_VSLTI_WU: case LAvecbin_VSLTI_DU:
      case LAvecbin_VADDI_BU: case LAvecbin_VADDI_HU: case LAvecbin_VADDI_WU: case LAvecbin_VADDI_DU:
      case LAvecbin_VSUBI_BU: case LAvecbin_VSUBI_HU: case LAvecbin_VSUBI_WU: case LAvecbin_VSUBI_DU:
      case LAvecbin_VBSLL_V:
      case LAvecbin_VBSRL_V:
      case LAvecbin_VMAXI_B: case LAvecbin_VMAXI_H: case LAvecbin_VMAXI_W: case LAvecbin_VMAXI_D:
      case LAvecbin_VMINI_B: case LAvecbin_VMINI_H: case LAvecbin_VMINI_W: case LAvecbin_VMINI_D:
      case LAvecbin_VMAXI_BU: case LAvecbin_VMAXI_HU: case LAvecbin_VMAXI_WU: case LAvecbin_VMAXI_DU:
      case LAvecbin_VMINI_BU: case LAvecbin_VMINI_HU: case LAvecbin_VMINI_WU: case LAvecbin_VMINI_DU:
      case LAvecbin_VFRSTPI_B: case LAvecbin_VFRSTPI_H:
      case LAvecbin_VROTRI_W:
      case LAvecbin_VSRLRI_W:
      case LAvecbin_VSRARI_W:
      case LAvecbin_VSLLWIL_D_W:  case LAvecbin_VSLLWIL_DU_WU:
      case LAvecbin_VBITCLRI_W:
      case LAvecbin_VBITSETI_W:
      case LAvecbin_VBITREVI_W:
      case LAvecbin_VSAT_W: case LAvecbin_VSAT_WU:
      case LAvecbin_VSLLI_W:
      case LAvecbin_VSRLI_W:
      case LAvecbin_VSRAI_W:
      case LAvecbin_VSRLNI_H_W:
      case LAvecbin_VSRLRNI_H_W:
      case LAvecbin_VSSRLNI_H_W: case LAvecbin_VSSRLNI_HU_W:
      case LAvecbin_VSSRLRNI_H_W: case LAvecbin_VSSRLRNI_HU_W:
      case LAvecbin_VSRANI_H_W:
      case LAvecbin_VSRARNI_H_W:
      case LAvecbin_VSSRANI_H_W: case LAvecbin_VSSRANI_HU_W:
      case LAvecbin_VSSRARNI_H_W: case LAvecbin_VSSRARNI_HU_W:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 5, vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VROTRI_D:
      case LAvecbin_VSRLRI_D:
      case LAvecbin_VSRARI_D:
      case LAvecbin_VBITCLRI_D:
      case LAvecbin_VBITSETI_D:
      case LAvecbin_VBITREVI_D:
      case LAvecbin_VSAT_D: case LAvecbin_VSAT_DU:
      case LAvecbin_VSLLI_D:
      case LAvecbin_VSRLI_D:
      case LAvecbin_VSRAI_D:
      case LAvecbin_VSRLNI_W_D:
      case LAvecbin_VSRLRNI_W_D:
      case LAvecbin_VSSRLNI_W_D: case LAvecbin_VSSRLNI_WU_D:
      case LAvecbin_VSSRLRNI_W_D: case LAvecbin_VSSRLRNI_WU_D:
      case LAvecbin_VSRANI_W_D:
      case LAvecbin_VSRARNI_W_D:
      case LAvecbin_VSSRANI_W_D: case LAvecbin_VSSRANI_WU_D:
      case LAvecbin_VSSRARNI_W_D: case LAvecbin_VSSRARNI_WU_D:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 6, vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VSRLNI_D_Q:
      case LAvecbin_VSRLRNI_D_Q:
      case LAvecbin_VSSRLNI_D_Q: case LAvecbin_VSSRLNI_DU_Q:
      case LAvecbin_VSSRLRNI_D_Q: case LAvecbin_VSSRLRNI_DU_Q:
      case LAvecbin_VSRANI_D_Q:
      case LAvecbin_VSRARNI_D_Q:
      case LAvecbin_VSSRANI_D_Q: case LAvecbin_VSSRANI_DU_Q:
      case LAvecbin_VSSRARNI_D_Q: case LAvecbin_VSSRARNI_DU_Q:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 7, vregEnc(src1), vregEnc(dst));
         return p;
      case LAvecbin_VEXTRINS_D: case LAvecbin_VEXTRINS_W: case LAvecbin_VEXTRINS_H: case LAvecbin_VEXTRINS_B:
      case LAvecbin_VSHUF4I_B: case LAvecbin_VSHUF4I_H: case LAvecbin_VSHUF4I_W: case LAvecbin_VSHUF4I_D:
      case LAvecbin_VBITSELI_B:
      case LAvecbin_VADDI_B:
      case LAvecbin_VORI_B:
      case LAvecbin_VXORI_B:
      case LAvecbin_VNORI_B:
      case LAvecbin_VPERMI_W:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_binop_ri(op, src2->LAri.I.imm, 8, vregEnc(src1), vregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkVecTrinary ( UInt* p, LOONGARCH64VecTriOp op, HReg src3,
                                   HReg src2, HReg src1, HReg dst )
{
   switch (op) {
      case LAvectri_VFMADD_S: case LAvectri_VFMADD_D:
      case LAvectri_VFMSUB_S: case LAvectri_VFMSUB_D:
      case LAvectri_VFNMADD_S: case LAvectri_VFNMADD_D:
      case LAvectri_VFNMSUB_S: case LAvectri_VFNMSUB_D:
      case LAvectri_VBITSEL_V:
      case LAvectri_VSHUF_B:
         *p++ = emit_triop_rrr(op, vregEnc(src3), vregEnc(src2),
                               vregEnc(src1), vregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkVecLoad ( UInt* p, LOONGARCH64VecLoadOp op,
                                LOONGARCH64AMode* src, HReg dst )
{
   switch (op) {
      case LAvecload_VLD:
      case LAvecload_VLDREPL_D:
         vassert(src->tag == LAam_RI);
         *p++ = emit_binop_ri(op, src->LAam.RI.index, 12,
                              iregEnc(src->LAam.RI.base), vregEnc(dst));
         return p;
      case LAvecload_VLDREPL_W:
         vassert(src->tag == LAam_RI);
         *p++ = emit_binop_ri(op, src->LAam.RI.index, 11,
                              iregEnc(src->LAam.RI.base), vregEnc(dst));
         return p;
      case LAvecload_VLDREPL_H:
         vassert(src->tag == LAam_RI);
         *p++ = emit_binop_ri(op, src->LAam.RI.index, 10,
                              iregEnc(src->LAam.RI.base), vregEnc(dst));
         return p;
      case LAvecload_VLDREPL_B:
         vassert(src->tag == LAam_RI);
         *p++ = emit_binop_ri(op, src->LAam.RI.index, 9,
                              iregEnc(src->LAam.RI.base), vregEnc(dst));
         return p;
      case LAvecload_VLDX:
         vassert(src->tag == LAam_RR);
         *p++ = emit_binop_rr(op, iregEnc(src->LAam.RR.index),
                              iregEnc(src->LAam.RR.base), vregEnc(dst));
         return p;
      case LAvecload_VLDI:
         vassert(src->tag == LAam_RI);
         *p++ = emit_op_si13_vd(op, src->LAam.RI.index, vregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkVecStore ( UInt* p, LOONGARCH64VecStoreOp op,
                                 LOONGARCH64AMode* dst, HReg src )
{
   switch (op) {
      case LAvecstore_VST:
         vassert(dst->tag == LAam_RI);
         *p++ = emit_binop_ri(op, dst->LAam.RI.index, 12,
                              iregEnc(dst->LAam.RI.base), vregEnc(src));
         return p;
      case LAvecstore_VSTX:
         vassert(dst->tag == LAam_RR);
         *p++ = emit_binop_rr(op, iregEnc(dst->LAam.RR.index),
                              iregEnc(dst->LAam.RR.base), vregEnc(src));
         return p;
      default: return NULL;
   }
}

static inline UInt* mkVecFpCmp ( UInt* p, LOONGARCH64VecFpCmpOp op,
                                 HReg src2, HReg src1, HReg dst )
{
   /*
      fcmp.cond.[sd] $fcc0, src1, src2
      movcf2gr       dst, $fcc0
    */
   switch (op) {
      case LAveccmp_VFCMP_CLT_S: case LAveccmp_VFCMP_CLT_D:
      case LAveccmp_VFCMP_CEQ_S: case LAveccmp_VFCMP_CEQ_D:
      case LAveccmp_VFCMP_CUN_S: case LAveccmp_VFCMP_CUN_D:
         *p++ = emit_binop_rr(op, vregEnc(src2), vregEnc(src1), vregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkCas ( UInt* p, HReg old, HReg addr, HReg expd,
                            HReg data, Bool size64 )
{
   /*
         ll.[wd] old, addr, 0
         bne     old, expd, barrier
         or      $t0, data, $zero
         sc.[wd] $t0, addr, 0
         beq     $t0, zero, fail
         or      old, expd, $zero
         b       end
      barrier:
         dbar    0
      fail:
         or      old, data, $zero
      end:
    */
   UInt o = iregEnc(old);
   UInt a = iregEnc(addr);
   UInt e = iregEnc(expd);
   UInt d = iregEnc(data);
   UInt t = 12;
   UInt z = 0;

   if (size64) {
      *p++ = emit_op_si14_rj_rd(LAllsc_LL_D, 0, a, o);
   } else {
      *p++ = emit_op_ui6_rj_rd(LAbin_SLLI_W, 0, e, e); // Sign-extend expd
      *p++ = emit_op_si14_rj_rd(LAllsc_LL_W, 0, a, o);
   }
   *p++ = emit_op_offs16_rj_rd(LAextra_BNE, 6, o, e);
   *p++ = emit_op_rk_rj_rd(LAbin_OR, z, d, t);
   if (size64) {
      *p++ = emit_op_si14_rj_rd(LAllsc_SC_D, 0, a, t);
   } else {
      *p++ = emit_op_si14_rj_rd(LAllsc_SC_W, 0, a, t);
   }
   *p++ = emit_op_offs16_rj_rd(LAextra_BEQ, 4, t, z);
   *p++ = emit_op_rk_rj_rd(LAbin_OR, z, e, o);
   *p++ = emit_op_offs26(LAextra_B, 3);
   *p++ = emit_op_hint15(LAbar_DBAR, 0);
   *p++ = emit_op_rk_rj_rd(LAbin_OR, z, d, o);
   return p;
}

static inline UInt* mkCmp ( UInt* p, LOONGARCH64CondCode cond,
                            HReg src2, HReg src1, HReg dst )
{
   UInt d  = iregEnc(dst);
   UInt s1 = iregEnc(src1);
   UInt s2 = iregEnc(src2);

   switch (cond) {
      case LAcc_EQ:
         /*
            xor   dst, src1, src2
            sltui dst, dst, 1
          */
         *p++ = emit_op_rk_rj_rd(LAbin_XOR, s2, s1, d);
         *p++ = emit_op_si12_rj_rd(LAextra_SLTUI, 1, d, d);
         return p;
      case LAcc_NE:
         /*
            xor   dst, src1, src2
            sltu  dst, $zero, dst
          */
         *p++ = emit_op_rk_rj_rd(LAbin_XOR, s2, s1, d);
         *p++ = emit_op_rk_rj_rd(LAextra_SLTU, d, 0, d);
         return p;
      case LAcc_LT:
         /* slt   dst, src1, src2 */
         *p++ = emit_op_rk_rj_rd(LAextra_SLT, s2, s1, d);
         return p;
      case LAcc_GE:
         /*
            slt   dst, src1, src2
            sltui dst, dst, 1
          */
         *p++ = emit_op_rk_rj_rd(LAextra_SLT, s2, s1, d);
         *p++ = emit_op_si12_rj_rd(LAextra_SLTUI, 1, d, d);
         return p;
      case LAcc_LTU:
         /* sltu  dst, src1, src2 */
         *p++ = emit_op_rk_rj_rd(LAextra_SLTU, s2, s1, d);
         return p;
      case LAcc_GEU:
         /*
            sltu  dst, src1, src2
            sltui dst, dst, 1
          */
         *p++ = emit_op_rk_rj_rd(LAextra_SLTU, s2, s1, d);
         *p++ = emit_op_si12_rj_rd(LAextra_SLTUI, 1, d, d);
         return p;
      /* No LAcc_AL here.
         case LAcc_AL:
            break;
       */
      default:
         return NULL;
   }
}

static inline UInt* mkCMove ( UInt* p, HReg cond, HReg r0,
                              HReg r1, HReg dst, Bool isInt )
{
   if (isInt) {
      /*
         masknez $t0, r0, cond
         maskeqz dst, r1, cond
         or      dst, $t0, dst
       */
      UInt c = iregEnc(cond);
      UInt d = iregEnc(dst);
      *p++ = emit_op_rk_rj_rd(LAextra_MASKNEZ, c, iregEnc(r0), 12);
      *p++ = emit_op_rk_rj_rd(LAextra_MASKEQZ, c, iregEnc(r1), d);
      *p++ = emit_op_rk_rj_rd(LAbin_OR, d, 12, d);
   } else {
      /*
         movgr2cf $fcc0, cond
         fsel     dst, r0, r1, $fcc0
       */
      *p++ = emit_op_rj_cd(LAextra_MOVGR2CF, iregEnc(cond), 0);
      *p++ = emit_op_ca_fk_fj_fd(LAextra_FSEL, 0, fregEnc(r1),
                                 fregEnc(r0), fregEnc(dst));
   }
   return p;
}

static inline UInt* mkCall ( UInt* p, HReg cond, Addr64 target, RetLoc rloc )
{
   if (!hregIsInvalid(cond) && rloc.pri != RLPri_None) {
      /* The call might not happen (it isn't unconditional) and
         it returns a result.  In this case we will need to
         generate a control flow diamond to put 0x555..555 in
         the return register(s) in the case where the call
         doesn't happen.  If this ever becomes necessary, maybe
         copy code from the 32-bit ARM equivalent.  Until that
         day, just give up. */
      return NULL;
   }

   UInt* ptmp = NULL;
   if (!hregIsInvalid(cond)) {
      /* Create a hole to put a conditional branch in.  We'll
         patch it once we know the branch length. */
      ptmp = p;
      p++;
   }

   /*
      $t0 = target
      jirl $ra, $t0, 0
    */
   p = mkLoadImm(p, hregT0(), target);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Patch the hole if necessary */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 3 && offs <= 6);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkXDirect ( UInt* p, Addr64 dstGA,
                                LOONGARCH64AMode* amPC,
                                HReg cond, Bool toFastEP,
                                const void* disp_cp_chain_me_to_slowEP,
                                const void* disp_cp_chain_me_to_fastEP )
{
   /* NB: what goes on here has to be very closely coordinated
      with chainXDirect_LOONGARCH64 and unchainXDirect_LOONGARCH64 below. */
   /* We're generating chain-me requests here, so we need to be
      sure this is actually allowed -- no-redir translations
      can't use chain-me's.  Hence: */
   vassert(disp_cp_chain_me_to_slowEP != NULL);
   vassert(disp_cp_chain_me_to_fastEP != NULL);

   /* Use ptmp for backpatching conditional jumps. */
   UInt* ptmp = NULL;

   /* First off, if this is conditional, create a conditional
      jump over the rest of it.  Or at least, leave a space for
      it that we will shortly fill in. */
   if (!hregIsInvalid(cond)) {
      ptmp = p;
      p++;
   }

   /* Update the guest PC.
      $t0 = dstGA
      st.d $t0, amPC
    */
   p = mkLoadImm(p, hregT0(), (ULong)dstGA);
   p = mkStore(p, LAstore_ST_D, amPC, hregT0());

   /* --- FIRST PATCHABLE BYTE follows --- */
   /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're
      calling to) backs up the return address, so as to find the
      address of the first patchable byte.  So: don't change the
      number of instructions (5) below. */
   /*
      la   $t0, VG_(disp_cp_chain_me_to_{slowEP,fastEP})
      jirl $ra, $t0, 0
    */
   const void* disp_cp_chain_me = toFastEP ? disp_cp_chain_me_to_fastEP
                                           : disp_cp_chain_me_to_slowEP;
   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)disp_cp_chain_me);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);
   /* --- END of PATCHABLE BYTES --- */

   /* Fix up the conditional jump, if there was one. */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 8 && offs <= 11);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkXIndir ( UInt* p, HReg dstGA, LOONGARCH64AMode* amPC,
                               HReg cond, const void* disp_cp_xindir )
{
   /* We're generating transfers that could lead indirectly to a
      chain-me, so we need to be sure this is actually allowed --
      no-redir translations are not allowed to reach normal
      translations without going through the scheduler.  That means
      no XDirects or XIndirs out from no-redir translations.
      Hence: */
   vassert(disp_cp_xindir != NULL);

   /* Use ptmp for backpatching conditional jumps. */
   UInt* ptmp = NULL;

   /* First off, if this is conditional, create a conditional
      jump over the rest of it. */
   if (!hregIsInvalid(cond)) {
      ptmp = p;
      p++;
   }

   /* Update the guest PC.
      or   $t0, dstGA, $zero
      st.d $t0, amPC
    */
   *p++ = emit_op_rk_rj_rd(LAbin_OR, 0, iregEnc(dstGA), 12);
   p = mkStore(p, LAstore_ST_D, amPC, hregT0());

   /*
      la   $t0, VG_(disp_cp_xindir)
      jirl $ra, $t0, 0
    */
   p = mkLoadImm(p, hregT0(), (ULong)(Addr)disp_cp_xindir);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Fix up the conditional jump, if there was one. */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 5 && offs <= 8);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkXAssisted ( UInt* p, HReg dstGA, LOONGARCH64AMode* amPC,
                                  HReg cond, IRJumpKind jk,
                                  const void* disp_cp_xassisted )
{
   /* First off, if this is conditional, create a conditional jump
      over the rest of it.  Or at least, leave a space for it that
      we will shortly fill in. */
   UInt* ptmp = NULL;
   if (!hregIsInvalid(cond)) {
      ptmp = p;
      p++;
   }

   /* Update the guest PC.
      or   $t0, dstGA, $zero
      st.d $t0, amPC
    */
   *p++ = emit_op_rk_rj_rd(LAbin_OR, 0, iregEnc(dstGA), 12);
   p = mkStore(p, LAstore_ST_D, amPC, hregT0());

   /* li.w $s8, magic_number */
   UInt trcval = 0;
   switch (jk) {
      case Ijk_Boring:
         trcval = VEX_TRC_JMP_BORING;
         break;
      case Ijk_ClientReq:
         trcval = VEX_TRC_JMP_CLIENTREQ;
         break;
      case Ijk_NoDecode:
         trcval = VEX_TRC_JMP_NODECODE;
         break;
      case Ijk_InvalICache:
         trcval = VEX_TRC_JMP_INVALICACHE;
         break;
      case Ijk_NoRedir:
         trcval = VEX_TRC_JMP_NOREDIR;
         break;
      case Ijk_SigTRAP:
         trcval = VEX_TRC_JMP_SIGTRAP;
         break;
      case Ijk_SigSEGV:
         trcval = VEX_TRC_JMP_SIGSEGV;
         break;
      case Ijk_SigBUS:
         trcval = VEX_TRC_JMP_SIGBUS;
         break;
      case Ijk_SigFPE_IntDiv:
         trcval = VEX_TRC_JMP_SIGFPE_INTDIV;
         break;
      case Ijk_SigFPE_IntOvf:
         trcval = VEX_TRC_JMP_SIGFPE_INTOVF;
         break;
      case Ijk_SigSYS:
         trcval = VEX_TRC_JMP_SIGSYS;
         break;
      case Ijk_Sys_syscall:
         trcval = VEX_TRC_JMP_SYS_SYSCALL;
         break;
      /* We don't expect to see the following being assisted.
         case Ijk_Call:
         case Ijk_Ret:
         case Ijk_Yield:
         case Ijk_EmWarn:
         case Ijk_EmFail:
         case Ijk_MapFail:
         case Ijk_FlushDCache:
         case Ijk_SigILL:
         case Ijk_SigFPE:
         case Ijk_Sys_int32:
         case Ijk_Sys_int128:
         case Ijk_Sys_int129:
         case Ijk_Sys_int130:
         case Ijk_Sys_int145:
         case Ijk_Sys_int210:
         case Ijk_Sys_sysenter:
       */
      default:
         ppIRJumpKind(jk);
         vpanic("emit_LOONGARCH64Instr.LAin_XAssisted: unexpected jump kind");
   }
   vassert(trcval != 0);
   p = mkLoadImm(p, hregGSP(), trcval);

   /*
      la   $t0, VG_(disp_cp_xassisted)
      jirl $ra, $t0, 0
    */
   p = mkLoadImm(p, hregT0(), (ULong)(Addr)disp_cp_xassisted);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Fix up the conditional jump, if there was one. */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 6 && offs <= 12);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkEvCheck ( UInt* p, LOONGARCH64AMode* amCounter,
                                LOONGARCH64AMode* amFailAddr )
{
   UInt* p0 = p;

   /*
         ld.w   $t0, amCounter
         addi.d $t0, $t0, -1
         st.w   $t0, amCounter
         bge    $t0, $zero, nofail
         ld.d   $t0, amFailAddr
         jirl   $ra, $t0, 0
      nofail:
   */
   p = mkLoad(p, LAload_LD_W, amCounter, hregT0());
   *p++ = emit_op_si12_rj_rd(LAbin_ADDI_D, -1 & 0xfff, 12, 12);
   p = mkStore(p, LAstore_ST_W, amCounter, hregT0());
   *p++ = emit_op_offs16_rj_rd(LAextra_BGE, 3, 12, 0);
   p = mkLoad(p, LAload_LD_W, amFailAddr, hregT0());
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Crosscheck */
   vassert(evCheckSzB_LOONGARCH64() == (UChar*)p - (UChar*)p0);
   return p;
}

static inline UInt* mkProfInc ( UInt* p )
{
   /*
      li     $t0, 0x6555755585559555UL
      ld.d   $t1, $t0, 0
      addi.d $t1, $t1, 1
      st.d   $t1, $t0, 0
    */
   p = mkLoadImm_EXACTLY4(p, hregT0(), 0x6555755585559555UL);
   *p++ = emit_op_si12_rj_rd(LAload_LD_D, 0, 12, 13);
   *p++ = emit_op_si12_rj_rd(LAbin_ADDI_D, 1, 13, 13);
   *p++ = emit_op_si12_rj_rd(LAstore_ST_D, 0, 12, 13);
   return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */
Int emit_LOONGARCH64Instr ( /*MB_MOD*/Bool* is_profInc,
                            UChar* buf,
                            Int nbuf,
                            const LOONGARCH64Instr* i,
                            Bool mode64,
                            VexEndness endness_host,
                            const void* disp_cp_chain_me_to_slowEP,
                            const void* disp_cp_chain_me_to_fastEP,
                            const void* disp_cp_xindir,
                            const void* disp_cp_xassisted )
{
   vassert(mode64 == True);

   UInt* p = (UInt*)buf;
   vassert(nbuf >= 32);
   vassert((((HWord)buf) & 3) == 0);

   switch (i->tag) {
      case LAin_LI:
         p = mkLoadImm(p, i->LAin.LI.dst, i->LAin.LI.imm);
         break;
      case LAin_Un:
         p = mkUnary(p, i->LAin.Unary.op, i->LAin.Unary.src,
                     i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         p = mkBinary(p, i->LAin.Binary.op, i->LAin.Binary.src2,
                      i->LAin.Binary.src1, i->LAin.Binary.dst);
         break;
      case LAin_Load:
         p = mkLoad(p, i->LAin.Load.op, i->LAin.Load.src,
                    i->LAin.Load.dst);
         break;
      case LAin_Store:
         p = mkStore(p, i->LAin.Store.op, i->LAin.Store.dst,
                     i->LAin.Store.src);
         break;
      case LAin_LLSC:
         p = mkLLSC(p, i->LAin.LLSC.op, i->LAin.LLSC.addr, i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         p = mkBar(p, i->LAin.Bar.op, i->LAin.Bar.hint);
         break;
      case LAin_FpUn:
         p = mkFpUnary(p, i->LAin.FpUnary.op, i->LAin.FpUnary.src,
                       i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         p = mkFpBinary(p, i->LAin.FpBinary.op, i->LAin.FpBinary.src2,
                        i->LAin.FpBinary.src1, i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         p = mkFpTrinary(p, i->LAin.FpTrinary.op, i->LAin.FpTrinary.src3,
                         i->LAin.FpTrinary.src2, i->LAin.FpTrinary.src1,
                         i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         p = mkFpLoad(p, i->LAin.FpLoad.op, i->LAin.FpLoad.src,
                      i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         p = mkFpStore(p, i->LAin.FpStore.op, i->LAin.FpStore.dst,
                       i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         p = mkFpMove(p, i->LAin.FpMove.op, i->LAin.FpMove.src,
                      i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         p = mkFpCmp(p, i->LAin.FpCmp.op, i->LAin.FpCmp.src2,
                     i->LAin.FpCmp.src1, i->LAin.FpCmp.dst);
         break;
      case LAin_VecUn:
         p = mkVecUnary(p, i->LAin.VecUnary.op, i->LAin.VecUnary.src,
                        i->LAin.VecUnary.dst);
         break;
      case LAin_VecBin:
         p = mkVecBinary(p, i->LAin.VecBinary.op, i->LAin.VecBinary.src2,
                         i->LAin.VecBinary.src1, i->LAin.VecBinary.dst);
         break;
      case LAin_VecTri:
         p = mkVecTrinary(p, i->LAin.VecTrinary.op, i->LAin.VecTrinary.src3,
                          i->LAin.VecTrinary.src2, i->LAin.VecTrinary.src1,
                          i->LAin.VecTrinary.dst);
         break;
      case LAin_VecLoad:
         p = mkVecLoad(p, i->LAin.VecLoad.op, i->LAin.VecLoad.src,
                       i->LAin.VecLoad.dst);
         break;
      case LAin_VecStore:
         p = mkVecStore(p, i->LAin.VecStore.op, i->LAin.VecStore.dst,
                        i->LAin.VecStore.src);
         break;
      case LAin_VecFpCmp:
         p = mkVecFpCmp(p, i->LAin.VecFpCmp.op, i->LAin.VecFpCmp.src2,
                        i->LAin.VecFpCmp.src1, i->LAin.VecFpCmp.dst);
         break;
      case LAin_Cas:
         p = mkCas(p, i->LAin.Cas.old, i->LAin.Cas.addr, i->LAin.Cas.expd,
                   i->LAin.Cas.data, i->LAin.Cas.size64);
         break;
      case LAin_Cmp:
         p = mkCmp(p, i->LAin.Cmp.cond, i->LAin.Cmp.src2,
                   i->LAin.Cmp.src1, i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         p = mkCMove(p, i->LAin.CMove.cond, i->LAin.CMove.r0,
                     i->LAin.CMove.r1, i->LAin.CMove.dst,
                     i->LAin.CMove.isInt);
         break;
      case LAin_Call:
         p = mkCall(p, i->LAin.Call.cond, i->LAin.Call.target,
                    i->LAin.Call.rloc);
         break;
      case LAin_XDirect:
         p = mkXDirect(p, i->LAin.XDirect.dstGA, i->LAin.XDirect.amPC,
                       i->LAin.XDirect.cond, i->LAin.XDirect.toFastEP,
                       disp_cp_chain_me_to_slowEP,
                       disp_cp_chain_me_to_fastEP);
         break;
      case LAin_XIndir:
         p = mkXIndir(p, i->LAin.XIndir.dstGA, i->LAin.XIndir.amPC,
                      i->LAin.XIndir.cond, disp_cp_xindir);
         break;
      case LAin_XAssisted:
         p = mkXAssisted(p, i->LAin.XAssisted.dstGA, i->LAin.XAssisted.amPC,
                         i->LAin.XAssisted.cond, i->LAin.XAssisted.jk,
                         disp_cp_xassisted);
         break;
      case LAin_EvCheck:
         p = mkEvCheck(p, i->LAin.EvCheck.amCounter,
                       i->LAin.EvCheck.amFailAddr);
         break;
      case LAin_ProfInc:
         p = mkProfInc(p);
         break;
      default:
         p = NULL;
         break;
   }

   if (p == NULL) {
      ppLOONGARCH64Instr(i, True);
      vpanic("emit_LOONGARCH64Instr");
      /*NOTREACHED*/
   }

   vassert(((UChar*)p) - &buf[0] <= 48);
   return ((UChar*)p) - &buf[0];
}

/* How big is an event check?  See case for mkEvCheck just above.  That
   crosschecks what this returns, so we can tell if we're inconsistent. */
Int evCheckSzB_LOONGARCH64 ( void )
{
   return 6 * 4; // 6 insns
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_LOONGARCH64 ( VexEndness endness_host,
                                         void* place_to_chain,
                                         const void* disp_cp_chain_me_EXPECTED,
                                         const void* place_to_jump_to )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
    *  la $t0, disp_cp_chain_me_to_EXPECTED
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    */
   UInt* p = (UInt*)place_to_chain;
   vassert(((HWord)p & 3) == 0);
   vassert(is_LoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)disp_cp_chain_me_EXPECTED));
   vassert(p[4] == emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1));

   /* And what we want to change it to is:
    *  la $t0, place_to_jump_to
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    *
    * The replacement has the same length as the original.
    */
   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)place_to_jump_to);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   VexInvalRange vir = { (HWord)place_to_chain, 4 * 4 + 4 };
   return vir;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_LOONGARCH64 ( VexEndness endness_host,
                                           void* place_to_unchain,
                                           const void* place_to_jump_to_EXPECTED,
                                           const void* disp_cp_chain_me )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
    *  la $t0, place_to_jump_to_EXPECTED
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    */
   UInt* p = (UInt*)place_to_unchain;
   vassert(((HWord)p & 3) == 0);
   vassert(is_LoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)place_to_jump_to_EXPECTED));
   vassert(p[4] == emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1));

   /* And what we want to change it to is:
    *  la $t0, disp_cp_chain_me
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    *
    * The replacement has the same length as the original.
    */
   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)disp_cp_chain_me);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   VexInvalRange vir = { (HWord)place_to_unchain, 4 * 4 + 4 };
   return vir;
}

/* Patch the counter address into a profile inc point, as previously
   created by the mkProfInc. */
VexInvalRange patchProfInc_LOONGARCH64 ( VexEndness endness_host,
                                         void*  place_to_patch,
                                         const ULong* location_of_counter )
{
   vassert(endness_host == VexEndnessLE);
   vassert(sizeof(ULong*) == 8);

   /*
      $t0 = NotKnownYet
      ld.d   $t1, $t0, 0
      addi.d $t1, $t1, 1
      st.d   $t1, $t0, 0
    */
   UInt* p = (UInt*)place_to_patch;
   vassert(((HWord)p & 3) == 0);
   vassert(is_LoadImm_EXACTLY4(p, hregT0(), 0x6555755585559555UL));
   vassert(p[4] == emit_op_si12_rj_rd(LAload_LD_D, 0, 12, 13));
   vassert(p[5] == emit_op_si12_rj_rd(LAbin_ADDI_D, 1, 13, 13));
   vassert(p[6] == emit_op_si12_rj_rd(LAstore_ST_D, 0, 12, 13));

   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)location_of_counter);

   VexInvalRange vir = { (HWord)place_to_patch, 4 * 4 };
   return vir;
}


/*---------------------------------------------------------------*/
/*--- end                             host_loongarch64_defs.c ---*/
/*---------------------------------------------------------------*/
