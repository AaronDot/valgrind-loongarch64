
/*--------------------------------------------------------------------*/
/*--- begin                               guest_loongarch64_toIR.c ---*/
/*--------------------------------------------------------------------*/

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

/* "Special" instructions.

   This instruction decoder can decode four special instructions
   which mean nothing natively (are no-ops as far as regs/mem are
   concerned) but have meaning for supporting Valgrind.  A special
   instruction is flagged by a 16-byte preamble:

      00450c00  (srli.d $zero, $zero, 3
      00453400   srli.d $zero, $zero, 13
      00457400   srli.d $zero, $zero, 29
      00454c00   srli.d $zero, $zero, 19)

   Following that, one of the following 3 are allowed
   (standard interpretation in parentheses):

      001535ad  (or $t1, $t1, $t1)  $a7 = client_request ( $t0 )
      001539ce  (or $t2, $t2, $t2)  $a7 = guest_NRADDR
      00153def  (or $t3, $t3, $t3)  call-noredir $t8
      00154210  (or $t4, $t4, $t4)  IR injection

   Any other bytes following the 16-byte preamble are illegal and
   constitute a failure in instruction decoding.  This all assumes
   that the preamble will never occur except in specific code
   fragments designed for Valgrind to catch.
*/

/* Translates LOONGARCH64 code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_loongarch64.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_loongarch64_defs.h"


/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a instruction, so
   that we don't have to pass them around endlessly.  CONST means does
   not change during translation of the instruction. */

/* CONST: what is the host's endianness?  We need to know this in
   order to do sub-register accesses to the SIMD/FP registers
   correctly. */
static VexEndness host_endness;

/* CONST: The guest address for the instruction currently being
   translated.  */
static Addr64 guest_PC_curr_instr;

/* MOD: The IRSB* into which we're generating code. */
static IRSB* irsb;


/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
/*------------------------------------------------------------*/

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

static const HChar* nameIReg( UInt reg )
{
   vassert(reg < 32);
   static const HChar* reg_names[32] = {
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
   return reg_names[reg];
}

static const HChar* nameFReg( UInt reg )
{
   vassert(reg < 32);
   static const HChar* reg_names[32] = {
      "$fa0",  "$fa1",  "$fa2",  "$fa3",  "$fa4",  "$fa5",  "$fa6",  "$fa7",
      "$ft0",  "$ft1",  "$ft2",  "$ft3",  "$ft4",  "$ft5",  "$ft6",  "$ft7",
      "$ft8",  "$ft9",  "$ft10", "$ft11", "$ft12", "$ft13", "$ft14", "$ft15",
      "$fs0",  "$fs1",  "$fs2",  "$fs3",  "$fs4",  "$fs5",  "$fs6",  "$fs7"
   };
   return reg_names[reg];
}

static const HChar* nameVReg( UInt reg )
{
   vassert(reg < 32);
   static const HChar* reg_names[32] = {
      "$vr0",  "$vr1",  "$vr2",  "$vr3",  "$vr4",  "$vr5",  "$vr6",  "$vr7",
      "$vr8",  "$vr9",  "$vr10", "$vr11", "$vr12", "$vr13", "$vr14", "$vr15",
      "$vr16", "$vr17", "$vr18", "$vr19", "$vr20", "$vr21", "$vr22", "$vr23",
      "$vr24", "$vr25", "$vr26", "$vr27", "$vr28", "$vr29", "$vr30", "$vr31"
   };
   return reg_names[reg];
}

static const HChar* nameXReg( UInt reg )
{
   vassert(reg < 32);
   static const HChar* reg_names[32] = {
      "$xr0",  "$xr1",  "$xr2",  "$xr3",  "$xr4",  "$xr5",  "$xr6",  "$xr7",
      "$xr8",  "$xr9",  "$xr10", "$xr11", "$xr12", "$xr13", "$xr14", "$xr15",
      "$xr16", "$xr17", "$xr18", "$xr19", "$xr20", "$xr21", "$xr22", "$xr23",
      "$xr24", "$xr25", "$xr26", "$xr27", "$xr28", "$xr29", "$xr30", "$xr31"
   };
   return reg_names[reg];
}

static const HChar* nameFCC( UInt reg )
{
   vassert(reg < 8);
   static const HChar* reg_names[8] = {
      "$fcc0", "$fcc1", "$fcc2", "$fcc3", "$fcc4", "$fcc5", "$fcc6", "$fcc7"
   };
   return reg_names[reg];
}

static const HChar* nameFCSR( UInt reg )
{
   vassert(reg < 4);
   static const HChar* reg_names[4] = {
      "$fcsr0", "$fcsr1", "$fcsr2", "$fcsr3"
   };
   return reg_names[reg];
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- loongarch64 insn stream.                             ---*/
/*------------------------------------------------------------*/

/* Get insn[max:min] */
#define SLICE(insn, max, min) \
   ((((UInt)(insn)) >> (min)) & (UInt)((1ULL << ((max) - (min) + 1)) - 1ULL))

/* Do a little-endian load of a 32-bit word, regardless of the
   endianness of the underlying host. */
static inline UInt getUInt ( const UChar* p )
{
   UInt w = 0;
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
   return w;
}

/* Sign extend to 32-bit */
static inline UInt extend32 ( UInt imm, UInt size )
{
   UInt shift = 32 - size;
   return (UInt)(((Int)imm << shift) >> shift);
}

/* Sign extend to 64-bit */
static inline ULong extend64 ( ULong imm, UInt size )
{
   UInt shift = 64 - size;
   return (ULong)(((Long)imm << shift) >> shift);
}

static inline UInt get_rd ( UInt insn )
{
   return SLICE(insn, 4, 0);
}

static inline UInt get_rj ( UInt insn )
{
   return SLICE(insn, 9, 5);
}

static inline UInt get_rk ( UInt insn )
{
   return SLICE(insn, 14, 10);
}

static inline UInt get_code ( UInt insn )
{
   return SLICE(insn, 14, 0);
}

static inline UInt get_ui5 ( UInt insn )
{
   return SLICE(insn, 14, 10);
}

static inline UInt get_ui6 ( UInt insn )
{
   return SLICE(insn, 15, 10);
}

static inline UInt get_sa2 ( UInt insn )
{
   return SLICE(insn, 16, 15);
}

static inline UInt get_sa3 ( UInt insn )
{
   return SLICE(insn, 17, 15);
}

static inline UInt get_lsbw ( UInt insn )
{
   return SLICE(insn, 14, 10);
}

static inline UInt get_msbw ( UInt insn )
{
   return SLICE(insn, 20, 16);
}

static inline UInt get_lsbd ( UInt insn )
{
   return SLICE(insn, 15, 10);
}

static inline UInt get_msbd ( UInt insn )
{
   return SLICE(insn, 21, 16);
}

static inline UInt get_si12 ( UInt insn )
{
   return SLICE(insn, 21, 10);
}

static inline UInt get_ui12 ( UInt insn )
{
   return SLICE(insn, 21, 10);
}

static inline UInt get_si14 ( UInt insn )
{
   return SLICE(insn, 23, 10);
}

static inline UInt get_si16 ( UInt insn )
{
   return SLICE(insn, 25, 10);
}

static inline UInt get_si20 ( UInt insn )
{
   return SLICE(insn, 24, 5);
}

static inline UInt get_hint5 ( UInt insn )
{
   return SLICE(insn, 4, 0);
}

static inline UInt get_hint15 ( UInt insn )
{
   return SLICE(insn, 14, 0);
}

static inline UInt get_offs16 ( UInt insn )
{
   return SLICE(insn, 25, 10);
}

static inline UInt get_offs21 ( UInt insn )
{
   return (SLICE(insn, 4, 0) << 16) | SLICE(insn, 25, 10);
}

static inline UInt get_offs26 ( UInt insn )
{
   return (SLICE(insn, 9, 0) << 16) | SLICE(insn, 25, 10);
}

static inline UInt get_fd ( UInt insn )
{
   return SLICE(insn, 4, 0);
}

static inline UInt get_fj ( UInt insn )
{
   return SLICE(insn, 9, 5);
}

static inline UInt get_fk ( UInt insn )
{
   return SLICE(insn, 14, 10);
}

static inline UInt get_fa ( UInt insn )
{
   return SLICE(insn, 19, 15);
}

static inline UInt get_cond ( UInt insn )
{
   return SLICE(insn, 19, 15);
}

static inline UInt get_fcsrl ( UInt insn )
{
   return SLICE(insn, 4, 0);
}

static inline UInt get_fcsrh ( UInt insn )
{
   return SLICE(insn, 9, 5);
}

static inline UInt get_cd ( UInt insn )
{
   return SLICE(insn, 2, 0);
}

static inline UInt get_cj ( UInt insn )
{
   return SLICE(insn, 7, 5);
}

static inline UInt get_ca ( UInt insn )
{
   return SLICE(insn, 17, 15);
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

static inline IRExpr* mkU64 ( ULong i )
{
   return IRExpr_Const(IRConst_U64(i));
}

static inline IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static inline IRExpr* mkU16 ( UShort i )
{
   return IRExpr_Const(IRConst_U16(i));
}

static inline IRExpr* mkU8 ( UInt i )
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8((UChar)i));
}

static inline IRExpr* mkU1 ( UInt i )
{
   vassert(i == 0 || i == 1);
   return IRExpr_Const(IRConst_U1((Bool)i));
}

static inline IRExpr* mkF64i ( ULong i )
{
   return IRExpr_Const(IRConst_F64i(i));
}

static inline IRExpr* mkF32i ( UInt i )
{
   return IRExpr_Const(IRConst_F32i(i));
}

static inline IRExpr* mkV128 ( UShort i )
{
   return IRExpr_Const(IRConst_V128(i));
}

static inline IRExpr* mkexpr ( IRTemp tmp )
{
   return IRExpr_RdTmp(tmp);
}

static inline IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

static inline IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}

static inline IRExpr* triop ( IROp op, IRExpr* a1, IRExpr* a2, IRExpr* a3 )
{
   return IRExpr_Triop(op, a1, a2, a3);
}

static inline IRExpr* qop ( IROp op, IRExpr* a1, IRExpr* a2,
                            IRExpr* a3, IRExpr* a4 )
{
   return IRExpr_Qop(op, a1, a2, a3, a4);
}

static inline IRExpr* load ( IRType ty, IRExpr* addr )
{
   return IRExpr_Load(Iend_LE, ty, addr);
}

/* Add a statement to the list held by "irbb". */
static inline void stmt ( IRStmt* st )
{
   addStmtToIRSB(irsb, st);
}

static inline void store ( IRExpr* addr, IRExpr* data )
{
   stmt(IRStmt_Store(Iend_LE, addr, data));
}

static inline void assign ( IRTemp dst, IRExpr* e )
{
   stmt(IRStmt_WrTmp(dst, e));
}

static inline void exit ( IRExpr* e, IRJumpKind jk, ULong offs )
{
   stmt(IRStmt_Exit(e, jk, IRConst_U64(guest_PC_curr_instr + offs),
                    offsetof(VexGuestLOONGARCH64State, guest_PC)));
}

/* Generate an expression to check if addr is aligned. */
static inline IRExpr* check_align ( IRExpr* addr, IRExpr* align )
{
   return binop(Iop_CmpNE64, binop(Iop_And64, addr, align),
                IRExpr_Get(offsetof(VexGuestLOONGARCH64State, guest_R0),
                           Ity_I64));
}

/* Generate a SIGSYS if the expression evaluates to true. */
static inline void gen_SIGSYS ( IRExpr* cond )
{
   exit(cond, Ijk_SigSYS, 4);
}

/* Generate a SIGBUS if the expression evaluates to true. */
static inline void gen_SIGBUS ( IRExpr* cond )
{
   exit(cond, Ijk_SigBUS, 4);
}

static inline void cas ( IRTemp old, IRExpr* addr, IRExpr* expd, IRExpr* new )
{
   IRCAS* c = mkIRCAS(IRTemp_INVALID, old, Iend_LE, addr,
                      NULL, expd, NULL, new);
   stmt(IRStmt_CAS(c));
}

/* Generate a new temporary of the given type. */
static inline IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp(irsb->tyenv, ty);
}

/* S-extend 8/16/32 bit int expr to 64. */
static IRExpr* extendS ( IRType ty, IRExpr* e )
{
   switch (ty) {
      case Ity_I1:  return unop(Iop_1Sto64, e);
      case Ity_I8:  return unop(Iop_8Sto64, e);
      case Ity_I16: return unop(Iop_16Sto64, e);
      case Ity_I32: return unop(Iop_32Sto64, e);
      default: vassert(0);
   }
}

/* Z-extend 8/16/32 bit int expr to 64. */
static IRExpr* extendU ( IRType ty, IRExpr* e )
{
   switch (ty) {
      case Ity_I1:  return unop(Iop_1Uto64, e);
      case Ity_I8:  return unop(Iop_8Uto64, e);
      case Ity_I16: return unop(Iop_16Uto64, e);
      case Ity_I32: return unop(Iop_32Uto64, e);
      default: vassert(0);
   }
}

/* Break a V256-bit value up into two V128s. */
static void breakupV256toV128s ( IRTemp t256,
                                 IRTemp* hi, IRTemp* lo )
{
   vassert(hi && *hi == IRTemp_INVALID);
   vassert(lo && *lo == IRTemp_INVALID);
   *hi = newTemp(Ity_V128);
   *lo = newTemp(Ity_V128);
   assign(*hi, unop(Iop_V256toV128_1, mkexpr(t256)));
   assign(*lo, unop(Iop_V256toV128_0, mkexpr(t256)));
}

// static IROp mkVecEXTHTS ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_WidenHIto16Sx8, Iop_WidenHIto32Sx4,
//           Iop_WidenHIto64Sx2, Iop_WidenHIto128Sx1 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecEXTHTU ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_WidenHIto16Ux8, Iop_WidenHIto32Ux4,
//           Iop_WidenHIto64Ux2, Iop_WidenHIto128Ux1 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecADD ( UInt size ) {
//    const IROp ops[5]
//       = { Iop_Add8x16, Iop_Add16x8, Iop_Add32x4, Iop_Add64x2, Iop_Add128x1 };
//    vassert(size < 5);
//    return ops[size];
// }

// static IROp mkVecQADDU ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_QAdd8Ux16, Iop_QAdd16Ux8, Iop_QAdd32Ux4, Iop_QAdd64Ux2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecQADDS ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_QAdd8Sx16, Iop_QAdd16Sx8, Iop_QAdd32Sx4, Iop_QAdd64Sx2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecSUB ( UInt size ) {
//    const IROp ops[5]
//       = { Iop_Sub8x16, Iop_Sub16x8, Iop_Sub32x4, Iop_Sub64x2, Iop_Sub128x1 };
//    vassert(size < 5);
//    return ops[size];
// }

// static IROp mkVecQSUBU ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_QSub8Ux16, Iop_QSub16Ux8, Iop_QSub32Ux4, Iop_QSub64Ux2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecQSUBS ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_QSub8Sx16, Iop_QSub16Sx8, Iop_QSub32Sx4, Iop_QSub64Sx2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecMUL ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Mul8x16, Iop_Mul16x8, Iop_Mul32x4, Iop_INVALID };
//    vassert(size < 3);
//    return ops[size];
// }

// static IROp mkVecMUHS ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_MulHi8Sx16, Iop_MulHi16Sx8, Iop_MulHi32Sx4, Iop_INVALID };
//    vassert(size < 3);
//    return ops[size];
// }

// static IROp mkVecMUHU ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_MulHi8Ux16, Iop_MulHi16Ux8, Iop_MulHi32Ux4, Iop_INVALID };
//    vassert(size < 3);
//    return ops[size];
// }

static IROp mkVecPACKOD ( UInt size ) {
   const IROp ops[4]
      = { Iop_PackOddLanes8x16, Iop_PackOddLanes16x8,
          Iop_PackOddLanes32x4, Iop_InterleaveHI64x2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecPACKEV ( UInt size ) {
   const IROp ops[4]
      = { Iop_PackEvenLanes8x16, Iop_PackEvenLanes16x8,
          Iop_PackEvenLanes32x4, Iop_InterleaveLO64x2 };
   vassert(size < 4);
   return ops[size];
}

// static IROp mkVecAVGU ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Avg8Ux16, Iop_Avg16Ux8, Iop_Avg32Ux4, Iop_Avg64Ux2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecAVGS ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Avg8Sx16, Iop_Avg16Sx8, Iop_Avg32Sx4, Iop_Avg64Sx2 };
//    vassert(size < 4);
//    return ops[size];
// }

static IROp mkVecCMPEQ ( UInt size ) {
   const IROp ops[4]
      = { Iop_CmpEQ8x16, Iop_CmpEQ16x8, Iop_CmpEQ32x4, Iop_CmpEQ64x2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecCMPGTS ( UInt size ) {
   const IROp ops[4]
      = { Iop_CmpGT8Sx16, Iop_CmpGT16Sx8, Iop_CmpGT32Sx4, Iop_CmpGT64Sx2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecCMPGTU ( UInt size ) {
   const IROp ops[4]
      = { Iop_CmpGT8Ux16, Iop_CmpGT16Ux8, Iop_CmpGT32Ux4, Iop_CmpGT64Ux2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecINTERLEAVELO ( UInt size ) {
   const IROp ops[4]
      = { Iop_InterleaveLO8x16, Iop_InterleaveLO16x8,
          Iop_InterleaveLO32x4, Iop_InterleaveLO64x2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecINTERLEAVEHI ( UInt size ) {
   const IROp ops[4]
      = { Iop_InterleaveHI8x16, Iop_InterleaveHI16x8,
          Iop_InterleaveHI32x4, Iop_InterleaveHI64x2 };
   vassert(size < 4);
   return ops[size];
}

// static IROp mkVecABS ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Abs8x16, Iop_Abs16x8, Iop_Abs32x4, Iop_Abs64x2 };
//    vassert(size < 4);
//    return ops[size];
// }

static IROp mkVecMAXU ( UInt size ) {
   const IROp ops[4]
      = { Iop_Max8Ux16, Iop_Max16Ux8, Iop_Max32Ux4, Iop_Max64Ux2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecMAXS ( UInt size ) {
   const IROp ops[4]
      = { Iop_Max8Sx16, Iop_Max16Sx8, Iop_Max32Sx4, Iop_Max64Sx2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecMINU ( UInt size ) {
   const IROp ops[4]
      = { Iop_Min8Ux16, Iop_Min16Ux8, Iop_Min32Ux4, Iop_Min64Ux2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecMINS ( UInt size ) {
   const IROp ops[4]
      = { Iop_Min8Sx16, Iop_Min16Sx8, Iop_Min32Sx4, Iop_Min64Sx2 };
   vassert(size < 4);
   return ops[size];
}

// static IROp mkVecSHL ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Shl8x16, Iop_Shl16x8, Iop_Shl32x4, Iop_Shl64x2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecSHR ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Shr8x16, Iop_Shr16x8, Iop_Shr32x4, Iop_Shr64x2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecSAR ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Sar8x16, Iop_Sar16x8, Iop_Sar32x4, Iop_Sar64x2 };
//    vassert(size < 4);
//    return ops[size];
// }

static IROp mkVecSHLN ( UInt size ) {
   const IROp ops[4]
      = { Iop_ShlN8x16, Iop_ShlN16x8, Iop_ShlN32x4, Iop_ShlN64x2 };
   vassert(size < 4);
   return ops[size];
}

static IROp mkVecSHRN ( UInt size ) {
   const IROp ops[4]
      = { Iop_ShrN8x16, Iop_ShrN16x8, Iop_ShrN32x4, Iop_ShrN64x2 };
   vassert(size < 4);
   return ops[size];
}

// static IROp mkVecSARN ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_SarN8x16, Iop_SarN16x8, Iop_SarN32x4, Iop_SarN64x2 };
//    vassert(size < 4);
//    return ops[size];
// }

// static IROp mkVecCLS ( UInt size ) {
//    const IROp ops[3]
//       = { Iop_Cls8x16, Iop_Cls16x8, Iop_Cls32x4 };
//    vassert(size < 3);
//    return ops[size];
// }

// static IROp mkVecCLZ ( UInt size ) {
//    const IROp ops[4]
//       = { Iop_Clz8x16, Iop_Clz16x8, Iop_Clz32x4, Iop_Clz64x2 };
//    vassert(size < 4);
//    return ops[size];
// }

static IROp mkVecGetElem ( UInt size ) {
   const IROp ops[4]
      = { Iop_GetElem8x16, Iop_GetElem16x8,
          Iop_GetElem32x4, Iop_GetElem64x2 };
   vassert(size < 4);
   return ops[size];
}

static const HChar *mkInsSize ( UInt size ) {
   const HChar *insSize[8]
      = { "b",  "h",  "w",  "d", "bu", "hu", "wu", "du" };
   vassert(size < 8);
   return insSize[size];
}

// static const HChar *mkInsExtSize ( UInt size ) {
//    const HChar *insSize[8]
//       = { "h.b",  "w.h",  "d.w",  "q.d", "hu.bu", "wu.hu", "du.wu", "qu.du" };
//    vassert(size < 8);
//    return insSize[size];
// }


/*------------------------------------------------------------*/
/*--- Helpers for accessing guest registers.               ---*/
/*------------------------------------------------------------*/

/* ---------------- Integer registers ---------------- */

static Int offsetIReg ( UInt iregNo )
{
   switch (iregNo) {
      case 0:  return offsetof(VexGuestLOONGARCH64State, guest_R0);
      case 1:  return offsetof(VexGuestLOONGARCH64State, guest_R1);
      case 2:  return offsetof(VexGuestLOONGARCH64State, guest_R2);
      case 3:  return offsetof(VexGuestLOONGARCH64State, guest_R3);
      case 4:  return offsetof(VexGuestLOONGARCH64State, guest_R4);
      case 5:  return offsetof(VexGuestLOONGARCH64State, guest_R5);
      case 6:  return offsetof(VexGuestLOONGARCH64State, guest_R6);
      case 7:  return offsetof(VexGuestLOONGARCH64State, guest_R7);
      case 8:  return offsetof(VexGuestLOONGARCH64State, guest_R8);
      case 9:  return offsetof(VexGuestLOONGARCH64State, guest_R9);
      case 10: return offsetof(VexGuestLOONGARCH64State, guest_R10);
      case 11: return offsetof(VexGuestLOONGARCH64State, guest_R11);
      case 12: return offsetof(VexGuestLOONGARCH64State, guest_R12);
      case 13: return offsetof(VexGuestLOONGARCH64State, guest_R13);
      case 14: return offsetof(VexGuestLOONGARCH64State, guest_R14);
      case 15: return offsetof(VexGuestLOONGARCH64State, guest_R15);
      case 16: return offsetof(VexGuestLOONGARCH64State, guest_R16);
      case 17: return offsetof(VexGuestLOONGARCH64State, guest_R17);
      case 18: return offsetof(VexGuestLOONGARCH64State, guest_R18);
      case 19: return offsetof(VexGuestLOONGARCH64State, guest_R19);
      case 20: return offsetof(VexGuestLOONGARCH64State, guest_R20);
      case 21: return offsetof(VexGuestLOONGARCH64State, guest_R21);
      case 22: return offsetof(VexGuestLOONGARCH64State, guest_R22);
      case 23: return offsetof(VexGuestLOONGARCH64State, guest_R23);
      case 24: return offsetof(VexGuestLOONGARCH64State, guest_R24);
      case 25: return offsetof(VexGuestLOONGARCH64State, guest_R25);
      case 26: return offsetof(VexGuestLOONGARCH64State, guest_R26);
      case 27: return offsetof(VexGuestLOONGARCH64State, guest_R27);
      case 28: return offsetof(VexGuestLOONGARCH64State, guest_R28);
      case 29: return offsetof(VexGuestLOONGARCH64State, guest_R29);
      case 30: return offsetof(VexGuestLOONGARCH64State, guest_R30);
      case 31: return offsetof(VexGuestLOONGARCH64State, guest_R31);
      default: vassert(0);
   }
}

static IRExpr* getIReg8 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I8);
}

static IRExpr* getIReg16 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I16);
}

static IRExpr* getIReg32 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I32);
}

static IRExpr* getIReg64 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I64);
}

static void putIReg ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I64);
   if (iregNo != 0) /* $r0 - constant zero */
      stmt(IRStmt_Put(offsetIReg(iregNo), e));
}

static void putPC ( IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I64);
   stmt(IRStmt_Put(offsetof(VexGuestLOONGARCH64State, guest_PC), e));
}

/* ---------------- Floating point / vector registers ---------------- */

static Int offsetXReg ( UInt iregNo )
{
   switch (iregNo) {
      case 0:  return offsetof(VexGuestLOONGARCH64State, guest_X0);
      case 1:  return offsetof(VexGuestLOONGARCH64State, guest_X1);
      case 2:  return offsetof(VexGuestLOONGARCH64State, guest_X2);
      case 3:  return offsetof(VexGuestLOONGARCH64State, guest_X3);
      case 4:  return offsetof(VexGuestLOONGARCH64State, guest_X4);
      case 5:  return offsetof(VexGuestLOONGARCH64State, guest_X5);
      case 6:  return offsetof(VexGuestLOONGARCH64State, guest_X6);
      case 7:  return offsetof(VexGuestLOONGARCH64State, guest_X7);
      case 8:  return offsetof(VexGuestLOONGARCH64State, guest_X8);
      case 9:  return offsetof(VexGuestLOONGARCH64State, guest_X9);
      case 10: return offsetof(VexGuestLOONGARCH64State, guest_X10);
      case 11: return offsetof(VexGuestLOONGARCH64State, guest_X11);
      case 12: return offsetof(VexGuestLOONGARCH64State, guest_X12);
      case 13: return offsetof(VexGuestLOONGARCH64State, guest_X13);
      case 14: return offsetof(VexGuestLOONGARCH64State, guest_X14);
      case 15: return offsetof(VexGuestLOONGARCH64State, guest_X15);
      case 16: return offsetof(VexGuestLOONGARCH64State, guest_X16);
      case 17: return offsetof(VexGuestLOONGARCH64State, guest_X17);
      case 18: return offsetof(VexGuestLOONGARCH64State, guest_X18);
      case 19: return offsetof(VexGuestLOONGARCH64State, guest_X19);
      case 20: return offsetof(VexGuestLOONGARCH64State, guest_X20);
      case 21: return offsetof(VexGuestLOONGARCH64State, guest_X21);
      case 22: return offsetof(VexGuestLOONGARCH64State, guest_X22);
      case 23: return offsetof(VexGuestLOONGARCH64State, guest_X23);
      case 24: return offsetof(VexGuestLOONGARCH64State, guest_X24);
      case 25: return offsetof(VexGuestLOONGARCH64State, guest_X25);
      case 26: return offsetof(VexGuestLOONGARCH64State, guest_X26);
      case 27: return offsetof(VexGuestLOONGARCH64State, guest_X27);
      case 28: return offsetof(VexGuestLOONGARCH64State, guest_X28);
      case 29: return offsetof(VexGuestLOONGARCH64State, guest_X29);
      case 30: return offsetof(VexGuestLOONGARCH64State, guest_X30);
      case 31: return offsetof(VexGuestLOONGARCH64State, guest_X31);
      default: vassert(0);
   }
}

static Int offsetFCC ( UInt iregNo )
{
   switch (iregNo) {
      case 0:  return offsetof(VexGuestLOONGARCH64State, guest_FCC0);
      case 1:  return offsetof(VexGuestLOONGARCH64State, guest_FCC1);
      case 2:  return offsetof(VexGuestLOONGARCH64State, guest_FCC2);
      case 3:  return offsetof(VexGuestLOONGARCH64State, guest_FCC3);
      case 4:  return offsetof(VexGuestLOONGARCH64State, guest_FCC4);
      case 5:  return offsetof(VexGuestLOONGARCH64State, guest_FCC5);
      case 6:  return offsetof(VexGuestLOONGARCH64State, guest_FCC6);
      case 7:  return offsetof(VexGuestLOONGARCH64State, guest_FCC7);
      default: vassert(0);
   }
}

/* Find the offset of the laneNo'th lane of type laneTy in the given
   Xreg.  Since the host is little-endian, the least significant lane
   has the lowest offset. */
static Int offsetXRegLane ( UInt xregNo, IRType laneTy, UInt laneNo )
{
   vassert(host_endness == VexEndnessLE);
   Int laneSzB;
   /* Since the host is little-endian, the least significant lane
      will be at the lowest address. */
   switch (laneTy) {
      case Ity_F32:  laneSzB = 4;  break;
      case Ity_F64:  laneSzB = 8;  break;
      case Ity_V128: laneSzB = 16; break;
      case Ity_V256: laneSzB = 32; break;
      default:       vassert(0);   break;
   }
   return offsetXReg(xregNo) + laneNo * laneSzB;
}

static IRExpr* getXReg ( UInt xregNo )
{
   return IRExpr_Get(offsetXRegLane(xregNo, Ity_V256, 0), Ity_V256);
}

static IRExpr* getVReg ( UInt vregNo )
{
   return IRExpr_Get(offsetXRegLane(vregNo, Ity_V128, 0), Ity_V128);
}

static IRExpr* getFReg64 ( UInt fregNo )
{
   return IRExpr_Get(offsetXRegLane(fregNo, Ity_F64, 0), Ity_F64);
}

static IRExpr* getFReg32 ( UInt fregNo )
{
   /* Get FReg32 from FReg64.
      We could probably use IRExpr_Get(offsetXRegLane(fregNo, Ity_F32, 0), Ity_F32),
      but that would cause Memcheck to report some errors.
    */
   IRExpr* i = unop(Iop_ReinterpF64asI64, getFReg64(fregNo));
   return unop(Iop_ReinterpI32asF32, unop(Iop_64to32, i));
}

static IRExpr* getFCC ( UInt iregNo )
{
   return IRExpr_Get(offsetFCC(iregNo), Ity_I8);
}

static IRExpr* getFCSR ( UInt iregNo )
{
   /*
      bits  | name
      ---------------
      4:0   | Enables
      7:5   | 0
      9:8   | RM
      15:10 | 0
      20:16 | Flags
      23:21 | 0
      28:24 | Cause
      31:29 | 0
    */
   Int offs = offsetof(VexGuestLOONGARCH64State, guest_FCSR);
   IRExpr* fcsr0 = IRExpr_Get(offs, Ity_I32);
   switch (iregNo) {
      case 0:
         return fcsr0;
      case 1:
         /* FCSR1 is Enables of FCSR0.  It seems that the hardware
            implementation is that the 7th bit belongs to FCSR1. */
         return binop(Iop_And32, fcsr0, mkU32(0x0000009f));
      case 2:
         /* FCSR2 is Cause and Flags of FCSR0. */
         return binop(Iop_And32, fcsr0, mkU32(0x1f1f0000));
      case 3:
         /* FCSR3 is RM of FCSR0. */
         return binop(Iop_And32, fcsr0, mkU32(0x00000300));
      default:
         vassert(0);
   }
}

static void putFReg32 ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F32);
   stmt(IRStmt_Put(offsetXReg(iregNo), e));
}

static void putFReg64 ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F64);
   stmt(IRStmt_Put(offsetXReg(iregNo), e));
}

static void putVReg ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_V128);
   stmt(IRStmt_Put(offsetXReg(iregNo), e));
}

static void putXReg ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_V256);
   stmt(IRStmt_Put(offsetXReg(iregNo), e));
}

static void putFCC ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I8);
   stmt(IRStmt_Put(offsetFCC(iregNo), e));
}

static void putFCSR ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I32);
   IRExpr* fcsr0 = getFCSR(0);
   IRExpr* and1;
   IRExpr* and2;
   switch (iregNo) {
      case 0:
         /* It seems that the hardware implementation allows the 6th
            bit and the 7th bit to be non-zero. */
         and1 = getIReg32(0);
         and2 = binop(Iop_And32, e, mkU32(0x1f1f03df));
         break;
      case 1:
         /* FCSR1 is Enables of FCSR0.  It seems that the hardware
            implementation is that the 7th bit belongs to FCSR1. */
         and1 = binop(Iop_And32, fcsr0, mkU32(0xffffff60));
         and2 = binop(Iop_And32, e, mkU32(0x0000009f));
         break;
      case 2:
         /* FCSR2 is Cause and Flags of FCSR0. */
         and1 = binop(Iop_And32, fcsr0, mkU32(0xe0e0ffff));
         and2 = binop(Iop_And32, e, mkU32(0x1f1f0000));
         break;
      case 3:
         /* FCSR3 is RM of FCSR0. */
         and1 = binop(Iop_And32, fcsr0, mkU32(0xfffffcff));
         and2 = binop(Iop_And32, e, mkU32(0x00000300));
         break;
      default:
         vassert(0);
   }
   Int offs = offsetof(VexGuestLOONGARCH64State, guest_FCSR);
   stmt(IRStmt_Put(offs, binop(Iop_Or32, and1, and2)));
}

static IRExpr* get_rounding_mode ( void )
{
   /*
      rounding mode | LOONGARCH | IR
      ------------------------------
      to nearest    | 00        | 00
      to zero       | 01        | 11
      to +infinity  | 10        | 10
      to -infinity  | 11        | 01
   */

   /* Bits 8 to 9 in FCSR are rounding mode. */
   IRExpr* fcsr = getFCSR(0);
   IRExpr* shr = binop(Iop_Shr32, fcsr, mkU8(8));
   IRTemp rm = newTemp(Ity_I32);
   assign(rm, binop(Iop_And32, shr, mkU32(0x3)));

   /* rm = XOR(rm, (rm << 1) & 2) */
   IRExpr* shl = binop(Iop_Shl32, mkexpr(rm), mkU8(1));
   IRExpr* and = binop(Iop_And32, shl, mkU32(2));
   return binop(Iop_Xor32, mkexpr(rm), and);
}

static void calculateFCSR ( enum fpop op, UInt nargs,
                            UInt src1, UInt src2, UInt src3 )
{
   IRExpr* s1 = NULL;
   IRExpr* s2 = NULL;
   IRExpr* s3 = NULL;
   switch (nargs) {
      case 3: s3 = unop(Iop_ReinterpF64asI64, getFReg64(src3)); /* fallthrough */
      case 2: s2 = unop(Iop_ReinterpF64asI64, getFReg64(src2)); /* fallthrough */
      case 1: s1 = unop(Iop_ReinterpF64asI64, getFReg64(src1)); break;
      default: vassert(0);
   }
   IRExpr** arg = mkIRExprVec_4(mkU64(op), s1, s2, s3);
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_FCSR",
                                &loongarch64_calculate_FCSR,
                                arg);
   IRTemp fcsr2 = newTemp(Ity_I32);
   assign(fcsr2, unop(Iop_64to32, call));
   putFCSR(2, mkexpr(fcsr2));
}

static IRExpr* gen_round_to_nearest ( void )
{
   return mkU32(0x0);
}

static IRExpr* gen_round_down ( void )
{
   return mkU32(0x1);
}

static IRExpr* gen_round_up ( void )
{
   return mkU32(0x2);
}

static IRExpr* gen_round_to_zero ( void )
{
   return mkU32(0x3);
}

static IRTemp newTempV128 ( void )
{
   return newTemp(Ity_V128);
}


/*------------------------------------------------------------*/
/*--- Helpers for fixed point arithmetic insns             ---*/
/*------------------------------------------------------------*/

static Bool gen_add_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("add.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* add = binop(Iop_Add32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, add));

   return True;
}

static Bool gen_add_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("add.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_Add64, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_sub_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("sub.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* sub = binop(Iop_Sub32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, sub));

   return True;
}

static Bool gen_sub_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("sub.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_Sub64, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_slt ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("slt %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* cond = binop(Iop_CmpLT64S, getIReg64(rj), getIReg64(rk));
   putIReg(rd, extendU(Ity_I1, cond));

   return True;
}

static Bool gen_sltu ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("sltu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* cond = binop(Iop_CmpLT64U, getIReg64(rj), getIReg64(rk));
   putIReg(rd, extendU(Ity_I1, cond));

   return True;
}

static Bool gen_slti ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt rj   = get_rj(insn);
   UInt rd   = get_rd(insn);

   DIP("slti %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                           (Int)extend32(si12, 12));

   IRExpr* cond = binop(Iop_CmpLT64S, getIReg64(rj),
                        mkU64(extend64(si12, 12)));
   putIReg(rd, extendU(Ity_I1, cond));

   return True;
}

static Bool gen_sltui ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt rj   = get_rj(insn);
   UInt rd   = get_rd(insn);

   DIP("sltui %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   IRExpr* cond = binop(Iop_CmpLT64U, getIReg64(rj),
                        mkU64(extend64(si12, 12)));
   putIReg(rd, extendU(Ity_I1, cond));

   return True;
}

static Bool gen_nor ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("nor %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* or = binop(Iop_Or64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, unop(Iop_Not64, or));

   return True;
}

static Bool gen_and ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("and %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_And64, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_or ( DisResult* dres, UInt insn,
                     const VexArchInfo* archinfo,
                     const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("or %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_Or64, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_xor ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("xor %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_Xor64, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_orn ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("orn %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* not = unop(Iop_Not64, getIReg64(rk));
   putIReg(rd, binop(Iop_Or64, getIReg64(rj), not));

   return True;
}

static Bool gen_andn ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("andn %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* not = unop(Iop_Not64, getIReg64(rk));
   putIReg(rd, binop(Iop_And64, getIReg64(rj), not));

   return True;
}

static Bool gen_mul_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mul.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mul = binop(Iop_MullS32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, unop(Iop_64to32, mul)));

   return True;
}

static Bool gen_mulh_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mulh.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mul = binop(Iop_MullS32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, unop(Iop_64HIto32, mul)));

   return True;
}

static Bool gen_mulh_wu ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mulh.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mul = binop(Iop_MullU32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, unop(Iop_64HIto32, mul)));

   return True;
}

static Bool gen_mul_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mul.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mul = binop(Iop_MullS64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, unop(Iop_128to64, mul));

   return True;
}

static Bool gen_mulh_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mulh.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mul = binop(Iop_MullS64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, unop(Iop_128HIto64, mul));

   return True;
}

static Bool gen_mulh_du ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mulh.du %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mul = binop(Iop_MullU64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, unop(Iop_128HIto64, mul));

   return True;
}

static Bool gen_mulw_d_w ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mulw.d.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_MullS32, getIReg32(rj), getIReg32(rk)));

   return True;
}

static Bool gen_mulw_d_wu ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mulw.d.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_MullU32, getIReg32(rj), getIReg32(rk)));

   return True;
}

static Bool gen_div_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("div.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* div = binop(Iop_DivS32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, div));

   return True;
}

static Bool gen_mod_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mod.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mod = binop(Iop_DivModS32to32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, unop(Iop_64HIto32, mod)));

   return True;
}

static Bool gen_div_wu ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("div.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* div = binop(Iop_DivU32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, div));

   return True;
}

static Bool gen_mod_wu ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mod.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mod = binop(Iop_DivModU32to32, getIReg32(rj), getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, unop(Iop_64HIto32, mod)));

   return True;
}

static Bool gen_div_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("div.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_DivS64, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_mod_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mod.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mod = binop(Iop_DivModS64to64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, unop(Iop_128HIto64, mod));

   return True;
}

static Bool gen_div_du ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("div.du %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_DivU64, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_mod_du ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("mod.du %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* mod = binop(Iop_DivModU64to64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, unop(Iop_128HIto64, mod));

   return True;
}

static Bool gen_alsl_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt sa2 = get_sa2(insn);
   UInt  rk = get_rk(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("alsl.w %s, %s, %s, %u\n", nameIReg(rd), nameIReg(rj),
                                  nameIReg(rk), sa2);

   IRExpr* shl = binop(Iop_Shl32, getIReg32(rj), mkU8(sa2 + 1));
   IRExpr* add = binop(Iop_Add32, shl, getIReg32(rk));
   putIReg(rd, extendS(Ity_I32, add));

   return True;
}

static Bool gen_alsl_wu ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt sa2 = get_sa2(insn);
   UInt  rk = get_rk(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("alsl.wu %s, %s, %s, %u\n", nameIReg(rd), nameIReg(rj),
                                   nameIReg(rk), sa2);

   IRExpr* shl = binop(Iop_Shl32, getIReg32(rj), mkU8(sa2 + 1));
   IRExpr* add = binop(Iop_Add32, shl, getIReg32(rk));
   putIReg(rd, extendU(Ity_I32, add));

   return True;
}

static Bool gen_alsl_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt sa2 = get_sa2(insn);
   UInt  rk = get_rk(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("alsl.d %s, %s, %s, %u\n", nameIReg(rd), nameIReg(rj),
                                  nameIReg(rk), sa2);

   IRExpr* shl = binop(Iop_Shl64, getIReg64(rj), mkU8(sa2 + 1));
   putIReg(rd, binop(Iop_Add64, shl, getIReg64(rk)));

   return True;
}

static Bool gen_lu12i_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt si20 = get_si20(insn);
   UInt   rd = get_rd(insn);

   DIP("lu12i.w %s, %d\n", nameIReg(rd), (Int)extend32(si20, 20));

   IRExpr* imm = mkU32(si20 << 12);
   putIReg(rd, extendS(Ity_I32, imm));

   return True;
}

static Bool gen_lu32i_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt si20 = get_si20(insn);
   UInt   rd = get_rd(insn);

   DIP("lu32i.d %s, %d\n", nameIReg(rd), (Int)extend32(si20, 20));

   IRExpr* imm = mkU64((ULong)extend32(si20, 20) << 32);
   IRExpr* shl = binop(Iop_Shl64, getIReg64(rd), mkU8(32));
   IRExpr* shr = binop(Iop_Shr64, shl, mkU8(32));
   putIReg(rd, binop(Iop_Or64, imm, shr));

   return True;
}

static Bool gen_lu52i_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("lu52i.d %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                               (Int)extend32(si12, 12));

   IRExpr* imm = mkU64((ULong)si12 << 52);
   IRExpr* shl = binop(Iop_Shl64, getIReg64(rj), mkU8(12));
   IRExpr* shr = binop(Iop_Shr64, shl, mkU8(12));
   putIReg(rd, binop(Iop_Or64, imm, shr));

   return True;
}

static Bool gen_pcaddi ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt si20 = get_si20(insn);
   UInt   rd = get_rd(insn);

   DIP("pcaddi %s, %d\n", nameIReg(rd), (Int)extend32(si20, 20));

   putIReg(rd, mkU64(guest_PC_curr_instr + extend64(si20 << 2, 22)));

   return True;
}

static Bool gen_pcalau12i ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt si20 = get_si20(insn);
   UInt   rd = get_rd(insn);

   DIP("pcalau12i %s, %d\n", nameIReg(rd), (Int)extend32(si20, 20));

   IRExpr* imm = mkU64(guest_PC_curr_instr + extend64(si20 << 12, 32));
   IRExpr* shr = binop(Iop_Shr64, imm, mkU8(12));
   putIReg(rd, binop(Iop_Shl64, shr, mkU8(12)));

   return True;
}

static Bool gen_pcaddu12i ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt si20 = get_si20(insn);
   UInt   rd = get_rd(insn);

   DIP("pcaddu12i %s, %d\n", nameIReg(rd), (Int)extend32(si20, 20));

   putIReg(rd, mkU64(guest_PC_curr_instr + extend64(si20 << 12, 32)));

   return True;
}

static Bool gen_pcaddu18i ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt si20 = get_si20(insn);
   UInt   rd = get_rd(insn);

   DIP("pcaddu18i %s, %d\n", nameIReg(rd), (Int)extend32(si20, 20));

   putIReg(rd, mkU64(guest_PC_curr_instr + extend64((ULong)si20 << 18, 38)));

   return True;
}

static Bool gen_addi_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("addi.w %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                              (Int)extend32(si12, 12));

   IRExpr* imm = mkU32(extend32(si12, 12));
   IRExpr* add = binop(Iop_Add32, getIReg32(rj), imm);
   putIReg(rd, extendS(Ity_I32, add));

   return True;
}

static Bool gen_addi_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("addi.d %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                              (Int)extend32(si12, 12));

   IRExpr* imm = mkU64(extend64(si12, 12));
   putIReg(rd, binop(Iop_Add64, getIReg64(rj), imm));

   return True;
}

static Bool gen_addu16i_d ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt si16 = get_si16(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("addu16i.d %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                                 (Int)extend32(si16, 16));

   IRExpr* imm = mkU64(extend64(si16 << 16, 32));
   putIReg(rd, binop(Iop_Add64, getIReg64(rj), imm));

   return True;
}

static Bool gen_andi ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt ui12 = get_ui12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("andi %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui12);

   IRExpr* imm = mkU64((ULong)ui12);
   putIReg(rd, binop(Iop_And64, getIReg64(rj), imm));

   return True;
}

static Bool gen_ori ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt ui12 = get_ui12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ori %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui12);

   IRExpr* imm = mkU64((ULong)ui12);
   putIReg(rd, binop(Iop_Or64, getIReg64(rj), imm));

   return True;
}

static Bool gen_xori ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt ui12 = get_ui12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("xori %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui12);

   IRExpr* imm = mkU64((ULong)ui12);
   putIReg(rd, binop(Iop_Xor64, getIReg64(rj), imm));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for fixed point shift insns                  ---*/
/*------------------------------------------------------------*/

static Bool gen_sll_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("sll.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* shl = binop(Iop_Shl32, getIReg32(rj), getIReg8(rk));
   putIReg(rd, extendS(Ity_I32, shl));

   return True;
}

static Bool gen_srl_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("srl.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* shr = binop(Iop_Shr32, getIReg32(rj), getIReg8(rk));
   putIReg(rd, extendS(Ity_I32, shr));

   return True;
}

static Bool gen_sra_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("sra.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* sar = binop(Iop_Sar32, getIReg32(rj), getIReg8(rk));
   putIReg(rd, extendS(Ity_I32, sar));

   return True;
}

static Bool gen_sll_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("sll.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_Shl64, getIReg64(rj), getIReg8(rk)));

   return True;
}

static Bool gen_srl_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("srl.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_Shr64, getIReg64(rj), getIReg8(rk)));

   return True;
}

static Bool gen_sra_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("sra.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   putIReg(rd, binop(Iop_Sar64, getIReg64(rj), getIReg8(rk)));

   return True;
}

static Bool gen_rotr_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("rotr.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp tmp1 = newTemp(Ity_I32);
   assign(tmp1, getIReg32(rj));
   IRTemp tmp2 = newTemp(Ity_I8);
   assign(tmp2, getIReg8(rk));
   IRExpr* shr = binop(Iop_Shr32, mkexpr(tmp1), mkexpr(tmp2));
   IRExpr* imm = unop(Iop_8Uto32, mkexpr(tmp2));
   IRExpr* sub = binop(Iop_Sub32, mkU32(32), imm);
   IRExpr* imm2 = unop(Iop_32to8, sub);
   IRExpr* shl = binop(Iop_Shl32, mkexpr(tmp1), imm2);
   IRExpr* or = binop(Iop_Or32, shr, shl);
   putIReg(rd, extendS(Ity_I32, or));

   return True;
}

static Bool gen_rotr_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("rotr.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp tmp1 = newTemp(Ity_I64);
   assign(tmp1, getIReg64(rj));
   IRTemp tmp2 = newTemp(Ity_I8);
   assign(tmp2, getIReg8(rk));
   IRExpr* shr = binop(Iop_Shr64, mkexpr(tmp1), mkexpr(tmp2));
   IRExpr* imm = unop(Iop_8Uto64, mkexpr(tmp2));
   IRExpr* sub = binop(Iop_Sub64, mkU64(64), imm);
   IRExpr* imm2 = unop(Iop_64to8, sub);
   IRExpr* shl = binop(Iop_Shl64, mkexpr(tmp1), imm2);
   putIReg(rd, binop(Iop_Or64, shr, shl));

   return True;
}

static Bool gen_slli_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt ui5 = get_ui5(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("slli.w %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui5);

   IRExpr* shl = binop(Iop_Shl32, getIReg32(rj), mkU8(ui5));
   putIReg(rd, extendS(Ity_I32, shl));

   return True;
}

static Bool gen_slli_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt ui6 = get_ui6(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("slli.d %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui6);

   putIReg(rd, binop(Iop_Shl64, getIReg64(rj), mkU8(ui6)));

   return True;
}

static Bool gen_srli_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt ui5 = get_ui5(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("srli.w %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui5);

   IRExpr* shr = binop(Iop_Shr32, getIReg32(rj), mkU8(ui5));
   putIReg(rd, extendS(Ity_I32, shr));

   return True;
}

static Bool gen_srli_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt ui6 = get_ui6(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("srli.d %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui6);

   putIReg(rd, binop(Iop_Shr64, getIReg64(rj), mkU8(ui6)));

   return True;
}

static Bool gen_srai_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt ui5 = get_ui5(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("srai.w %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui5);

   IRExpr* sar = binop(Iop_Sar32, getIReg32(rj), mkU8(ui5));
   putIReg(rd, extendS(Ity_I32, sar));

   return True;
}

static Bool gen_srai_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt ui6 = get_ui6(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("srai.d %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui6);

   putIReg(rd, binop(Iop_Sar64, getIReg64(rj), mkU8(ui6)));

   return True;
}

static Bool gen_rotri_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt ui5 = get_ui5(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("rotri.w %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui5);

   IRTemp tmp = newTemp(Ity_I32);
   assign(tmp, getIReg32(rj));
   IRExpr* shr = binop(Iop_Shr32, mkexpr(tmp), mkU8(ui5));
   IRExpr* shl = binop(Iop_Shl32, mkexpr(tmp), mkU8(32 - ui5));
   if (32 - ui5 == 32)
      shl = mkU32(0);
   IRExpr* or = binop(Iop_Or32, shr, shl);
   putIReg(rd, extendS(Ity_I32, or));

   return True;
}

static Bool gen_rotri_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt ui6 = get_ui6(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("rotri.d %s, %s, %u\n", nameIReg(rd), nameIReg(rj), ui6);

   IRTemp tmp = newTemp(Ity_I64);
   assign(tmp, getIReg64(rj));
   IRExpr* shr = binop(Iop_Shr64, mkexpr(tmp), mkU8(ui6));
   IRExpr* shl = binop(Iop_Shl64, mkexpr(tmp), mkU8(64 - ui6));
   if (64 - ui6 == 64)
      shl = mkU64(0);
   putIReg(rd, binop(Iop_Or64, shr, shl));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for fixed point bit insns                    ---*/
/*------------------------------------------------------------*/

static Bool gen_ext_w_h ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ext.w.h %s, %s\n", nameIReg(rd), nameIReg(rj));

   putIReg(rd, extendS(Ity_I16, getIReg16(rj)));

   return True;
}

static Bool gen_ext_w_b ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ext.w.b %s, %s\n", nameIReg(rd), nameIReg(rj));

   putIReg(rd, extendS(Ity_I8, getIReg8(rj)));

   return True;
}

static Bool gen_clo_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("clo.w %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr* not = unop(Iop_Not32, getIReg32(rj));
   IRExpr* clz = unop(Iop_Clz32, not);
   putIReg(rd, extendU(Ity_I32, clz));

   return True;
}

static Bool gen_clz_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("clz.w %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr* clz = unop(Iop_Clz32, getIReg32(rj));
   putIReg(rd, extendU(Ity_I32, clz));

   return True;
}

static Bool gen_cto_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("cto.w %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr* not = unop(Iop_Not32, getIReg32(rj));
   IRExpr* clz = unop(Iop_Ctz32, not);
   putIReg(rd, extendU(Ity_I32, clz));

   return True;
}

static Bool gen_ctz_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ctz.w %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr* clz = unop(Iop_Ctz32, getIReg32(rj));
   putIReg(rd, extendU(Ity_I32, clz));

   return True;
}

static Bool gen_clo_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("clo.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr* not = unop(Iop_Not64, getIReg64(rj));
   putIReg(rd, unop(Iop_Clz64, not));

   return True;
}

static Bool gen_clz_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("clz.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   putIReg(rd, unop(Iop_Clz64, getIReg64(rj)));

   return True;
}

static Bool gen_cto_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("cto.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr* not = unop(Iop_Not64, getIReg64(rj));
   putIReg(rd, unop(Iop_Ctz64, not));

   return True;
}

static Bool gen_ctz_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ctz.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   putIReg(rd, unop(Iop_Ctz64, getIReg64(rj)));

   return True;
}

static Bool gen_revb_2h ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("revb.2h %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_revb_2h",
                                &loongarch64_calculate_revb_2h,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_revb_4h ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("revb.4h %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_revb_4h",
                                &loongarch64_calculate_revb_4h,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_revb_2w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("revb.2w %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_revb_2w",
                                &loongarch64_calculate_revb_2w,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_revb_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("revb.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_revb_d",
                                &loongarch64_calculate_revb_d,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_revh_2w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("revh.2w %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_revh_2w",
                                &loongarch64_calculate_revh_2w,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_revh_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("revh.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_revh_d",
                                &loongarch64_calculate_revh_d,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_bitrev_4b ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("bitrev.4b %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_bitrev_4b",
                                &loongarch64_calculate_bitrev_4b,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_bitrev_8b ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("bitrev.8b %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_bitrev_8b",
                                &loongarch64_calculate_bitrev_8b,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_bitrev_w ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("bitrev.w %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_bitrev_w",
                                &loongarch64_calculate_bitrev_w,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_bitrev_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("bitrev.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_bitrev_d",
                                &loongarch64_calculate_bitrev_d,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_bytepick_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt sa2 = get_sa2(insn);
   UInt  rk = get_rk(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("bytepick.w %s, %s, %s, %u\n", nameIReg(rd), nameIReg(rj),
                                      nameIReg(rk), sa2);

   UInt shift = 8 * (4 - sa2);
   IRExpr* shl = binop(Iop_Shl32, getIReg32(rk), mkU8(32 - shift));
   if (32 - shift == 32)
      shl = mkU32(0);
   IRExpr* shr = binop(Iop_Shr32, getIReg32(rj), mkU8(shift));
   if (shift == 32)
      shr = mkU32(0);
   IRExpr* or = binop(Iop_Or32, shl, shr);
   putIReg(rd, extendS(Ity_I32, or));

   return True;
}

static Bool gen_bytepick_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt sa3 = get_sa3(insn);
   UInt  rk = get_rk(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("bytepick.d %s, %s, %s, %u\n", nameIReg(rd), nameIReg(rj),
                                      nameIReg(rk), sa3);

   UInt shift = 8 * (8 - sa3);
   IRExpr* shl = binop(Iop_Shl64, getIReg64(rk), mkU8(64 - shift));
   if (64 - shift == 64)
      shl = mkU64(0);
   IRExpr* shr = binop(Iop_Shr64, getIReg64(rj), mkU8(shift));
   if (shift == 64)
      shr = mkU64(0);
   putIReg(rd, binop(Iop_Or64, shl, shr));

   return True;
}

static Bool gen_maskeqz ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("maskeqz %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* cond = binop(Iop_CmpNE64, getIReg64(rk), mkU64(0));
   putIReg(rd, binop(Iop_And64, extendS(Ity_I1, cond), getIReg64(rj)));

   return True;
}

static Bool gen_masknez ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("masknez %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* cond = binop(Iop_CmpEQ64, getIReg64(rk), mkU64(0));
   putIReg(rd, binop(Iop_And64, extendS(Ity_I1, cond), getIReg64(rj)));

   return True;
}

static Bool gen_bstrins_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt msb = get_msbw(insn);
   UInt lsb = get_lsbw(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("bstrins.w %s, %s, %u, %u\n", nameIReg(rd), nameIReg(rj), msb, lsb);

   IRTemp tmp = newTemp(Ity_I32);
   assign(tmp, getIReg32(rd));
   IRExpr* shl1;
   if (msb == 31) {
      shl1 = mkU32(0);
   } else {
      IRExpr* shr1 = binop(Iop_Shr32, mkexpr(tmp), mkU8(msb + 1));
      shl1 = binop(Iop_Shl32, shr1, mkU8(msb + 1));
   }
   IRExpr* shl2 = binop(Iop_Shl32, getIReg32(rj), mkU8(31 - msb + lsb));
   IRExpr* shr2 = binop(Iop_Shr32, shl2, mkU8(31 - msb));
   IRExpr* shr3;
   if (lsb == 0) {
      shr3 = mkU32(0);
   } else {
      IRExpr* shl3 = binop(Iop_Shl32, mkexpr(tmp), mkU8(32 - lsb));
      shr3 = binop(Iop_Shr32, shl3, mkU8(32 - lsb));
   }
   IRExpr* or1 = binop(Iop_Or32, shl1, shr2);
   IRExpr* or2 = binop(Iop_Or32, or1, shr3);
   putIReg(rd, extendS(Ity_I32, or2));

   return True;
}

static Bool gen_bstrpick_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt msb = get_msbw(insn);
   UInt lsb = get_lsbw(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("bstrpick.w %s, %s, %u, %u\n", nameIReg(rd), nameIReg(rj), msb, lsb);

   IRExpr* shl = binop(Iop_Shl32, getIReg32(rj), mkU8(31 - msb));
   IRExpr* shr = binop(Iop_Shr32, shl, mkU8(31 - msb + lsb));
   putIReg(rd, extendS(Ity_I32, shr));

   return True;
}

static Bool gen_bstrins_d ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt msb = get_msbd(insn);
   UInt lsb = get_lsbd(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("bstrins.d %s, %s, %u, %u\n", nameIReg(rd), nameIReg(rj), msb, lsb);

   IRTemp tmp = newTemp(Ity_I64);
   assign(tmp, getIReg64(rd));
   IRExpr* shl1;
   if (msb == 63) {
      shl1 = mkU64(0);
   } else {
      IRExpr* shr1 = binop(Iop_Shr64, mkexpr(tmp), mkU8(msb + 1));
      shl1 = binop(Iop_Shl64, shr1, mkU8(msb + 1));
   }
   IRExpr* shl2 = binop(Iop_Shl64, getIReg64(rj), mkU8(63 - msb + lsb));
   IRExpr* shr2 = binop(Iop_Shr64, shl2, mkU8(63 - msb));
   IRExpr* shr3;
   if (lsb == 0) {
      shr3 = mkU64(0);
   } else {
      IRExpr* shl3 = binop(Iop_Shl64, mkexpr(tmp), mkU8(64 - lsb));
      shr3 = binop(Iop_Shr64, shl3, mkU8(64 - lsb));
   }
   IRExpr* or = binop(Iop_Or64, shl1, shr2);
   putIReg(rd, binop(Iop_Or64, or, shr3));

   return True;
}

static Bool gen_bstrpick_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt msb = get_msbd(insn);
   UInt lsb = get_lsbd(insn);
   UInt  rj = get_rj(insn);
   UInt  rd = get_rd(insn);

   DIP("bstrpick.d %s, %s, %u, %u\n", nameIReg(rd), nameIReg(rj), msb, lsb);

   IRExpr* shl = binop(Iop_Shl64, getIReg64(rj), mkU8(63 - msb));
   putIReg(rd, binop(Iop_Shr64, shl, mkU8(63 - msb + lsb)));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for fixed point load/store insns             ---*/
/*------------------------------------------------------------*/

static Bool gen_ld_b ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ld.b %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   putIReg(rd, extendS(Ity_I8, load(Ity_I8, addr)));

   return True;
}

static Bool gen_ld_h ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ld.h %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x1)));
   putIReg(rd, extendS(Ity_I16, load(Ity_I16, addr)));

   return True;
}

static Bool gen_ld_w ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ld.w %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   putIReg(rd, extendS(Ity_I32, load(Ity_I32, addr)));

   return True;
}

static Bool gen_ld_d ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ld.d %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   putIReg(rd, load(Ity_I64, addr));

   return True;
}

static Bool gen_st_b ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("st.b %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   store(addr, getIReg8(rd));

   return True;
}

static Bool gen_st_h ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("st.h %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x1)));
   store(addr, getIReg16(rd));

   return True;
}

static Bool gen_st_w ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("st.w %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   store(addr, getIReg32(rd));

   return True;
}

static Bool gen_st_d ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("st.d %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   store(addr, getIReg64(rd));

   return True;
}

static Bool gen_ld_bu ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ld.bu %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   putIReg(rd, extendU(Ity_I8, load(Ity_I8, addr)));

   return True;
}

static Bool gen_ld_hu ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ld.hu %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x1)));
   putIReg(rd, extendU(Ity_I16, load(Ity_I16, addr)));

   return True;
}

static Bool gen_ld_wu ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ld.wu %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   putIReg(rd, extendU(Ity_I32, load(Ity_I32, addr)));

   return True;
}

static Bool gen_ldx_b ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldx.b %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, extendS(Ity_I8, load(Ity_I8, addr)));

   return True;
}

static Bool gen_ldx_h ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldx.h %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x1)));
   putIReg(rd, extendS(Ity_I16, load(Ity_I16, addr)));

   return True;
}

static Bool gen_ldx_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldx.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   putIReg(rd, extendS(Ity_I32, load(Ity_I32, addr)));

   return True;
}

static Bool gen_ldx_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldx.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   putIReg(rd, load(Ity_I64, addr));

   return True;
}

static Bool gen_stx_b ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stx.b %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   store(addr, getIReg8(rd));

   return True;
}

static Bool gen_stx_h ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stx.h %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x1)));
   store(addr, getIReg16(rd));

   return True;
}

static Bool gen_stx_w ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stx.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   store(addr, getIReg32(rd));

   return True;
}

static Bool gen_stx_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stx.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   store(addr, getIReg64(rd));

   return True;
}

static Bool gen_ldx_bu ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldx.bu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   putIReg(rd, extendU(Ity_I8, load(Ity_I8, addr)));

   return True;
}

static Bool gen_ldx_hu ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldx.hu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x1)));
   putIReg(rd, extendU(Ity_I16, load(Ity_I16, addr)));

   return True;
}

static Bool gen_ldx_wu ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldx.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   putIReg(rd, extendU(Ity_I32, load(Ity_I32, addr)));

   return True;
}

static Bool gen_preld ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt rj   = get_rj(insn);
   UInt hint = get_hint5(insn);

   DIP("preld %u, %s, %d\n", hint, nameIReg(rj), (Int)extend32(si12, 12));

   return True;
}

static Bool gen_preldx ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt rj   = get_rj(insn);
   UInt hint = get_hint5(insn);

   DIP("preldx %u, %s, %d\n", hint, nameIReg(rj), (Int)extend32(si12, 12));

   return True;
}

static Bool gen_dbar ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt hint = get_hint15(insn);

   DIP("dbar %u\n", hint);

   stmt(IRStmt_MBE(Imbe_Fence));

   return True;
}

static Bool gen_ibar ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt hint = get_hint15(insn);

   DIP("ibar %u\n", hint);

   stmt(IRStmt_MBE(Imbe_InsnFence));

   return True;
}

static Bool gen_ldptr_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ldptr.w %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                               (Int)extend32(si14, 14));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj),
                        mkU64(extend64(si14 << 2, 16)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   putIReg(rd, extendS(Ity_I32, load(Ity_I32, addr)));

   return True;
}

static Bool gen_stptr_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("stptr.w %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                               (Int)extend32(si14, 14));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj),
                        mkU64(extend64(si14 << 2, 16)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   store(addr, getIReg32(rd));

   return True;
}

static Bool gen_ldptr_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ldptr.d %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                               (Int)extend32(si14, 14));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj),
                        mkU64(extend64(si14 << 2, 16)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   putIReg(rd, load(Ity_I64, addr));

   return True;
}

static Bool gen_stptr_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("stptr.d %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                               (Int)extend32(si14, 14));

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj),
                        mkU64(extend64(si14 << 2, 16)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   store(addr, getIReg64(rd));

   return True;
}

static Bool gen_ldgt_b ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldgt.b %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   putIReg(rd, extendS(Ity_I8, load(Ity_I8, mkexpr(addr))));

   return True;
}

static Bool gen_ldgt_h ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldgt.h %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x1)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   putIReg(rd, extendS(Ity_I16, load(Ity_I16, mkexpr(addr))));

   return True;
}

static Bool gen_ldgt_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldgt.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   putIReg(rd, extendS(Ity_I32, load(Ity_I32, mkexpr(addr))));

   return True;
}

static Bool gen_ldgt_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldgt.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   putIReg(rd, load(Ity_I64, mkexpr(addr)));

   return True;
}

static Bool gen_ldle_b ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldle.b %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   putIReg(rd, extendS(Ity_I8, load(Ity_I8, mkexpr(addr))));

   return True;
}

static Bool gen_ldle_h ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldle.h %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x1)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   putIReg(rd, extendS(Ity_I16, load(Ity_I16, mkexpr(addr))));

   return True;
}

static Bool gen_ldle_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldle.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   putIReg(rd, extendS(Ity_I32, load(Ity_I32, mkexpr(addr))));

   return True;
}

static Bool gen_ldle_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ldle.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   putIReg(rd, load(Ity_I64, mkexpr(addr)));

   return True;
}

static Bool gen_stgt_b ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stgt.b %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   store(mkexpr(addr), getIReg8(rd));

   return True;
}

static Bool gen_stgt_h ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stgt.h %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x1)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   store(mkexpr(addr), getIReg16(rd));

   return True;
}

static Bool gen_stgt_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stgt.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   store(mkexpr(addr), getIReg32(rd));

   return True;
}

static Bool gen_stgt_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stgt.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   store(mkexpr(addr), getIReg64(rd));

   return True;
}

static Bool gen_stle_b ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stle.b %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   store(mkexpr(addr), getIReg8(rd));

   return True;
}

static Bool gen_stle_h ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stle.h %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x1)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   store(mkexpr(addr), getIReg16(rd));

   return True;
}

static Bool gen_stle_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stle.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   store(mkexpr(addr), getIReg32(rd));

   return True;
}

static Bool gen_stle_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("stle.d %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   store(mkexpr(addr), getIReg64(rd));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for fixed point atomic insns                 ---*/
/*------------------------------------------------------------*/

static Bool gen_ll_helper ( UInt rd, UInt rj, UInt si14, Bool size64 )
{
   Int offs_size = offsetof(VexGuestLOONGARCH64State, guest_LLSC_SIZE);
   Int offs_addr = offsetof(VexGuestLOONGARCH64State, guest_LLSC_ADDR);
   Int offs_data = offsetof(VexGuestLOONGARCH64State, guest_LLSC_DATA);

   /* Get address of the load. */
   IRTemp addr = newTemp(Ity_I64);
   assign(addr, binop(Iop_Add64, getIReg64(rj),
                      mkU64(extend64(si14 << 2, 16))));
   if (size64)
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   else
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));

   /* Load the value. */
   IRTemp res = newTemp(Ity_I64);
   if (size64)
      assign(res, load(Ity_I64, mkexpr(addr)));
   else
      assign(res, extendS(Ity_I32, load(Ity_I32, mkexpr(addr))));

   /* Set up the LLSC fallback data. */
   if (size64)
      stmt(IRStmt_Put(offs_size, mkU64(8)));
   else
      stmt(IRStmt_Put(offs_size, mkU64(4)));
   stmt(IRStmt_Put(offs_addr, mkexpr(addr)));
   stmt(IRStmt_Put(offs_data, mkexpr(res)));

   /* Write the result to the destination register. */
   putIReg(rd, mkexpr(res));

   return True;
}

static Bool gen_sc_helper ( UInt rd, UInt rj, UInt si14, Bool size64 )
{
   Int offs_size = offsetof(VexGuestLOONGARCH64State, guest_LLSC_SIZE);
   Int offs_addr = offsetof(VexGuestLOONGARCH64State, guest_LLSC_ADDR);
   Int offs_data = offsetof(VexGuestLOONGARCH64State, guest_LLSC_DATA);

   /* Get address of the load. */
   IRTemp addr = newTemp(Ity_I64);
   assign(addr, binop(Iop_Add64, getIReg64(rj),
                      mkU64(extend64(si14 << 2, 16))));
   if (size64)
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   else
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));

   /* Get new value. */
   IRTemp new;
   if (size64) {
      new = newTemp(Ity_I64);
      assign(new, getIReg64(rd));
   } else {
      new = newTemp(Ity_I32);
      assign(new, getIReg32(rd));
   }

   /* Mark the SC initially as failed. */
   putIReg(rd, mkU64(0));

   /* Set that no transaction is in progress. */
   IRTemp size = newTemp(Ity_I64);
   assign(size, IRExpr_Get(offs_size, Ity_I64));
   stmt(IRStmt_Put(offs_size, mkU64(0) /* "no transaction" */));

   /* Fail if no or wrong-size transaction. */
   if (size64)
      exit(binop(Iop_CmpNE64, mkexpr(size), mkU64(8)), Ijk_Boring, 4);
   else
      exit(binop(Iop_CmpNE64, mkexpr(size), mkU64(4)), Ijk_Boring, 4);

   /* Fail if the address doesn't match the LL address. */
   exit(binop(Iop_CmpNE64, mkexpr(addr), IRExpr_Get(offs_addr, Ity_I64)),
        Ijk_Boring, 4);

   /* Fail if the data doesn't match the LL data. */
   IRTemp data;
   if (size64) {
      data = newTemp(Ity_I64);
      assign(data, IRExpr_Get(offs_data, Ity_I64));
      IRExpr* d = load(Ity_I64, mkexpr(addr));
      exit(binop(Iop_CmpNE64, d, mkexpr(data)), Ijk_Boring, 4);
   } else {
      data = newTemp(Ity_I32);
      IRTemp tmp = newTemp(Ity_I64);
      assign(tmp, IRExpr_Get(offs_data, Ity_I64));
      assign(data, unop(Iop_64to32, mkexpr(tmp)));
      IRExpr* d = extendS(Ity_I32, load(Ity_I32, mkexpr(addr)));
      exit(binop(Iop_CmpNE64, d, mkexpr(tmp)), Ijk_Boring, 4);
   }

   /* Try to CAS the new value in. */
   IRTemp old;
   if (size64) {
      old = newTemp(Ity_I64);
      cas(old, mkexpr(addr), mkexpr(data), mkexpr(new));
   } else {
      old = newTemp(Ity_I32);
      cas(old, mkexpr(addr), mkexpr(data), mkexpr(new));
   }

   /* Fail if the CAS failed (old != expd). */
   if (size64)
      exit(binop(Iop_CasCmpNE64, mkexpr(old), mkexpr(data)), Ijk_Boring, 4);
   else
      exit(binop(Iop_CasCmpNE32, mkexpr(old), mkexpr(data)), Ijk_Boring, 4);

   /* Otherwise mark the operation as successful. */
   putIReg(rd, mkU64(1));

   return True;
}

static Bool gen_ll_w ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ll.w %s, %s, %d%s\n", nameIReg(rd), nameIReg(rj),
                              (Int)extend32(si14, 14),
                              abiinfo->guest__use_fallback_LLSC ?
                              " (fallback implementation)" : "");

   if (abiinfo->guest__use_fallback_LLSC) {
      return gen_ll_helper(rd, rj, si14, False);
   } else {
      IRTemp  res = newTemp(Ity_I32);
      IRTemp addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, getIReg64(rj),
                         mkU64(extend64(si14 << 2, 16))));
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
      stmt(IRStmt_LLSC(Iend_LE, res, mkexpr(addr), NULL/*LL*/));
      putIReg(rd, extendS(Ity_I32, mkexpr(res)));
      return True;
   }
}

static Bool gen_sc_w ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("sc.w %s, %s, %d%s\n", nameIReg(rd), nameIReg(rj),
                              (Int)extend32(si14, 14),
                              abiinfo->guest__use_fallback_LLSC ?
                              " (fallback implementation)" : "");

   if (abiinfo->guest__use_fallback_LLSC) {
      return gen_sc_helper(rd, rj, si14, False);
   } else {
      IRTemp  res = newTemp(Ity_I1);
      IRTemp addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, getIReg64(rj),
                         mkU64(extend64(si14 << 2, 16))));
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
      stmt(IRStmt_LLSC(Iend_LE, res, mkexpr(addr), getIReg32(rd)));
      return True;
   }
}

static Bool gen_ll_d ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("ll.d %s, %s, %d%s\n", nameIReg(rd), nameIReg(rj),
                              (Int)extend32(si14, 14),
                              abiinfo->guest__use_fallback_LLSC ?
                              " (fallback implementation)" : "");

   if (abiinfo->guest__use_fallback_LLSC) {
      return gen_ll_helper(rd, rj, si14, True);
   } else {
      IRTemp  res = newTemp(Ity_I64);
      IRTemp addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, getIReg64(rj),
                         mkU64(extend64(si14 << 2, 16))));
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
      stmt(IRStmt_LLSC(Iend_LE, res, mkexpr(addr), NULL/*LL*/));
      putIReg(rd, mkexpr(res));
      return True;
   }
}

static Bool gen_sc_d ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt si14 = get_si14(insn);
   UInt   rj = get_rj(insn);
   UInt   rd = get_rd(insn);

   DIP("sc.d %s, %s, %d%s\n", nameIReg(rd), nameIReg(rj),
                              (Int)extend32(si14, 14),
                              abiinfo->guest__use_fallback_LLSC ?
                              " (fallback implementation)" : "");

   if (abiinfo->guest__use_fallback_LLSC) {
      return gen_sc_helper(rd, rj, si14, True);
   } else {
      IRTemp  res = newTemp(Ity_I1);
      IRTemp addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, getIReg64(rj),
                         mkU64(extend64(si14 << 2, 16))));
      gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
      stmt(IRStmt_LLSC(Iend_LE, res, mkexpr(addr), getIReg64(rd)));
      return True;
   }
}

enum amop {
   AMSWAP, AMADD, AMAND, AMOR, AMXOR, AMMAX, AMMIN, AMMAX_U, AMMIN_U
};

static Bool gen_am_w_helper ( enum amop op, Bool fence,
                              UInt rd, UInt rj, UInt rk )
{
   if (fence)
      stmt(IRStmt_MBE(Imbe_Fence));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));

   IRTemp o = newTemp(Ity_I32);
   assign(o, load(Ity_I32, mkexpr(addr)));
   IRTemp n = newTemp(Ity_I32);
   assign(n, getIReg32(rk));
   IRExpr* e;
   switch (op) {
      case AMSWAP:
         e = mkexpr(n);
         break;
      case AMADD:
         e = binop(Iop_Add32, mkexpr(o), mkexpr(n));
         break;
      case AMAND:
         e = binop(Iop_And32, mkexpr(o), mkexpr(n));
         break;
      case AMOR:
         e = binop(Iop_Or32, mkexpr(o), mkexpr(n));
         break;
      case AMXOR:
         e = binop(Iop_Xor32, mkexpr(o), mkexpr(n));
         break;
      case AMMAX: {
         IRExpr* cond = binop(Iop_CmpLT32S, mkexpr(n), mkexpr(o));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      case AMMIN: {
         IRExpr* cond = binop(Iop_CmpLT32S, mkexpr(o), mkexpr(n));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      case AMMAX_U: {
         IRExpr* cond = binop(Iop_CmpLT32U, mkexpr(n), mkexpr(o));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      case AMMIN_U: {
         IRExpr* cond = binop(Iop_CmpLT32U, mkexpr(o), mkexpr(n));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      default:
         return False;
   }

   IRTemp old = newTemp(Ity_I32);
   cas(old, mkexpr(addr), mkexpr(o), e);
   IRExpr* cond = binop(Iop_CasCmpNE32, mkexpr(old), mkexpr(o));
   exit(cond, Ijk_Boring, 0); /* Loop if failed */
   putIReg(rd, extendS(Ity_I32, mkexpr(o)));

   if (fence)
      stmt(IRStmt_MBE(Imbe_Fence));

   return True;
}

static Bool gen_am_d_helper ( enum amop op, Bool fence,
                              UInt rd, UInt rj, UInt rk )
{
   if (fence)
      stmt(IRStmt_MBE(Imbe_Fence));

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));

   IRTemp o = newTemp(Ity_I64);
   assign(o, load(Ity_I64, mkexpr(addr)));
   IRTemp n = newTemp(Ity_I64);
   assign(n, getIReg64(rk));
   IRExpr* e;
   switch (op) {
      case AMSWAP:
         e = mkexpr(n);
         break;
      case AMADD:
         e = binop(Iop_Add64, mkexpr(o), mkexpr(n));
         break;
      case AMAND:
         e = binop(Iop_And64, mkexpr(o), mkexpr(n));
         break;
      case AMOR:
         e = binop(Iop_Or64, mkexpr(o), mkexpr(n));
         break;
      case AMXOR:
         e = binop(Iop_Xor64, mkexpr(o), mkexpr(n));
         break;
      case AMMAX: {
         IRExpr* cond = binop(Iop_CmpLT64S, mkexpr(n), mkexpr(o));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      case AMMIN: {
         IRExpr* cond = binop(Iop_CmpLT64S, mkexpr(o), mkexpr(n));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      case AMMAX_U: {
         IRExpr* cond = binop(Iop_CmpLT64U, mkexpr(n), mkexpr(o));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      case AMMIN_U: {
         IRExpr* cond = binop(Iop_CmpLT64U, mkexpr(o), mkexpr(n));
         e = IRExpr_ITE(cond, mkexpr(o), mkexpr(n));
         break;
      }
      default:
         return False;
   }

   IRTemp old = newTemp(Ity_I64);
   cas(old, mkexpr(addr), mkexpr(o), e);
   IRExpr* cond = binop(Iop_CasCmpNE64, mkexpr(old), mkexpr(o));
   exit(cond, Ijk_Boring, 0); /* Loop if failed */
   putIReg(rd, mkexpr(o));

   if (fence)
      stmt(IRStmt_MBE(Imbe_Fence));

   return True;
}

static Bool gen_amswap_w ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amswap.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMSWAP, False, rd, rj, rk);
}

static Bool gen_amswap_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amswap.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMSWAP, False, rd, rj, rk);
}

static Bool gen_amadd_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amadd.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMADD, False, rd, rj, rk);
}

static Bool gen_amadd_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amadd.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMADD, False, rd, rj, rk);
}

static Bool gen_amand_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amand.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMAND, False, rd, rj, rk);
}

static Bool gen_amand_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amand.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMAND, False, rd, rj, rk);
}

static Bool gen_amor_w ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amor.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMOR, False, rd, rj, rk);
}

static Bool gen_amor_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amor.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMOR, False, rd, rj, rk);
}

static Bool gen_amxor_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amxor.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMXOR, False, rd, rj, rk);
}

static Bool gen_amxor_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amxor.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMXOR, False, rd, rj, rk);
}

static Bool gen_ammax_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMAX, False, rd, rj, rk);
}

static Bool gen_ammax_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMAX, False, rd, rj, rk);
}

static Bool gen_ammin_w ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMIN, False, rd, rj, rk);
}

static Bool gen_ammin_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMIN, False, rd, rj, rk);
}

static Bool gen_ammax_wu ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMAX_U, False, rd, rj, rk);
}

static Bool gen_ammax_du ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax.du %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMAX_U, False, rd, rj, rk);
}

static Bool gen_ammin_wu ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMIN_U, False, rd, rj, rk);
}

static Bool gen_ammin_du ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin.du %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMIN_U, False, rd, rj, rk);
}

static Bool gen_amswap_db_w ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amswap_db.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMSWAP, True, rd, rj, rk);
}

static Bool gen_amswap_db_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amswap_db.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMSWAP, True, rd, rj, rk);
}

static Bool gen_amadd_db_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amadd_db.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMADD, True, rd, rj, rk);
}

static Bool gen_amadd_db_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amadd_db.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMADD, True, rd, rj, rk);
}

static Bool gen_amand_db_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amand_db.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMAND, True, rd, rj, rk);
}

static Bool gen_amand_db_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amand_db.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMAND, True, rd, rj, rk);
}

static Bool gen_amor_db_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amor_db.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMOR, True, rd, rj, rk);
}

static Bool gen_amor_db_d ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amor_db.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMOR, True, rd, rj, rk);
}

static Bool gen_amxor_db_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amxor_db.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMXOR, True, rd, rj, rk);
}

static Bool gen_amxor_db_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("amxor_db.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMXOR, True, rd, rj, rk);
}

static Bool gen_ammax_db_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax_db.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMAX, True, rd, rj, rk);
}

static Bool gen_ammax_db_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax_db.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMAX, True, rd, rj, rk);
}

static Bool gen_ammin_db_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin_db.w %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMIN, True, rd, rj, rk);
}

static Bool gen_ammin_db_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin_db.d %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMIN, True, rd, rj, rk);
}

static Bool gen_ammax_db_wu ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax_db.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMAX_U, True, rd, rj, rk);
}

static Bool gen_ammax_db_du ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammax_db.du %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMAX_U, True, rd, rj, rk);
}

static Bool gen_ammin_db_wu ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin_db.wu %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_w_helper(AMMIN_U, True, rd, rj, rk);
}

static Bool gen_ammin_db_du ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("ammin_db.du %s, %s, %s\n", nameIReg(rd), nameIReg(rk), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LAM)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_am_d_helper(AMMIN_U, True, rd, rj, rk);
}


/*------------------------------------------------------------*/
/*--- Helpers for fixed point extra insns                  ---*/
/*------------------------------------------------------------*/

static Bool gen_crc_w_b_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crc.w.b.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(8));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crc",
                                &loongarch64_calculate_crc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_crc_w_h_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crc.w.h.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(16));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crc",
                                &loongarch64_calculate_crc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_crc_w_w_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crc.w.w.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(32));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crc",
                                &loongarch64_calculate_crc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_crc_w_d_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crc.w.d.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(64));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crc",
                                &loongarch64_calculate_crc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_crcc_w_b_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crcc.w.b.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(8));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crcc",
                                &loongarch64_calculate_crcc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_crcc_w_h_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crcc.w.h.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(16));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crcc",
                                &loongarch64_calculate_crcc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_crcc_w_w_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crcc.w.w.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(32));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crcc",
                                &loongarch64_calculate_crcc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_crcc_w_d_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("crcc.w.d.w %s, %s, %s\n", nameIReg(rd), nameIReg(rj), nameIReg(rk));

   IRExpr** arg = mkIRExprVec_3(getIReg64(rk), getIReg64(rj), mkU64(64));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_crcc",
                                &loongarch64_calculate_crcc,
                                arg);
   putIReg(rd, call);

   return True;
}

static Bool gen_break ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt code = get_code(insn);

   DIP("break %u\n", code);

   putPC(mkU64(guest_PC_curr_instr + 4));

   /* On LoongArch, most instructions do not raise exceptions;
      instead, gcc notifies the kernel with a trap instruction.
      We simulate the behavior of the linux kernel here.
      See arch/loongarch/kernel/traps.c.
    */
   switch (code) {
      case 6: /* BRK_OVERFLOW */
         dres->jk_StopHere = Ijk_SigFPE_IntOvf;
         break;
      case 7: /* BRK_DIVZERO */
         dres->jk_StopHere = Ijk_SigFPE_IntDiv;
         break;
      default:
         dres->jk_StopHere = Ijk_SigTRAP;
         break;
   }
   dres->whatNext    = Dis_StopHere;

   return True;
}

static Bool gen_syscall ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt hint = get_hint15(insn);

   DIP("syscall %u\n", hint);

   putPC(mkU64(guest_PC_curr_instr + 4));

   dres->jk_StopHere = Ijk_Sys_syscall;
   dres->whatNext    = Dis_StopHere;

   return True;
}

static Bool gen_asrtle_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);

   DIP("asrtle.d %s, %s\n", nameIReg(rj), nameIReg(rk));

   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), getIReg64(rj)));

   return True;
}

static Bool gen_asrtgt_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);

   DIP("asrtgt.d %s, %s\n", nameIReg(rj), nameIReg(rk));

   gen_SIGSYS(binop(Iop_CmpLE64U, getIReg64(rj), getIReg64(rk)));

   return True;
}

static Bool gen_rdtimel_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("rdtimel.w %s, %s\n", nameIReg(rd), nameIReg(rj));

   putIReg(rd, mkU64(0));

   return True;
}

static Bool gen_rdtimeh_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("rdtimeh.w %s, %s\n", nameIReg(rd), nameIReg(rj));

   putIReg(rd, mkU64(0));

   return True;
}

static Bool gen_rdtime_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("rdtime.d %s, %s\n", nameIReg(rd), nameIReg(rj));

   putIReg(rd, mkU64(0));

   return True;
}

static Bool gen_cpucfg ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt rd = get_rd(insn);

   DIP("cpucfg %s, %s\n", nameIReg(rd), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_CPUCFG)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr** arg = mkIRExprVec_1(getIReg64(rj));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_cpucfg",
                                &loongarch64_calculate_cpucfg,
                                arg);
   putIReg(rd, call);

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for floating point arithmetic insns          ---*/
/*------------------------------------------------------------*/

static Bool gen_fadd_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fadd.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FADD_S, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, triop(Iop_AddF32, rm, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fadd_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fadd.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FADD_D, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, triop(Iop_AddF64, rm, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fsub_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fsub.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FSUB_S, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, triop(Iop_SubF32, rm, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fsub_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fsub.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FSUB_D, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, triop(Iop_SubF64, rm, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fmul_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmul.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMUL_S, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, triop(Iop_MulF32, rm, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fmul_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmul.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMUL_D, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, triop(Iop_MulF64, rm, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fdiv_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fdiv.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FDIV_S, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, triop(Iop_DivF32, rm, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fdiv_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fdiv.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FDIV_D, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, triop(Iop_DivF64, rm, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fmadd_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmadd.s %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                   nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMADD_S, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, qop(Iop_MAddF32, rm, getFReg32(fj),
                     getFReg32(fk), getFReg32(fa)));

   return True;
}

static Bool gen_fmadd_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmadd.d %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                   nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMADD_D, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, qop(Iop_MAddF64, rm, getFReg64(fj),
                     getFReg64(fk), getFReg64(fa)));

   return True;
}

static Bool gen_fmsub_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmsub.s %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                   nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMSUB_S, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, qop(Iop_MSubF32, rm, getFReg32(fj),
                     getFReg32(fk), getFReg32(fa)));

   return True;
}

static Bool gen_fmsub_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmsub.d %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                   nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMSUB_D, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, qop(Iop_MSubF64, rm, getFReg64(fj),
                     getFReg64(fk), getFReg64(fa)));

   return True;
}

static Bool gen_fnmadd_s ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fnmadd.s %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                    nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FNMADD_S, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   IRExpr* madd = qop(Iop_MAddF32, rm, getFReg32(fj),
                      getFReg32(fk), getFReg32(fa));
   putFReg32(fd, unop(Iop_NegF32, madd));

   return True;
}

static Bool gen_fnmadd_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fnmadd.d %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                    nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FNMADD_D, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   IRExpr* madd = qop(Iop_MAddF64, rm, getFReg64(fj),
                      getFReg64(fk), getFReg64(fa));
   putFReg64(fd, unop(Iop_NegF64, madd));

   return True;
}

static Bool gen_fnmsub_s ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fnmsub.s %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                    nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FNMSUB_S, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   IRExpr* msub = qop(Iop_MSubF32, rm, getFReg32(fj),
                      getFReg32(fk), getFReg32(fa));
   putFReg32(fd, unop(Iop_NegF32, msub));

   return True;
}

static Bool gen_fnmsub_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fa = get_fa(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fnmsub.d %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                    nameFReg(fk), nameFReg(fa));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FNMSUB_D, 3, fj, fk, fa);
   IRExpr* rm = get_rounding_mode();
   IRExpr* msub = qop(Iop_MSubF64, rm, getFReg64(fj),
                      getFReg64(fk), getFReg64(fa));
   putFReg64(fd, unop(Iop_NegF64, msub));

   return True;
}

static Bool gen_fmax_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmax.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMAX_S, 2, fj, fk, 0);
   putFReg32(fd, binop(Iop_MaxNumF32, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fmax_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmax.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMAX_D, 2, fj, fk, 0);
   putFReg64(fd, binop(Iop_MaxNumF64, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fmin_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmin.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMIN_S, 2, fj, fk, 0);
   putFReg32(fd, binop(Iop_MinNumF32, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fmin_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmin.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMIN_D, 2, fj, fk, 0);
   putFReg64(fd, binop(Iop_MinNumF64, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fmaxa_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmaxa.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMAXA_S, 2, fj, fk, 0);
   putFReg32(fd, binop(Iop_MaxNumAbsF32, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fmaxa_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmaxa.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMAXA_D, 2, fj, fk, 0);
   putFReg64(fd, binop(Iop_MaxNumAbsF64, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fmina_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmina.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMINA_S, 2, fj, fk, 0);
   putFReg32(fd, binop(Iop_MinNumAbsF32, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fmina_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmina.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FMINA_D, 2, fj, fk, 0);
   putFReg64(fd, binop(Iop_MinNumAbsF64, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_fabs_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fabs.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FABS_S, 1, fj, 0, 0);
   putFReg32(fd, unop(Iop_AbsF32, getFReg32(fj)));

   return True;
}

static Bool gen_fabs_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fabs.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FABS_D, 1, fj, 0, 0);
   putFReg64(fd, unop(Iop_AbsF64, getFReg64(fj)));

   return True;
}

static Bool gen_fneg_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fneg.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FNEG_S, 1, fj, 0, 0);
   putFReg32(fd, unop(Iop_NegF32, getFReg32(fj)));

   return True;
}

static Bool gen_fneg_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fneg.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FNEG_D, 1, fj, 0, 0);
   putFReg64(fd, unop(Iop_NegF64, getFReg64(fj)));

   return True;
}

static Bool gen_fsqrt_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fsqrt.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FSQRT_S, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, binop(Iop_SqrtF32, rm, getFReg32(fj)));

   return True;
}

static Bool gen_fsqrt_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fsqrt.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FSQRT_D, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, binop(Iop_SqrtF64, rm, getFReg64(fj)));

   return True;
}

static Bool gen_frecip_s ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("frecip.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FRECIP_S, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, triop(Iop_DivF32, rm, mkF32i(1), getFReg32(fj)));

   return True;
}

static Bool gen_frecip_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("frecip.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FRECIP_D, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, triop(Iop_DivF64, rm, mkF64i(1), getFReg64(fj)));

   return True;
}

static Bool gen_frsqrt_s ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("frsqrt.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FRSQRT_S, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, binop(Iop_RSqrtF32, rm, getFReg32(fj)));

   return True;
}

static Bool gen_frsqrt_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("frsqrt.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FRSQRT_D, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, binop(Iop_RSqrtF64, rm, getFReg64(fj)));

   return True;
}

static Bool gen_fscaleb_s ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fscaleb.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FSCALEB_S, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, triop(Iop_ScaleBF32, rm, getFReg32(fj), getFReg32(fk)));

   return True;
}

static Bool gen_fscaleb_d ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fscaleb.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FSCALEB_D, 2, fj, fk, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, triop(Iop_ScaleBF64, rm, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_flogb_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("flogb.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FLOGB_S, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, binop(Iop_LogBF32, rm, getFReg32(fj)));

   return True;
}

static Bool gen_flogb_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("flogb.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FLOGB_D, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, binop(Iop_LogBF64, rm, getFReg64(fj)));

   return True;
}

static Bool gen_fcopysign_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fcopysign.s %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* i1 = unop(Iop_ReinterpF32asI32, getFReg32(fj));
   IRExpr* shl1 = binop(Iop_Shl32, i1, mkU8(1));
   IRExpr* shr1 = binop(Iop_Shr32, shl1, mkU8(1));
   IRExpr* i2 = unop(Iop_ReinterpF32asI32, getFReg32(fk));
   IRExpr* shr2 = binop(Iop_Shr32, i2, mkU8(31));
   IRExpr* shl2 = binop(Iop_Shl32, shr2, mkU8(31));
   IRExpr* or = binop(Iop_Or32, shr1, shl2);
   putFReg32(fd, unop(Iop_ReinterpI32asF32, or));

   return True;
}

static Bool gen_fcopysign_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fcopysign.d %s, %s, %s\n", nameFReg(fd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* i1 = unop(Iop_ReinterpF64asI64, getFReg64(fj));
   IRExpr* shl1 = binop(Iop_Shl64, i1, mkU8(1));
   IRExpr* shr1 = binop(Iop_Shr64, shl1, mkU8(1));
   IRExpr* i2 = unop(Iop_ReinterpF64asI64, getFReg64(fk));
   IRExpr* shr2 = binop(Iop_Shr64, i2, mkU8(63));
   IRExpr* shl2 = binop(Iop_Shl64, shr2, mkU8(63));
   IRExpr* or = binop(Iop_Or64, shr1, shl2);
   putFReg64(fd, unop(Iop_ReinterpI64asF64, or));

   return True;
}

static Bool gen_fclass_s ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fclass.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr** arg = mkIRExprVec_1(unop(Iop_ReinterpF64asI64, getFReg64(fj)));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_fclass_s",
                                &loongarch64_calculate_fclass_s,
                                arg);
   putFReg32(fd, unop(Iop_ReinterpI32asF32, unop(Iop_64to32, call)));

   return True;
}

static Bool gen_fclass_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fclass.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr** arg = mkIRExprVec_1(unop(Iop_ReinterpF64asI64, getFReg64(fj)));
   IRExpr* call = mkIRExprCCall(Ity_I64, 0/*regparms*/,
                                "loongarch64_calculate_fclass_d",
                                &loongarch64_calculate_fclass_d,
                                arg);
   putFReg64(fd, unop(Iop_ReinterpI64asF64, call));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for floating point comparison insns          ---*/
/*------------------------------------------------------------*/

static inline IRExpr* is_UN ( IRExpr* e )
{
   return binop(Iop_CmpEQ32, e, mkU32(0x45));
}

static inline IRExpr* is_LT ( IRExpr* e )
{
   return binop(Iop_CmpEQ32, e, mkU32(0x1));
}

static inline IRExpr* is_GT ( IRExpr* e )
{
   return binop(Iop_CmpEQ32, e, mkU32(0x0));
}

static inline IRExpr* is_EQ ( IRExpr* e )
{
   return binop(Iop_CmpEQ32, e, mkU32(0x40));
}

static Bool gen_fcmp_cond_helper ( enum fpop op, UInt cc,
                                   UInt fj, UInt fk, Bool size64 )
{
   /* We have to convert 'irRes' from an IR-convention return result
      (IRCmpF32Result / IRCmpF64Result) to a LOONGARCH-encoded group.

      FP cmp result | IR
      --------------------
      UN            | 0x45
      LT            | 0x01
      GT            | 0x00
      EQ            | 0x40
    */
   IRTemp result = newTemp(Ity_I32);
   if (size64)
      assign(result, binop(Iop_CmpF64, getFReg64(fj), getFReg64(fk)));
   else
      assign(result, binop(Iop_CmpF32, getFReg32(fj), getFReg32(fk)));

   IRExpr* e;
   switch (op) {
      case FCMP_CAF_S: case FCMP_CAF_D: case FCMP_SAF_S: case FCMP_SAF_D:
         e = mkU1(0);
         break;
      case FCMP_CLT_S: case FCMP_CLT_D: case FCMP_SLT_S: case FCMP_SLT_D:
         e = is_LT(mkexpr(result));
         break;
      case FCMP_CEQ_S: case FCMP_CEQ_D: case FCMP_SEQ_S: case FCMP_SEQ_D:
         e = is_EQ(mkexpr(result));
         break;
      case FCMP_CLE_S: case FCMP_CLE_D: case FCMP_SLE_S: case FCMP_SLE_D:
         e = binop(Iop_Or1, is_LT(mkexpr(result)), is_EQ(mkexpr(result)));
         break;
      case FCMP_CUN_S: case FCMP_CUN_D: case FCMP_SUN_S: case FCMP_SUN_D:
         e = is_UN(mkexpr(result));
         break;
      case FCMP_CULT_S: case FCMP_CULT_D: case FCMP_SULT_S: case FCMP_SULT_D:
         e = binop(Iop_Or1, is_UN(mkexpr(result)), is_LT(mkexpr(result)));
         break;
      case FCMP_CUEQ_S: case FCMP_CUEQ_D: case FCMP_SUEQ_S: case FCMP_SUEQ_D:
         e = binop(Iop_Or1, is_UN(mkexpr(result)), is_EQ(mkexpr(result)));
         break;
      case FCMP_CULE_S: case FCMP_CULE_D: case FCMP_SULE_S: case FCMP_SULE_D:
         e = binop(Iop_Or1, is_UN(mkexpr(result)),
                            binop(Iop_Or1, is_LT(mkexpr(result)),
                                           is_EQ(mkexpr(result))));
         break;
      case FCMP_CNE_S: case FCMP_CNE_D: case FCMP_SNE_S: case FCMP_SNE_D:
         e = binop(Iop_Or1, is_GT(mkexpr(result)), is_LT(mkexpr(result)));
         break;
      case FCMP_COR_S: case FCMP_COR_D: case FCMP_SOR_S: case FCMP_SOR_D:
         e = binop(Iop_Or1, is_GT(mkexpr(result)),
                            binop(Iop_Or1, is_LT(mkexpr(result)),
                                           is_EQ(mkexpr(result))));
         break;
      case FCMP_CUNE_S: case FCMP_CUNE_D: case FCMP_SUNE_S: case FCMP_SUNE_D:
         e = binop(Iop_Or1, is_UN(mkexpr(result)),
                            binop(Iop_Or1, is_GT(mkexpr(result)),
                                           is_LT(mkexpr(result))));
         break;
      default:
         return False;
   }

   calculateFCSR(op, 2, fj, fk, 0);
   putFCC(cc, unop(Iop_1Uto8, e));

   return True;
}

static Bool gen_fcmp_caf_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.caf.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CAF_S, cd, fj, fk, False);
}

static Bool gen_fcmp_caf_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.caf.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CAF_D, cd, fj, fk, True);
}

static Bool gen_fcmp_saf_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.saf.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SAF_S, cd, fj, fk, False);
}

static Bool gen_fcmp_saf_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.saf.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SAF_D, cd, fj, fk, True);
}

static Bool gen_fcmp_clt_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.clt.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CLT_S, cd, fj, fk, False);
}

static Bool gen_fcmp_clt_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.clt.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CLT_D, cd, fj, fk, True);
}

static Bool gen_fcmp_slt_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.slt.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SLT_S, cd, fj, fk, False);
}

static Bool gen_fcmp_slt_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.slt.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SLT_D, cd, fj, fk, True);
}

static Bool gen_fcmp_ceq_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.ceq.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CEQ_S, cd, fj, fk, False);
}

static Bool gen_fcmp_ceq_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.ceq.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CEQ_D, cd, fj, fk, True);
}

static Bool gen_fcmp_seq_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.seq.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SEQ_S, cd, fj, fk, False);
}

static Bool gen_fcmp_seq_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.seq.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SEQ_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cle_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cle.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CLE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cle_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cle.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CLE_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sle_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sle.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SLE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sle_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sle.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SLE_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cun_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cun.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CUN_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cun_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cun.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CUN_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sun_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sun.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SUN_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sun_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sun.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SUN_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cult_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cult.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CULT_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cult_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cult.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CULT_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sult_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sult.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SULT_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sult_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sult.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SULT_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cueq_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cueq.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CUEQ_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cueq_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cueq.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CUEQ_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sueq_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sueq.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SUEQ_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sueq_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sueq.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SUEQ_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cule_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cule.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CULE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cule_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cule.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CULE_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sule_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sule.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SULE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sule_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sule.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SULE_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cne_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cne.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CNE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cne_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cne.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CNE_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sne_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sne.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SNE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sne_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sne.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SNE_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cor_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cor.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_COR_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cor_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cor.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_COR_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sor_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sor.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SOR_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sor_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sor.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SOR_D, cd, fj, fk, True);
}

static Bool gen_fcmp_cune_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cune.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CUNE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_cune_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.cune.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_CUNE_D, cd, fj, fk, True);
}

static Bool gen_fcmp_sune_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sune.s %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SUNE_S, cd, fj, fk, False);
}

static Bool gen_fcmp_sune_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("fcmp.sune.d %s, %s, %s\n", nameFCC(cd), nameFReg(fj), nameFReg(fk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_fcmp_cond_helper(FCMP_SUNE_D, cd, fj, fk, True);
}


/*------------------------------------------------------------*/
/*--- Helpers for floating point conversion insns          ---*/
/*------------------------------------------------------------*/

static IRExpr* is_Invalid_Overflow ( void )
{
   /* Bits 16 to 20 in FCSR are flags.
      Bit 18 - overflow
      Bit 20 - invalid
    */
   IRExpr* fcsr = getFCSR(0);
   IRExpr* shr = binop(Iop_Shr32, fcsr, mkU8(16));
   IRExpr* and = binop(Iop_And32, shr, mkU32(0x14));
   return binop(Iop_CmpNE32, and, getIReg32(0));
}

static Bool gen_convert_s_helper ( enum fpop op, UInt fd, UInt fj )
{
   IRExpr* e;
   IRExpr* rm;
   switch (op) {
      case FTINTRM_W_S:
         rm = gen_round_down();
         e = binop(Iop_F32toI32S, rm, getFReg32(fj));
         break;
      case FTINTRM_W_D:
         rm = gen_round_down();
         e = binop(Iop_F64toI32S, rm, getFReg64(fj));
         break;
      case FTINTRP_W_S:
         rm = gen_round_up();
         e = binop(Iop_F32toI32S, rm, getFReg32(fj));
         break;
      case FTINTRP_W_D:
         rm = gen_round_up();
         e = binop(Iop_F64toI32S, rm, getFReg64(fj));
         break;
      case FTINTRZ_W_S:
         rm = gen_round_to_zero();
         e = binop(Iop_F32toI32S, rm, getFReg32(fj));
         break;
      case FTINTRZ_W_D:
         rm = gen_round_to_zero();
         e = binop(Iop_F64toI32S, rm, getFReg64(fj));
         break;
      case FTINTRNE_W_S:
         rm = gen_round_to_nearest();
         e = binop(Iop_F32toI32S, rm, getFReg32(fj));
         break;
      case FTINTRNE_W_D:
         rm = gen_round_to_nearest();
         e = binop(Iop_F64toI32S, rm, getFReg64(fj));
         break;
      case FTINT_W_S:
         rm = get_rounding_mode();
         e = binop(Iop_F32toI32S, rm, getFReg32(fj));
         break;
      case FTINT_W_D:
         rm = get_rounding_mode();
         e = binop(Iop_F64toI32S, rm, getFReg64(fj));
         break;
      default:
         return False;
   }

   calculateFCSR(op, 1, fj, 0, 0);
   IRExpr* ite = IRExpr_ITE(is_Invalid_Overflow(), mkU32(0x7fffffff), e);
   putFReg32(fd, unop(Iop_ReinterpI32asF32, ite));

   return True;
}

static Bool gen_convert_d_helper ( enum fpop op, UInt fd, UInt fj )
{
   IRExpr* e;
   IRExpr* rm;
   switch (op) {
      case FTINTRM_L_S:
         rm = gen_round_down();
         e = binop(Iop_F32toI64S, rm, getFReg32(fj));
         break;
      case FTINTRM_L_D:
         rm = gen_round_down();
         e = binop(Iop_F64toI64S, rm, getFReg64(fj));
         break;
      case FTINTRP_L_S:
         rm = gen_round_up();
         e = binop(Iop_F32toI64S, rm, getFReg32(fj));
         break;
      case FTINTRP_L_D:
         rm = gen_round_up();
         e = binop(Iop_F64toI64S, rm, getFReg64(fj));
         break;
      case FTINTRZ_L_S:
         rm = gen_round_to_zero();
         e = binop(Iop_F32toI64S, rm, getFReg32(fj));
         break;
      case FTINTRZ_L_D:
         rm = gen_round_to_zero();
         e = binop(Iop_F64toI64S, rm, getFReg64(fj));
         break;
      case FTINTRNE_L_S:
         rm = gen_round_to_nearest();
         e = binop(Iop_F32toI64S, rm, getFReg32(fj));
         break;
      case FTINTRNE_L_D:
         rm = gen_round_to_nearest();
         e = binop(Iop_F64toI64S, rm, getFReg64(fj));
         break;
      case FTINT_L_S:
         rm = get_rounding_mode();
         e = binop(Iop_F32toI64S, rm, getFReg32(fj));
         break;
      case FTINT_L_D:
         rm = get_rounding_mode();
         e = binop(Iop_F64toI64S, rm, getFReg64(fj));
         break;
      default:
         return False;
   }

   calculateFCSR(op, 1, fj, 0, 0);
   IRExpr* ite = IRExpr_ITE(is_Invalid_Overflow(),
                            mkU64(0x7fffffffffffffffULL), e);
   putFReg64(fd, unop(Iop_ReinterpI64asF64, ite));

   return True;
}

static Bool gen_fcvt_s_d ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fcvt.s.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FCVT_S_D, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, binop(Iop_F64toF32, rm, getFReg64(fj)));

   return True;
}

static Bool gen_fcvt_d_s ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fcvt.d.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FCVT_D_S, 1, fj, 0, 0);
   putFReg64(fd, unop(Iop_F32toF64, getFReg32(fj)));

   return True;
}

static Bool gen_ftintrm_w_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrm.w.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRM_W_S, fd, fj);
}

static Bool gen_ftintrm_w_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrm.w.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRM_W_D, fd, fj);
}

static Bool gen_ftintrm_l_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrm.l.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRM_L_S, fd, fj);
}

static Bool gen_ftintrm_l_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrm.l.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRM_L_D, fd, fj);
}

static Bool gen_ftintrp_w_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrp.w.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRP_W_S, fd, fj);
}

static Bool gen_ftintrp_w_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrp.w.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRP_W_D, fd, fj);
}

static Bool gen_ftintrp_l_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrp.l.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRP_L_S, fd, fj);
}

static Bool gen_ftintrp_l_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrp.l.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRP_L_D, fd, fj);
}

static Bool gen_ftintrz_w_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrz.w.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRZ_W_S, fd, fj);
}

static Bool gen_ftintrz_w_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrz.w.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRZ_W_D, fd, fj);
}

static Bool gen_ftintrz_l_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrz.l.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRZ_L_S, fd, fj);
}

static Bool gen_ftintrz_l_d ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrz.l.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRZ_L_D, fd, fj);
}

static Bool gen_ftintrne_w_s ( DisResult* dres, UInt insn,
                               const VexArchInfo* archinfo,
                               const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrne.w.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRNE_W_S, fd, fj);
}

static Bool gen_ftintrne_w_d ( DisResult* dres, UInt insn,
                               const VexArchInfo* archinfo,
                               const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrne.w.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINTRNE_W_D, fd, fj);
}

static Bool gen_ftintrne_l_s ( DisResult* dres, UInt insn,
                               const VexArchInfo* archinfo,
                               const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrne.l.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRNE_L_S, fd, fj);
}

static Bool gen_ftintrne_l_d ( DisResult* dres, UInt insn,
                               const VexArchInfo* archinfo,
                               const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftintrne.l.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINTRNE_L_D, fd, fj);
}

static Bool gen_ftint_w_s ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftint.w.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINT_W_S, fd, fj);
}

static Bool gen_ftint_w_d ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftint.w.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_s_helper(FTINT_W_D, fd, fj);
}

static Bool gen_ftint_l_s ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftint.l.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINT_L_S, fd, fj);
}

static Bool gen_ftint_l_d ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ftint.l.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   return gen_convert_d_helper(FTINT_L_D, fd, fj);
}

static Bool gen_ffint_s_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ffint.s.w %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FFINT_S_W, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   IRExpr* f = unop(Iop_ReinterpF32asI32, getFReg32(fj));
   putFReg32(fd, binop(Iop_I32StoF32, rm, f));

   return True;
}

static Bool gen_ffint_s_l ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ffint.s.l %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FFINT_S_L, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   IRExpr* f = unop(Iop_ReinterpF64asI64, getFReg64(fj));
   putFReg32(fd, binop(Iop_I64StoF32, rm, f));

   return True;
}

static Bool gen_ffint_d_w ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ffint.d.w %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FFINT_D_W, 1, fj, 0, 0);
   IRExpr* f = unop(Iop_ReinterpF32asI32, getFReg32(fj));
   putFReg64(fd, unop(Iop_I32StoF64, f));

   return True;
}

static Bool gen_ffint_d_l ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("ffint.d.l %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FFINT_D_L, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   IRExpr* f = unop(Iop_ReinterpF64asI64, getFReg64(fj));
   putFReg64(fd, binop(Iop_I64StoF64, rm, f));

   return True;
}

static Bool gen_frint_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("frint.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FRINT_S, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg32(fd, binop(Iop_RoundF32toInt, rm, getFReg32(fj)));

   return True;
}

static Bool gen_frint_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("frint.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   calculateFCSR(FRINT_D, 1, fj, 0, 0);
   IRExpr* rm = get_rounding_mode();
   putFReg64(fd, binop(Iop_RoundF64toInt, rm, getFReg64(fj)));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for floating point move insns                ---*/
/*------------------------------------------------------------*/

static Bool gen_fmov_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmov.s %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   putFReg32(fd, getFReg32(fj));

   return True;
}

static Bool gen_fmov_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fmov.d %s, %s\n", nameFReg(fd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   putFReg64(fd, getFReg64(fj));

   return True;
}

static Bool gen_fsel ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt ca = get_ca(insn);
   UInt fk = get_fk(insn);
   UInt fj = get_fj(insn);
   UInt fd = get_fd(insn);

   DIP("fsel %s, %s, %s, %s\n", nameFReg(fd), nameFReg(fj),
                                nameFReg(fk), nameFCC(ca));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* cc = unop(Iop_8Uto64, getFCC(ca));
   IRExpr* cond = binop(Iop_CmpEQ64, cc, mkU64(0));
   putFReg64(fd, IRExpr_ITE(cond, getFReg64(fj), getFReg64(fk)));

   return True;
}

static Bool gen_movgr2fr_w ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("movgr2fr.w %s, %s\n", nameFReg(fd), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   /* The high bits might be undefined, now the hardware implementation
      of this instruction is that it is equivalent to movgr2fr.d. */
   putFReg64(fd, unop(Iop_ReinterpI64asF64, getIReg64(rj)));

   return True;
}

static Bool gen_movgr2fr_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("movgr2fr.d %s, %s\n", nameFReg(fd), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   putFReg64(fd, unop(Iop_ReinterpI64asF64, getIReg64(rj)));

   return True;
}

static Bool gen_movgr2frh_w ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("movgr2frh.w %s, %s\n", nameFReg(fd), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* shl1 = binop(Iop_Shl64, getIReg64(rj), mkU8(32));
   IRExpr* i = unop(Iop_ReinterpF64asI64, getFReg64(fd));
   IRExpr* shl2 = binop(Iop_Shl64, i, mkU8(32));
   IRExpr* shr = binop(Iop_Shr64, shl2, mkU8(32));
   IRExpr* or = binop(Iop_Or64, shl1, shr);
   putFReg64(fd, unop(Iop_ReinterpI64asF64, or));

   return True;
}

static Bool gen_movfr2gr_s ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt rd = get_rd(insn);

   DIP("movfr2gr.s %s, %s\n", nameIReg(rd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* i = unop(Iop_ReinterpF32asI32, getFReg32(fj));
   putIReg(rd, extendS(Ity_I32, i));

   return True;
}

static Bool gen_movfr2gr_d ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt rd = get_rd(insn);

   DIP("movfr2gr.d %s, %s\n", nameIReg(rd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   putIReg(rd, unop(Iop_ReinterpF64asI64, getFReg64(fj)));

   return True;
}

static Bool gen_movfrh2gr_s ( DisResult* dres, UInt insn,
                              const VexArchInfo* archinfo,
                              const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt rd = get_rd(insn);

   DIP("movfrh2gr.s %s, %s\n", nameIReg(rd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* i = unop(Iop_ReinterpF64asI64, getFReg64(fj));
   IRExpr* shr = binop(Iop_Shr64, i, mkU8(32));
   putIReg(rd, extendS(Ity_I32, unop(Iop_64to32, shr)));

   return True;
}

static Bool gen_movgr2fcsr ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt   rj = get_rj(insn);
   UInt fcsr = get_fcsrl(insn);

   DIP("movgr2fcsr %s, %s\n", nameFCSR(fcsr), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   putFCSR(fcsr, getIReg32(rj));

   return True;
}

static Bool gen_movfcsr2gr ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo* abiinfo )
{
   UInt fcsr = get_fcsrh(insn);
   UInt   rd = get_rd(insn);

   DIP("movfcsr2gr %s, %s\n", nameIReg(rd), nameFCSR(fcsr));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   putIReg(rd, extendS(Ity_I32, getFCSR(fcsr)));

   return True;
}

static Bool gen_movfr2cf ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt fj = get_fj(insn);
   UInt cd = get_cd(insn);

   DIP("movfr2cf %s, %s\n", nameFCC(cd), nameFReg(fj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* i = unop(Iop_ReinterpF64asI64, getFReg64(fj));
   IRExpr* and = binop(Iop_And64, i, mkU64(0x1));
   putFCC(cd, unop(Iop_64to8, and));

   return True;
}

static Bool gen_movcf2fr ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt cj = get_cj(insn);
   UInt fd = get_fd(insn);

   DIP("movcf2fr %s, %s\n", nameFReg(fd), nameFCC(cj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   /* The hardware implementation of this instruction
      does clear the high bits. */
   IRExpr* cc = unop(Iop_8Uto64, getFCC(cj));
   putFReg64(fd, unop(Iop_ReinterpI64asF64, cc));

   return True;
}

static Bool gen_movgr2cf ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt rj = get_rj(insn);
   UInt cd = get_cd(insn);

   DIP("movgr2cf %s, %s\n", nameFCC(cd), nameIReg(rj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* and = binop(Iop_And64, getIReg64(rj), mkU64(0x1));
   putFCC(cd, unop(Iop_64to8, and));

   return True;
}

static Bool gen_movcf2gr ( DisResult* dres, UInt insn,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo* abiinfo )
{
   UInt cj = get_cj(insn);
   UInt rd = get_rd(insn);

   DIP("movcf2gr %s, %s\n", nameIReg(rd), nameFCC(cj));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   /* The hardware implementation of this instruction
      does clear the high bits. */
   putIReg(rd, unop(Iop_8Uto64, getFCC(cj)));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for floating point load/store insns          ---*/
/*------------------------------------------------------------*/

static Bool gen_fld_s ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   fd = get_fd(insn);

   DIP("fld.s %s, %s, %d\n", nameFReg(fd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   putFReg32(fd, load(Ity_F32, addr));

   return True;
}

static Bool gen_fst_s ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   fd = get_fd(insn);

   DIP("fst.s %s, %s, %d\n", nameFReg(fd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   store(addr, getFReg32(fd));

   return True;
}

static Bool gen_fld_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   fd = get_fd(insn);

   DIP("fld.d %s, %s, %d\n", nameFReg(fd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   putFReg64(fd, load(Ity_F64, addr));

   return True;
}

static Bool gen_fst_d ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt si12 = get_si12(insn);
   UInt   rj = get_rj(insn);
   UInt   fd = get_fd(insn);

   DIP("fst.d %s, %s, %d\n", nameFReg(fd), nameIReg(rj),
                             (Int)extend32(si12, 12));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), mkU64(extend64(si12, 12)));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   store(addr, getFReg64(fd));

   return True;
}

static Bool gen_fldx_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fldx.s %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   putFReg32(fd, load(Ity_F32, addr));

   return True;
}

static Bool gen_fldx_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fldx.d %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   putFReg64(fd, load(Ity_F64, addr));

   return True;
}

static Bool gen_fstx_s ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fstx.s %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x3)));
   store(addr, getFReg32(fd));

   return True;
}

static Bool gen_fstx_d ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fstx.d %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* addr = binop(Iop_Add64, getIReg64(rj), getIReg64(rk));
   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_UAL))
      gen_SIGBUS(check_align(addr, mkU64(0x7)));
   store(addr, getFReg64(fd));

   return True;
}

static Bool gen_fldgt_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fldgt.s %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   putFReg32(fd, load(Ity_F32, mkexpr(addr)));

   return True;
}

static Bool gen_fldgt_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fldgt.d %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   putFReg64(fd, load(Ity_F64, mkexpr(addr)));

   return True;
}

static Bool gen_fldle_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fldle.s %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   putFReg32(fd, load(Ity_F32, mkexpr(addr)));

   return True;
}

static Bool gen_fldle_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fldle.d %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   putFReg64(fd, load(Ity_F64, mkexpr(addr)));

   return True;
}

static Bool gen_fstgt_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fstgt.s %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   store(mkexpr(addr), getFReg32(fd));

   return True;
}

static Bool gen_fstgt_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fstgt.d %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLE64U, mkexpr(addr), getIReg64(rk)));
   store(mkexpr(addr), getFReg64(fd));

   return True;
}

static Bool gen_fstle_s ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fstle.s %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x3)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   store(mkexpr(addr), getFReg32(fd));

   return True;
}

static Bool gen_fstle_d ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo* abiinfo )
{
   UInt rk = get_rk(insn);
   UInt rj = get_rj(insn);
   UInt fd = get_fd(insn);

   DIP("fstle.d %s, %s, %s\n", nameFReg(fd), nameIReg(rj), nameIReg(rk));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp addr = newTemp(Ity_I64);
   assign(addr, getIReg64(rj));
   gen_SIGBUS(check_align(mkexpr(addr), mkU64(0x7)));
   gen_SIGSYS(binop(Iop_CmpLT64U, getIReg64(rk), mkexpr(addr)));
   store(mkexpr(addr), getFReg64(fd));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for branch insns                             ---*/
/*------------------------------------------------------------*/

static Bool gen_beqz ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt offs21 = get_offs21(insn);
   UInt     rj = get_rj(insn);

   DIP("beqz %s, %d\n", nameIReg(rj), (Int)extend32(offs21, 21));

   IRExpr* cond = binop(Iop_CmpEQ64, getIReg64(rj), mkU64(0));
   exit(cond, Ijk_Boring, extend64(offs21 << 2, 23));

   return True;
}

static Bool gen_bnez ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt offs21 = get_offs21(insn);
   UInt     rj = get_rj(insn);

   DIP("bnez %s, %d\n", nameIReg(rj), (Int)extend32(offs21, 21));

   IRExpr* cond = binop(Iop_CmpNE64, getIReg64(rj), mkU64(0));
   exit(cond, Ijk_Boring, extend64(offs21 << 2, 23));

   return True;
}

static Bool gen_bceqz ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt offs21 = get_offs21(insn);
   UInt cj     = get_cj(insn);

   DIP("bceqz %s, %d\n", nameFCC(cj), (Int)extend32(offs21, 21));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* cc = unop(Iop_8Uto64, getFCC(cj));
   IRExpr* cond = binop(Iop_CmpEQ64, cc, mkU64(0));
   exit(cond, Ijk_Boring, extend64(offs21 << 2, 23));

   return True;
}

static Bool gen_bcnez ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo* abiinfo )
{
   UInt offs21 = get_offs21(insn);
   UInt cj     = get_cj(insn);

   DIP("bcnez %s, %d\n", nameFCC(cj), (Int)extend32(offs21, 21));

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_FP)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRExpr* cc = unop(Iop_8Uto64, getFCC(cj));
   IRExpr* cond = binop(Iop_CmpNE64, cc, mkU64(0));
   exit(cond, Ijk_Boring, extend64(offs21 << 2, 23));

   return True;
}

static Bool gen_jirl ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt offs16 = get_offs16(insn);
   UInt     rj = get_rj(insn);
   UInt     rd = get_rd(insn);

   DIP("jirl %s, %s, %d\n", nameIReg(rd), nameIReg(rj),
                            (Int)extend32(offs16, 16));

   IRTemp tmp = newTemp(Ity_I64);
   assign(tmp, getIReg64(rj)); /* This is necessary when rd == rj */
   putIReg(rd, mkU64(guest_PC_curr_instr + 4));
   IRExpr* imm = mkU64(extend64(offs16 << 2, 18));
   putPC(binop(Iop_Add64, mkexpr(tmp), imm));

   dres->whatNext = Dis_StopHere;
   dres->jk_StopHere = Ijk_Boring;

   return True;
}

static Bool gen_b ( DisResult* dres, UInt insn,
                    const VexArchInfo* archinfo,
                    const VexAbiInfo* abiinfo )
{
   UInt offs26 = get_offs26(insn);

   DIP("b %d\n", (Int)extend32(offs26, 26));

   putPC(mkU64(guest_PC_curr_instr + extend64(offs26 << 2, 28)));

   dres->whatNext = Dis_StopHere;
   dres->jk_StopHere = Ijk_Boring;

   return True;
}

static Bool gen_bl ( DisResult* dres, UInt insn,
                     const VexArchInfo* archinfo,
                     const VexAbiInfo* abiinfo )
{
   UInt offs26 = get_offs26(insn);

   DIP("bl %d\n", (Int)extend32(offs26, 26));

   putIReg(1, mkU64(guest_PC_curr_instr + 4));
   putPC(mkU64(guest_PC_curr_instr + extend64(offs26 << 2, 28)));

   dres->whatNext = Dis_StopHere;
   dres->jk_StopHere = Ijk_Boring;

   return True;
}

static Bool gen_beq ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt offs16 = get_offs16(insn);
   UInt     rj = get_rj(insn);
   UInt     rd = get_rd(insn);

   DIP("beq %s, %s, %d\n", nameIReg(rj), nameIReg(rd),
                           (Int)extend32(offs16, 16));

   IRExpr* cond = binop(Iop_CmpEQ64, getIReg64(rj), getIReg64(rd));
   exit(cond, Ijk_Boring, extend64(offs16 << 2, 18));

   return True;
}

static Bool gen_bne ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt offs16 = get_offs16(insn);
   UInt     rj = get_rj(insn);
   UInt     rd = get_rd(insn);

   DIP("bne %s, %s, %d\n", nameIReg(rj), nameIReg(rd),
                           (Int)extend32(offs16, 16));

   IRExpr* cond = binop(Iop_CmpNE64, getIReg64(rj), getIReg64(rd));
   exit(cond, Ijk_Boring, extend64(offs16 << 2, 18));

   return True;
}

static Bool gen_blt ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt offs16 = get_offs16(insn);
   UInt     rj = get_rj(insn);
   UInt     rd = get_rd(insn);

   DIP("blt %s, %s, %d\n", nameIReg(rj), nameIReg(rd),
                           (Int)extend32(offs16, 16));

   IRExpr* cond = binop(Iop_CmpLT64S, getIReg64(rj), getIReg64(rd));
   exit(cond, Ijk_Boring, extend64(offs16 << 2, 18));

   return True;
}

static Bool gen_bge ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt offs16 = get_offs16(insn);
   UInt     rj = get_rj(insn);
   UInt     rd = get_rd(insn);

   DIP("bge %s, %s, %d\n", nameIReg(rj), nameIReg(rd),
                           (Int)extend32(offs16, 16));

   IRExpr* cond = binop(Iop_CmpLE64S, getIReg64(rd), getIReg64(rj));
   exit(cond, Ijk_Boring, extend64(offs16 << 2, 18));

   return True;
}

static Bool gen_bltu ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt offs16 = get_offs16(insn);
   UInt     rj = get_rj(insn);
   UInt     rd = get_rd(insn);

   DIP("bltu %s, %s, %d\n", nameIReg(rj), nameIReg(rd),
                            (Int)extend32(offs16, 16));

   IRExpr* cond = binop(Iop_CmpLT64U, getIReg64(rj), getIReg64(rd));
   exit(cond, Ijk_Boring, extend64(offs16 << 2, 18));

   return True;
}

static Bool gen_bgeu ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt offs16 = get_offs16(insn);
   UInt     rj = get_rj(insn);
   UInt     rd = get_rd(insn);

   DIP("bgeu %s, %s, %d\n", nameIReg(rj), nameIReg(rd),
                            (Int)extend32(offs16, 16));

   IRExpr* cond = binop(Iop_CmpLE64U, getIReg64(rd), getIReg64(rj));
   exit(cond, Ijk_Boring, extend64(offs16 << 2, 18));

   return True;
}

/*------------------------------------------------------------*/
/*--- Helpers for Vector integer arithmetic insns          ---*/
/*------------------------------------------------------------*/

static Bool gen_vmax_vmin ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt vd    = SLICE(insn, 4, 0);
   UInt vj    = SLICE(insn, 9, 5);
   UInt vk    = SLICE(insn, 14, 10);
   UInt insSz = SLICE(insn, 16, 15);
   UInt isMin = SLICE(insn, 17, 17);
   UInt isU   = SLICE(insn, 18, 18);

   IROp op = isMin ? isU ? mkVecMINU(insSz) : mkVecMINS(insSz) :
                     isU ? mkVecMAXU(insSz) : mkVecMAXS(insSz);
   UInt id = isU ? (insSz + 4) : insSz;
   const HChar *nm[2] = { "vmax", "vmin" };
   DIP("%s.%s %s, %s, %s\n", nm[isMin], mkInsSize(id),
                             nameVReg(vd), nameVReg(vj), nameVReg(vk));
   putVReg(vd, binop(op, getVReg(vj), getVReg(vk)));
   return True;
}

static IRTemp gen_vmsk_b ( IRTemp shr )
{
   UInt i;
   IRTemp tmp[16];
   IRTemp tOr = newTemp(Ity_I32);
   IRTemp res = newTempV128();

   //1 <-- cond == 1 <-- rk != 0 <-- Iop_CmpNE8 sucess
   //0 <-- cond == 0 <-- rk == 0 <-- Iop_CmpNE8 failed
   for (i = 0; i < 16; i++) {
      tmp[i] = newTemp(Ity_I32);
      assign(tmp[i], binop(Iop_Shl32,
                           unop(Iop_8Uto32,
                                binop(Iop_GetElem8x16,
                                      mkexpr(shr), mkU8(i))),
                           mkU8(i)));
   }

   assign(tOr, binop(Iop_Or32,
                     binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32, mkexpr(tmp[15]), mkexpr(tmp[14])),
                                 binop(Iop_Or32, mkexpr(tmp[13]), mkexpr(tmp[12]))),
                           binop(Iop_Or32,
                                 binop(Iop_Or32, mkexpr(tmp[11]), mkexpr(tmp[10])),
                                 binop(Iop_Or32, mkexpr(tmp[9]), mkexpr(tmp[8])))),
                     binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32, mkexpr(tmp[7]), mkexpr(tmp[6])),
                                 binop(Iop_Or32, mkexpr(tmp[5]), mkexpr(tmp[4]))),
                           binop(Iop_Or32,
                                 binop(Iop_Or32, mkexpr(tmp[3]), mkexpr(tmp[2])),
                                 binop(Iop_Or32, mkexpr(tmp[1]), mkexpr(tmp[0]))))));
   
   assign(res, unop(Iop_64UtoV128, extendU(Ity_I32, mkexpr(tOr))));
   return res;
}

static IRTemp gen_vmsk_h ( IRTemp shr )
{
   UInt i;
   IRTemp tmp[8];
   IRTemp tOr = newTemp(Ity_I32);
   IRTemp res = newTempV128();

   for (i = 0; i < 8; i++) {
      tmp[i] = newTemp(Ity_I32);
      assign(tmp[i], binop(Iop_Shl32,
                           unop(Iop_16Uto32,
                                binop(Iop_GetElem16x8,
                                      mkexpr(shr), mkU8(i))),
                           mkU8(i)));
   }

   assign(tOr, binop(Iop_Or32,
                     binop(Iop_Or32,
                           binop(Iop_Or32, mkexpr(tmp[7]), mkexpr(tmp[6])),
                           binop(Iop_Or32, mkexpr(tmp[5]), mkexpr(tmp[4]))),
                     binop(Iop_Or32,
                           binop(Iop_Or32, mkexpr(tmp[3]), mkexpr(tmp[2])),
                           binop(Iop_Or32, mkexpr(tmp[1]), mkexpr(tmp[0])))));

   assign(res, unop(Iop_64UtoV128, extendU(Ity_I32, mkexpr(tOr))));
   return res;
}

static IRTemp gen_vmsk_w ( IRTemp shr )
{
   UInt i;
   IRTemp tmp[4];
   IRTemp tOr = newTemp(Ity_I32);
   IRTemp res = newTempV128();

   for (i = 0; i < 4; i++) {
      tmp[i] = newTemp(Ity_I32);
      assign(tmp[i], binop(Iop_Shl32,
                           binop(Iop_GetElem32x4,
                                 mkexpr(shr), mkU8(i)),
                           mkU8(i)));
   }

   assign(tOr, binop(Iop_Or32,
                     binop(Iop_Or32, mkexpr(tmp[3]), mkexpr(tmp[2])),
                     binop(Iop_Or32, mkexpr(tmp[1]), mkexpr(tmp[0]))));
   
   assign(res, unop(Iop_64UtoV128, extendU(Ity_I32, mkexpr(tOr))));
   return res;
}

static IRTemp gen_vmsk_d ( IRTemp shr )
{
   UInt i;
   IRTemp tmp[2];
   IRTemp res = newTempV128();

   for (i = 0; i < 2; i++) {
      tmp[i] = newTemp(Ity_I64);
      assign(tmp[i], binop(Iop_Shl64,
                           binop(Iop_GetElem64x2,
                                 mkexpr(shr), mkU8(i)),
                           mkU8(i)));
   }
   
   assign(res, unop(Iop_64UtoV128,
                    binop(Iop_Or64,mkexpr(tmp[1]), mkexpr(tmp[0]))));
   return res;
}

static Bool gen_vmsk ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo*  abiinfo )
{
   UInt vd    = SLICE(insn, 4, 0);
   UInt vj    = SLICE(insn, 9, 5);
   UInt insSz = SLICE(insn, 11, 10);
   UInt insTy = SLICE(insn, 13, 12);

   IRTemp shr = newTempV128();
   IRTemp cmp = newTempV128();
   IRTemp res = newTempV128();
   IRTemp src = newTempV128();
   assign(src, getVReg(vj));

   switch (insTy) {
      case 0b00: {
         UInt shrNum[4] = {7, 15, 31, 63};

         DIP("vmskltz.%s %s, %s\n", mkInsSize(insSz), nameVReg(vd), nameVReg(vj));
         assign(cmp, binop(mkVecCMPGTS(insSz), mkV128(0x0000), mkexpr(src)));
         assign(shr, binop(mkVecSHRN(insSz), mkexpr(cmp), mkU8(shrNum[insSz])));

         switch(insSz) {
            case 0b00: res = gen_vmsk_b(shr); break;
            case 0b01: res = gen_vmsk_h(shr); break;
            case 0b10: res = gen_vmsk_w(shr); break;
            case 0b11: res = gen_vmsk_d(shr); break;
            default: vassert(0);
         }
         break;
      }
      case 0b01: {
         DIP("vmskgez.b %s, %s\n", nameVReg(vd), nameVReg(vj));
         assign(cmp, binop(Iop_OrV128,
                           binop(Iop_CmpGT8Sx16, mkexpr(src), mkV128(0x0000)),
                           binop(Iop_CmpEQ8x16, mkV128(0x0000), mkexpr(src))));
         assign(shr, binop(Iop_ShrN8x16, mkexpr(cmp), mkU8(7)));
         res = gen_vmsk_b(shr);
         break;
      }
      case 0b10: {
         DIP("vmsknz.b %s, %s\n", nameVReg(vd), nameVReg(vj));
         assign(cmp, unop(Iop_NotV128,
                          binop(Iop_CmpEQ8x16, mkV128(0x0000), mkexpr(src))));
         assign(shr, binop(Iop_ShrN8x16, mkexpr(cmp), mkU8(7)));
         res = gen_vmsk_b(shr);
         break;
      }
      default: vassert(0);
   }

   putVReg(vd, mkexpr(res));
   return True;
}

static Bool gen_xvmsk ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo*  abiinfo )
{
   UInt xd    = SLICE(insn, 4, 0);
   UInt xj    = SLICE(insn, 9, 5);
//   UInt insSz = SLICE(insn, 11, 10);
   UInt insTy = SLICE(insn, 13, 12);

   IRTemp shrHi = newTempV128();
   IRTemp shrLo = newTempV128();
   IRTemp cmpHi = newTempV128();
   IRTemp cmpLo = newTempV128();
   IRTemp res = newTemp(Ity_V256);
   IRTemp src = newTemp(Ity_V256);
   assign(src, getXReg(xj));

   switch (insTy) {
      // case 0b00: {
      //    UInt shrNum[4] = {7, 15, 31, 63};

      //    DIP("vmskltz.%s %s, %s\n", mkInsSize(insSz), nameVReg(vd), nameVReg(vj));
      //    assign(cmp, binop(mkVecCMPGTS(insSz), mkV128(0x0000), mkexpr(src)));
      //    assign(shr, binop(mkVecSHRN(insSz), mkexpr(cmp), mkU8(shrNum[insSz])));

      //    switch(insSz) {
      //       case 0b00: res = gen_vmsk_b(shr); break;
      //       case 0b01: res = gen_vmsk_h(shr); break;
      //       case 0b10: res = gen_vmsk_w(shr); break;
      //       case 0b11: res = gen_vmsk_d(shr); break;
      //       default: vassert(0);
      //    }
      //    break;
      // }
      // case 0b01: {
      //    DIP("vmskgez.b %s, %s\n", nameVReg(vd), nameVReg(vj));
      //    assign(cmp, binop(Iop_OrV128,
      //                      binop(Iop_CmpGT8Sx16, mkexpr(src), mkV128(0x0000)),
      //                      binop(Iop_CmpEQ8x16, mkV128(0x0000), mkexpr(src))));
      //    assign(shr, binop(Iop_ShrN8x16, mkexpr(cmp), mkU8(7)));
      //    res = gen_vmsk_b(shr);
      //    break;
      // }
      case 0b10: {
         DIP("xvmsknz.b %s, %s\n", nameXReg(xd), nameXReg(xj));
         IRTemp hi, lo;
         hi = lo = IRTemp_INVALID;
         breakupV256toV128s(src, &hi, &lo);
         assign(cmpHi, unop(Iop_NotV128,
                          binop(Iop_CmpEQ8x16, mkV128(0x0000), mkexpr(hi))));
         assign(shrHi, binop(Iop_ShrN8x16, mkexpr(cmpHi), mkU8(7)));
         assign(cmpLo, unop(Iop_NotV128,
                          binop(Iop_CmpEQ8x16, mkV128(0x0000), mkexpr(lo))));
         assign(shrLo, binop(Iop_ShrN8x16, mkexpr(cmpLo), mkU8(7)));
         assign(res, binop(Iop_V128HLtoV256, mkexpr(gen_vmsk_b(shrHi)), mkexpr(gen_vmsk_b(shrLo))));
         break;
      }
      default: vassert(0);
   }

   putXReg(xd, mkexpr(res));
   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for Vector bit operation insns               ---*/
/*------------------------------------------------------------*/

static Bool gen_logical_v ( DisResult* dres, UInt insn,
                            const VexArchInfo* archinfo,
                            const VexAbiInfo* abiinfo )
{
   UInt vd    = SLICE(insn, 4, 0);
   UInt vj    = SLICE(insn, 9, 5);
   UInt vk    = SLICE(insn, 14, 10);
   UInt insTy = SLICE(insn, 17, 15);

   IRTemp res  = newTempV128();
   IRTemp srcL = newTempV128();
   IRTemp srcR = newTempV128();
   assign(srcL, getVReg(vj));
   assign(srcR, getVReg(vk));

   switch (insTy) {
      case 0b100: //vand.v
         assign(res, binop(Iop_AndV128, mkexpr(srcL), mkexpr(srcR)));
         break;
      case 0b101: //vor.v
         assign(res, binop(Iop_OrV128,  mkexpr(srcL), mkexpr(srcR)));
         break;
      case 0b110: //vxor.v
         assign(res, binop(Iop_XorV128, mkexpr(srcL), mkexpr(srcR)));
         break;
      case 0b111: //vnor.v
         assign(res, unop(Iop_NotV128, binop(Iop_OrV128,
                                             mkexpr(srcL), mkexpr(srcR))));
         break;
      case 0b000: //vandn.v
         assign(res, binop(Iop_AndV128,
                           unop(Iop_NotV128, mkexpr(srcL)),
                           mkexpr(srcR)));
         break;
      case 0b001: //vorn.v
         assign(res, binop(Iop_OrV128,
                           mkexpr(srcL),
                           unop(Iop_NotV128, mkexpr(srcR))));
         break;
      default: vassert(0);
   }

   const HChar *nm[8] = { "vandn.v", "vorn.v", "", "",
                          "vand.v",  "vor.v", "vxor.v", "vnor.v" };
   DIP("%s %s, %s, %s\n", nm[insTy], nameVReg(vd), nameVReg(vj), nameVReg(vk));
   putVReg(vd, mkexpr(res));
   return True;
}


static Bool gen_vbiti ( DisResult* dres, UInt insn,
                        const VexArchInfo* archinfo,
                        const VexAbiInfo*  abiinfo )
{
   UInt vd     = SLICE(insn, 4, 0);
   UInt vj     = SLICE(insn, 9, 5);
   UInt insImm = SLICE(insn, 17, 10);
   UInt insTy  = SLICE(insn, 19, 18);

   IRTemp c1   = newTempV128();
   IRTemp argR = newTempV128();
   IRTemp res  = newTempV128();
   UInt insSz, uImm;

   if ((insImm & 0xf8) == 0x8) {         // 00001mmm; b
      uImm = insImm & 0x07;
      insSz = 0;
   } else if ((insImm & 0xf0) == 0x10) { // 0001mmmm; h
      uImm = insImm & 0x0f;
      insSz = 1;
   } else if ((insImm & 0xe0) == 0x20) { // 001mmmmm; w
      uImm = insImm & 0x1f;
      insSz = 2;
   } else if ((insImm & 0xc0) == 0x40) { // 01mmmmmm; d
      uImm = insImm & 0x3f;
      insSz = 3;
   } else {
      vassert(0);
   }

   switch (insSz) {
      case 0b00: {
         assign(c1, unop(Iop_Dup8x16, mkU8(1)));
         break;
      }
      case 0b01: {
         assign(c1, unop(Iop_Dup16x8, mkU16(1)));
         break;
      }
      case 0b10: {
         assign(c1, unop(Iop_Dup32x4, mkU32(1)));
         break;
      }
      case 0b11: {
         assign(c1, binop(Iop_64HLtoV128, mkU64(1), mkU64(1)));
         break;
      }
      default: vassert(0);
   }

   assign(argR, binop(mkVecSHLN(insSz), mkexpr(c1), mkU8(uImm)));
   switch (insTy) {
      case 0b00: //vbitclri
         assign(res, binop(Iop_AndV128,
                           getVReg(vj), unop(Iop_NotV128, mkexpr(argR))));
         break;
      case 0b01: //vbitseti
         assign(res, binop(Iop_OrV128, getVReg(vj), mkexpr(argR)));
         break;
      case 0b10: //vbitrevi
         assign(res, binop(Iop_XorV128, getVReg(vj), mkexpr(argR)));
         break;
      default: vassert(0);
   }

   const HChar *nm[3] = { "vbitrevi", "vbitclri", "vbitseti" };
   DIP("%s.%s %s, %u\n", nm[insTy], mkInsSize(insSz), nameVReg(vd), uImm);
   putVReg(vd, mkexpr(res));
   return True;
}

/*------------------------------------------------------------*/
/*--- Helpers for Vector String Processing insns           ---*/
/*------------------------------------------------------------*/

/*------------------------------------------------------------*/
/*--- Helpers for Vector Floating-point Operation insns    ---*/
/*------------------------------------------------------------*/

/*------------------------------------------------------------*/
/*--- Helpers for Vector Floating-point Conversion insns   ---*/
/*------------------------------------------------------------*/

/*------------------------------------------------------------*/
/*--- Helpers for Vector comparison and selection insns    ---*/
/*------------------------------------------------------------*/

static Bool gen_vcmp_integer ( DisResult* dres, UInt insn,
                               const VexArchInfo* archinfo,
                               const VexAbiInfo* abiinfo )
{
   UInt vd    = SLICE(insn, 4, 0);
   UInt vj    = SLICE(insn, 9, 5);
   UInt vk    = SLICE(insn, 14, 10);
   UInt insSz = SLICE(insn, 16, 15);
   UInt insTy = SLICE(insn, 19, 17);

   UInt szId   = insSz;
   IRTemp res  = newTempV128();
   IRTemp argL = newTempV128();
   IRTemp argR = newTempV128();
   assign(argL, getVReg(vj));
   assign(argR, getVReg(vk));

   switch (insTy) {
      case 0b000: { //vseq
         assign(res, binop(mkVecCMPEQ(insSz), mkexpr(argL), mkexpr(argR)));
         break;
      }
      case 0b001: { //vsle.x
         assign(res, binop(Iop_OrV128,
                           binop(mkVecCMPGTS(insSz), mkexpr(argR), mkexpr(argL)),
                           binop(mkVecCMPEQ(insSz), mkexpr(argL), mkexpr(argR))));
         break;
      }
      case 0b010: { //vsle.xu
         assign(res, binop(Iop_OrV128,
                           binop(mkVecCMPGTU(insSz), mkexpr(argR), mkexpr(argL)),
                           binop(mkVecCMPEQ(insSz), mkexpr(argL), mkexpr(argR))));
         szId = insSz + 4;
         break;
      }
      case 0b011: { //vslt
         assign(res, binop(mkVecCMPGTS(insSz), mkexpr(argR), mkexpr(argL)));
         break;
      }
      case 0b100: { //vslt{u}
         assign(res, binop(mkVecCMPGTU(insSz), mkexpr(argR), mkexpr(argL)));
         szId = insSz + 4;
         break;
      }
      default: vassert(0);
   }

   const HChar *nm[5] = { "vseq",  "vsle",  "vsle",  "vslt",  "vslt" };
   DIP("%s.%s %s, %s, %s\n", nm[insTy], mkInsSize(szId),
                             nameVReg(vd), nameVReg(vj), nameVReg(vk));
   putVReg(vd, mkexpr(res));
   return True;
}

static Bool gen_vset ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo*  abiinfo )
{
   UInt cd    = SLICE(insn, 2, 0);
   UInt vj    = SLICE(insn, 9, 5);
   UInt insSz = SLICE(insn, 11, 10);
   UInt insTy = SLICE(insn, 13, 12);

   IROp ops64   = Iop_INVALID;
   IRTemp resHi = newTemp(Ity_I64);
   IRTemp resLo = newTemp(Ity_I64);
   IRTemp res   = newTempV128();
   IRTemp eq    = newTempV128();
   IRTemp z128  = newTempV128();
   assign(z128, mkV128(0x0000));

   switch (insTy) {
      case 0b01: {
         if (SLICE(insn, 10, 10) == 0b0) {
            DIP("vseteqz.v %u, %s", cd, nameVReg(vj));
            assign(res, binop(Iop_CmpEQ64x2, getVReg(vj), mkexpr(z128)));
            ops64 = Iop_And64;
         } else {
            DIP("vsetnez.v %u, %s", cd, nameVReg(vj));
            assign(res, unop(Iop_NotV128,
                             binop(Iop_CmpEQ64x2, getVReg(vj), mkexpr(z128))));
            ops64 = Iop_Or64;
         }
         break;
      }
      case 0b10: {  //vsetanyeqz
         DIP("vsetanyeqz.%s %u, %s", mkInsSize(insSz), cd, nameVReg(vj));
         assign(eq, binop(mkVecCMPEQ(insSz), getVReg(vj), mkexpr(z128)));
         assign(res, unop(Iop_NotV128,
                          binop(Iop_CmpEQ64x2, mkexpr(eq), mkexpr(z128))));
         ops64 = Iop_Or64;
         break;
      }
      case 0b11: {  //vsetqllnez
         DIP("vsetqllnez.%s %u, %s", mkInsSize(insSz), cd, nameVReg(vj));
         assign(eq, binop(mkVecCMPEQ(insSz), getVReg(vj), mkexpr(z128)));
         assign(res, binop(Iop_CmpEQ64x2, mkexpr(eq), mkexpr(z128)));
         ops64 = Iop_And64;
         break;
      }
      default: vassert(0);
   }

   assign(resHi, binop(Iop_GetElem64x2, mkexpr(res), mkU8(1)));
   assign(resLo, binop(Iop_GetElem64x2, mkexpr(res), mkU8(0)));
   putFCC(cd, unop(Iop_64to8, binop(ops64, mkexpr(resHi), mkexpr(resLo))));
   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for Vector moving and shuffling insns        ---*/
/*------------------------------------------------------------*/

static Bool gen_vreplgr2vr ( DisResult* dres, UInt insn,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo*  abiinfo )
{
   UInt vd    = SLICE(insn, 4, 0);
   UInt rj    = SLICE(insn, 9, 5);
   UInt insSz = SLICE(insn, 11, 10);

   IRTemp res = newTempV128();
   switch (insSz) {
      case 0b00:
         assign(res, unop(Iop_Dup8x16, getIReg8(rj)));
         break;
      case 0b01:
         assign(res, unop(Iop_Dup16x8, getIReg16(rj)));
         break;
      case 0b10:
         assign(res, unop(Iop_Dup32x4, getIReg32(rj)));
         break;
      case 0b11:
         assign(res, binop(Iop_64HLtoV128, getIReg64(rj), getIReg64(rj)));
         break;
      default: vassert(0);
   }

   DIP("vreplgr2vr.%s %s, %s", mkInsSize(insSz),
                               nameVReg(vd), nameIReg(rj));
   putVReg(vd, mkexpr(res));
   return True;
}

static Bool gen_evod ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt vd    = SLICE(insn, 4, 0);
   UInt vj    = SLICE(insn, 9, 5);
   UInt vk    = SLICE(insn, 14, 10);
   UInt insSz = SLICE(insn, 16, 15);

   const HChar *nm;
   IRTemp argL = newTempV128();
   IRTemp argR = newTempV128();
   IRTemp res  = newTempV128();

   switch (SLICE(insn, 19, 17)) {
      case 0b011: //vpackev
         nm = "vpackev";
         assign(argL, binop(mkVecPACKEV(insSz),
                            getVReg(vj),
                            mkV128(0x0000)));
         assign(argR, binop(mkVecPACKEV(insSz),
                            getVReg(vk),
                            mkV128(0x0000)));
         assign(res, binop(mkVecINTERLEAVEHI(insSz),
                           mkexpr(argL),
                           mkexpr(argR)));
         break;
      case 0b100: //vpackod
         nm = "vpackod";
         assign(argL, binop(mkVecPACKOD(insSz),
                            getVReg(vj),
                            mkV128(0x0000)));
         assign(argR, binop(mkVecPACKOD(insSz),
                            getVReg(vk),
                            mkV128(0x0000)));
         assign(res, binop(mkVecINTERLEAVEHI(insSz),
                           mkexpr(argL),
                           mkexpr(argR)));
         break;
      case 0b101: //vilvl
         nm = "vilvl";
         assign(res, binop(mkVecINTERLEAVELO(insSz),
                           getVReg(vj),
                           getVReg(vk)));
         break;
      case 0b110: //vilvh
         nm = "vilvh";
         assign(res, binop(mkVecINTERLEAVEHI(insSz),
                           getVReg(vj),
                           getVReg(vk)));
         break;
      case 0b111: //vpickev
         nm = "vpickev";
         assign(res, binop(mkVecPACKEV(insSz),
                           getVReg(vj),
                           getVReg(vk)));
         break;
      case 0b000: //vpickod
         nm = "vpickod";
         assign(res, binop(mkVecPACKOD(insSz),
                           getVReg(vj),
                           getVReg(vk)));
         break;
      default: vassert(0);
   }

   DIP("%s.%s %s, %s, %s\n", nm, mkInsSize(insSz),
                             nameVReg(vd), nameVReg(vj), nameVReg(vk));
   putVReg(vd, mkexpr(res));
   return True;
}

static Bool gen_vshuf_b ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo*  abiinfo )
{
   UInt va = SLICE(insn, 19, 15);
   UInt vk = SLICE(insn, 14, 10);
   UInt vj = SLICE(insn, 9, 5);
   UInt vd = SLICE(insn, 4, 0);

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LSX)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   IRTemp sHi = newTemp(Ity_V128);
   IRTemp sLo = newTemp(Ity_V128);
   IRTemp sId = newTemp(Ity_V128);
   assign(sHi, getVReg(vj));
   assign(sLo, getVReg(vk));
   assign(sId, getVReg(va));
   UInt i;
   IRTemp id[16], res[16];

   for (i = 0; i < 16; i++) {
         id[i] = newTemp(Ity_I8);
         res[i] = newTemp(Ity_I8);

         assign(id[i], binop(Iop_GetElem8x16, mkexpr(sId), mkU8(i)));

         assign(res[i], IRExpr_ITE(
                           binop(Iop_CmpEQ64,
                                 extendU(Ity_I8, binop(Iop_And8,
                                                       mkexpr(id[i]),
                                                       mkU8(0xC0))),
                                 mkU64(0x0)),
                           IRExpr_ITE(
                              binop(Iop_CmpLT64U,
                                    extendU(Ity_I8, binop(Iop_And8,
                                                          mkexpr(id[i]),
                                                          mkU8(0x1F))),
                                    mkU64(0x10)),
                              binop(Iop_GetElem8x16,
                                    mkexpr(sLo),
                                    mkexpr(id[i])),
                              binop(Iop_GetElem8x16,
                                    mkexpr(sHi),
                                    unop(Iop_64to8,
                                         binop(Iop_Sub64,
                                               extendU(Ity_I8, mkexpr(id[i])),
                                               mkU64(0x10))))),
                           mkU8(0x0)));
   }

   putVReg(vd,
            binop(Iop_64HLtoV128,
                  binop(Iop_32HLto64,
                        binop(Iop_16HLto32,
                              binop(Iop_8HLto16, mkexpr(res[15]), mkexpr(res[14])),
                              binop(Iop_8HLto16, mkexpr(res[13]), mkexpr(res[12]))),
                        binop(Iop_16HLto32,
                              binop(Iop_8HLto16, mkexpr(res[11]), mkexpr(res[10])),
                              binop(Iop_8HLto16, mkexpr(res[9]), mkexpr(res[8])))),
                  binop(Iop_32HLto64,
                        binop(Iop_16HLto32,
                              binop(Iop_8HLto16, mkexpr(res[7]), mkexpr(res[6])),
                              binop(Iop_8HLto16, mkexpr(res[5]), mkexpr(res[4]))),
                        binop(Iop_16HLto32,
                              binop(Iop_8HLto16, mkexpr(res[3]), mkexpr(res[2])),
                              binop(Iop_8HLto16, mkexpr(res[1]), mkexpr(res[0]))))));

   return True;
}


/*------------------------------------------------------------*/
/*--- Helpers for Vector load/store insns                  ---*/
/*------------------------------------------------------------*/

static Bool gen_vld ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt vd   = SLICE(insn, 4, 0);
   UInt rj   = SLICE(insn, 9, 5);
   UInt si12 = SLICE(insn, 21, 10);

   IRExpr* addr = binop(Iop_Add64,
                        getIReg64(rj),
                        mkU64(extend64(si12, 12)));
   DIP("vld %s, %s, %d\n", nameVReg(vd), nameIReg(rj),
                           (Int)extend32(si12, 12));
   putVReg(vd, load(Ity_V128, addr));
   return True;
}

static Bool gen_xvld ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt xd   = SLICE(insn, 4, 0);
   UInt rj   = SLICE(insn, 9, 5);
   UInt si12 = SLICE(insn, 21, 10);

   IRExpr* addr = binop(Iop_Add64,
                        getIReg64(rj),
                        mkU64(extend64(si12, 12)));
   DIP("xvld %s, %s, %d\n", nameXReg(xd), nameIReg(rj),
                           (Int)extend32(si12, 12));
   putXReg(xd, load(Ity_V256, addr));
   return True;
}

static Bool gen_vldrepl ( DisResult* dres, UInt insn,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo*  abiinfo )
{
   UInt vd     = SLICE(insn, 4, 0);
   UInt rj     = SLICE(insn, 9, 5);
   UInt insImm = SLICE(insn, 23, 10);

   UInt sImm, insSz;
   IRTemp res = newTempV128();
   IRTemp addr = newTemp(Ity_I64);

   if ((insImm & 0x3000) == 0x2000) {     // 10si12; b
      sImm = insImm & 0xfff;
      insSz = 0;
   } else if ((insImm & 0x3800) == 0x1000) { // 010si11; h
      sImm = insImm & 0x7ff;
      insSz = 1;
   } else if ((insImm & 0x3c00) == 0x800) { // 0010si10; w
      sImm = insImm & 0x3ff;
      insSz = 2;
   } else if ((insImm & 0x3e00) == 0x400) { // 00010si9; d
      sImm = insImm & 0x1ff;
      insSz = 3;
   } else {
      vassert(0);
   }

   switch (insSz) {
      case 0b00: {
         assign(addr, binop(Iop_Add64,
                            getIReg64(rj),
                            mkU64(extend64(sImm, 12))));
         assign(res, unop(Iop_Dup8x16, load(Ity_I8, mkexpr(addr))));
         break;
      }
      case 0b01: {
         assign(addr, binop(Iop_Add64,
                            getIReg64(rj),
                            mkU64(extend64(sImm << 1, 12))));
         assign(res, unop(Iop_Dup16x8, load(Ity_I16, mkexpr(addr))));
         break;
      }
      case 0b10: {
         assign(addr, binop(Iop_Add64,
                            getIReg64(rj),
                            mkU64(extend64(sImm << 2, 12))));
         assign(res, unop(Iop_Dup32x4, load(Ity_I32, mkexpr(addr))));
         break;
      }
      case 0b11: {
         assign(addr, binop(Iop_Add64,
                            getIReg64(rj),
                            mkU64(extend64(sImm << 3, 12))));
         assign(res, binop(Iop_64HLtoV128,
                           load(Ity_I64, mkexpr(addr)),
                           load(Ity_I64, mkexpr(addr))));
         break;
      }
      default: vassert(0);
   }

   DIP("vldrepl.%s %s, %s, %u\n", mkInsSize(insSz),
                                  nameVReg(vd), nameIReg(rj), sImm);
   putVReg(vd, mkexpr(res));
   return True;
}

static Bool gen_vst ( DisResult* dres, UInt insn,
                      const VexArchInfo* archinfo,
                      const VexAbiInfo* abiinfo )
{
   UInt vd   = SLICE(insn, 4, 0);
   UInt rj   = SLICE(insn, 9, 5);
   UInt si12 = SLICE(insn, 21, 10);

   IRExpr* addr = binop(Iop_Add64,
                        getIReg64(rj),
                        mkU64(extend64(si12, 12)));
   DIP("vst %s, %s, %d\n", nameVReg(vd), nameIReg(rj),
                           (Int)extend32(si12, 12));
   store(addr, getVReg(vd));
   return True;
}

static Bool gen_xvst ( DisResult* dres, UInt insn,
                       const VexArchInfo* archinfo,
                       const VexAbiInfo* abiinfo )
{
   UInt xd   = SLICE(insn, 4, 0);
   UInt rj   = SLICE(insn, 9, 5);
   UInt si12 = SLICE(insn, 21, 10);

   IRExpr* addr = binop(Iop_Add64,
                        getIReg64(rj),
                        mkU64(extend64(si12, 12)));
   DIP("xvst %s, %s, %d\n", nameXReg(xd), nameIReg(rj),
                            (Int)extend32(si12, 12));
   store(addr, getXReg(xd));
   return True;
}

static Bool gen_vstelm ( DisResult* dres, UInt insn,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo*  abiinfo )
{
   UInt vd     = SLICE(insn, 4, 0);
   UInt rj     = SLICE(insn, 9, 5);
   UInt si8    = SLICE(insn, 17, 10);
   UInt insImm = SLICE(insn, 23, 18);

   IRExpr* addr;
   UInt idx, insSz;

   if ((insImm & 0x30) == 0x20) {     // 10_idx; b
      idx = insImm & 0xf;
      insSz = 0;
   } else if ((insImm & 0x38) == 0x10) { // 01_idx; h
      idx = insImm & 0x7;
      insSz = 1;
   } else if ((insImm & 0x3c) == 0x8) { // 001_idx; w
      idx = insImm & 0x3;
      insSz = 2;
   } else if ((insImm & 0x3e) == 0x4) { // 0001_idx; d
      idx = insImm & 0x1;
      insSz = 3;
   } else {
      vassert(0);
   }

   switch (insSz) {
      case 0b00:
         addr = binop(Iop_Add64,
                      getIReg64(rj),
                      mkU64(extend64(si8, 8)));
         break;
      case 0b01:
         addr = binop(Iop_Add64,
                      getIReg64(rj),
                      mkU64(extend64(si8 << 1, 9)));
         break;
      case 0b10:
         addr = binop(Iop_Add64,
                      getIReg64(rj),
                      mkU64(extend64(si8 << 2, 10)));
         break;
      case 0b11:
         addr = binop(Iop_Add64,
                      getIReg64(rj),
                      mkU64(extend64(si8 << 3, 11)));
         break;
      default: vassert(0);
   }

   DIP("vstelm.%s %s, %s, %d, %u\n", mkInsSize(insSz), nameVReg(vd), nameIReg(rj),
                                     (Int)extend32(si8, 8), idx);
   store(addr, binop(mkVecGetElem(insSz), getVReg(vd), mkU8(idx)));
   return True;
}


/*------------------------------------------------------------*/
/*--- Disassemble a single LOONGARCH64 instruction         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single LOONGARCH64 instruction into IR.  The instruction
   has is located at |guest_instr| and has guest IP of |guest_PC_curr_instr|,
   which will have been set before the call here.  Returns True iff the
   instruction was decoded, in which case *dres will be set accordingly,
   or False, in which case *dres should be ignored by the caller. */

static Bool disInstr_LOONGARCH64_WRK_special ( DisResult* dres,
                                               const UChar* guest_instr )
{
   const UChar* code = guest_instr;
   /* Spot the 16-byte preamble:
      00450c00  srli.d $zero, $zero, 3
      00453400  srli.d $zero, $zero, 13
      00457400  srli.d $zero, $zero, 29
      00454c00  srli.d $zero, $zero, 19
   */
   if (getUInt(code +  0) == 0x00450c00 &&
       getUInt(code +  4) == 0x00453400 &&
       getUInt(code +  8) == 0x00457400 &&
       getUInt(code + 12) == 0x00454c00) {
      /* Got a "Special" instruction preamble.  Which one is it? */
      if (getUInt(code + 16) == 0x001535ad) {        /* or $t1, $t1, $t1 */
         DIP("$a7 = client_request ( $t0 )\n");
         putPC(mkU64(guest_PC_curr_instr + 20));
         dres->whatNext    = Dis_StopHere;
         dres->len         = 20;
         dres->jk_StopHere = Ijk_ClientReq;
         return True;
      } else if (getUInt(code + 16) == 0x001539ce) { /* or $t2, $t2, $t2 */
         DIP("$a7 = guest_NRADDR\n");
         putIReg(11, IRExpr_Get(offsetof(VexGuestLOONGARCH64State, guest_NRADDR),
                     Ity_I64));
         dres->len = 20;
         return True;
      } else if (getUInt(code + 16) == 0x00153def) { /* or $t3, $t3, $t3 */
         DIP("branch-and-link-to-noredir $t8\n");
         putIReg(1, mkU64(guest_PC_curr_instr + 20));
         putPC(getIReg64(20));
         dres->whatNext    = Dis_StopHere;
         dres->len         = 20;
         dres->jk_StopHere = Ijk_NoRedir;
         return True;
      } else if (getUInt(code + 16) == 0x00154210) { /* or $t4, $t4, $t4 */
         DIP("IR injection\n");
         vex_inject_ir(irsb, Iend_LE);
         /* Invalidate the current insn. The reason is that the IRop we're
            injecting here can change. In which case the translation has to
            be redone. For ease of handling, we simply invalidate all the
            time.
          */
         stmt(IRStmt_Put(offsetof(VexGuestLOONGARCH64State, guest_CMSTART),
                         mkU64(guest_PC_curr_instr)));
         stmt(IRStmt_Put(offsetof(VexGuestLOONGARCH64State, guest_CMLEN),
                         mkU64(20)));
         putPC(mkU64(guest_PC_curr_instr + 20));
         dres->whatNext    = Dis_StopHere;
         dres->len         = 20;
         dres->jk_StopHere = Ijk_InvalICache;
         return True;
      }
      /* We don't know what it is. */
      vassert(0);
      /*NOTREACHED*/
   }
   return False;
}

static Bool disInstr_LOONGARCH64_WRK_00_0000_0000 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 21, 15)) {
      case 0b0000000:
         switch (SLICE(insn, 14, 10)) {
            case 0b00100:
               ok = gen_clo_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b00101:
               ok = gen_clz_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b00110:
               ok = gen_cto_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b00111:
               ok = gen_ctz_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b01000:
               ok = gen_clo_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01001:
               ok = gen_clz_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01010:
               ok = gen_cto_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01011:
               ok = gen_ctz_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01100:
               ok = gen_revb_2h(dres, insn, archinfo, abiinfo);
               break;
            case 0b01101:
               ok = gen_revb_4h(dres, insn, archinfo, abiinfo);
               break;
            case 0b01110:
               ok = gen_revb_2w(dres, insn, archinfo, abiinfo);
               break;
            case 0b01111:
               ok = gen_revb_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b10000:
               ok = gen_revh_2w(dres, insn, archinfo, abiinfo);
               break;
            case 0b10001:
               ok = gen_revh_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b10010:
               ok = gen_bitrev_4b(dres, insn, archinfo, abiinfo);
               break;
            case 0b10011:
               ok = gen_bitrev_8b(dres, insn, archinfo, abiinfo);
               break;
            case 0b10100:
               ok = gen_bitrev_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b10101:
               ok = gen_bitrev_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b10110:
               ok = gen_ext_w_h(dres, insn, archinfo, abiinfo);
               break;
            case 0b10111:
               ok = gen_ext_w_b(dres, insn, archinfo, abiinfo);
               break;
            case 0b11000:
               ok = gen_rdtimel_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b11001:
               ok = gen_rdtimeh_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b11010:
               ok = gen_rdtime_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b11011:
               ok = gen_cpucfg(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0000010:
         ok = gen_asrtle_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0000011:
         ok = gen_asrtgt_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100000:
         ok = gen_add_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100001:
         ok = gen_add_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100010:
         ok = gen_sub_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100011:
         ok = gen_sub_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100100:
         ok = gen_slt(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100101:
         ok = gen_sltu(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100110:
         ok = gen_maskeqz(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100111:
         ok = gen_masknez(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101000:
         ok = gen_nor(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101001:
         ok = gen_and(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101010:
         ok = gen_or(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101011:
         ok = gen_xor(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101100:
         ok = gen_orn(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101101:
         ok = gen_andn(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101110:
         ok = gen_sll_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101111:
         ok = gen_srl_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110000:
         ok = gen_sra_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110001:
         ok = gen_sll_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110010:
         ok = gen_srl_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110011:
         ok = gen_sra_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110110:
         ok = gen_rotr_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110111:
         ok = gen_rotr_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111000:
         ok = gen_mul_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111001:
         ok = gen_mulh_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111010:
         ok = gen_mulh_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111011:
         ok = gen_mul_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111100:
         ok = gen_mulh_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111101:
         ok = gen_mulh_du(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111110:
         ok = gen_mulw_d_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111111:
         ok = gen_mulw_d_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000000:
         ok = gen_div_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000001:
         ok = gen_mod_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000010:
         ok = gen_div_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000011:
         ok = gen_mod_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000100:
         ok = gen_div_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000101:
         ok = gen_mod_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000110:
         ok = gen_div_du(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000111:
         ok = gen_mod_du(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001000:
         ok = gen_crc_w_b_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001001:
         ok = gen_crc_w_h_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001010:
         ok = gen_crc_w_w_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001011:
         ok = gen_crc_w_d_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001100:
         ok = gen_crcc_w_b_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001101:
         ok = gen_crcc_w_h_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001110:
         ok = gen_crcc_w_w_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001111:
         ok = gen_crcc_w_d_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010100:
         ok = gen_break(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010110:
         ok = gen_syscall(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }

   if (ok) {
      return ok;
   }

   switch (SLICE(insn, 21, 18)) {
      case 0b0001:
         if (SLICE(insn, 17, 17) == 0) {
            ok = gen_alsl_w(dres, insn, archinfo, abiinfo);
         } else {
            ok = gen_alsl_wu(dres, insn, archinfo, abiinfo);
         }
         break;
      case 0b0010:
         if (SLICE(insn, 17, 17) == 0) {
            ok = gen_bytepick_w(dres, insn, archinfo, abiinfo);
         } else {
            ok = False;
         }
         break;
      case 0b0011:
         ok = gen_bytepick_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011:
         if (SLICE(insn, 17, 17) == 0) {
            ok = gen_alsl_d(dres, insn, archinfo, abiinfo);
         } else {
            ok = False;
         }
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_0000_0001 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;
   if (SLICE(insn, 21, 21) == 0) {
      switch (SLICE(insn, 20, 16)) {
         case 0b00000:
            if (SLICE(insn, 15, 15) == 1) {
               ok = gen_slli_w(dres, insn, archinfo, abiinfo);
            } else {
               ok = False;
            }
            break;
         case 0b00001:
            ok = gen_slli_d(dres, insn, archinfo, abiinfo);
            break;
         case 0b00100:
            if (SLICE(insn, 15, 15) == 1) {
               ok = gen_srli_w(dres, insn, archinfo, abiinfo);
            } else {
               ok = False;
            }
            break;
         case 0b00101:
            ok = gen_srli_d(dres, insn, archinfo, abiinfo);
            break;
         case 0b01000:
            if (SLICE(insn, 15, 15) == 1) {
               ok = gen_srai_w(dres, insn, archinfo, abiinfo);
            } else {
               ok = False;
            }
            break;
         case 0b01001:
            ok = gen_srai_d(dres, insn, archinfo, abiinfo);
            break;
         case 0b01100:
            if (SLICE(insn, 15, 15) == 1) {
               ok = gen_rotri_w(dres, insn, archinfo, abiinfo);
            } else {
               ok = False;
            }
            break;
         case 0b01101:
            ok = gen_rotri_d(dres, insn, archinfo, abiinfo);
            break;
         default:
            ok = False;
            break;
      }
   } else {
      if (SLICE(insn, 15, 15) == 0) {
         ok = gen_bstrins_w(dres, insn, archinfo, abiinfo);
      } else {
         ok = gen_bstrpick_w(dres, insn, archinfo, abiinfo);
      }
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_0000_0100 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 21, 15)) {
      case 0b0000001:
         ok = gen_fadd_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0000010:
         ok = gen_fadd_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0000101:
         ok = gen_fsub_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0000110:
         ok = gen_fsub_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001001:
         ok = gen_fmul_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001010:
         ok = gen_fmul_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001101:
         ok = gen_fdiv_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001110:
         ok = gen_fdiv_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010001:
         ok = gen_fmax_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010010:
         ok = gen_fmax_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010101:
         ok = gen_fmin_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010110:
         ok = gen_fmin_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0011001:
         ok = gen_fmaxa_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0011010:
         ok = gen_fmaxa_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0011101:
         ok = gen_fmina_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0011110:
         ok = gen_fmina_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100001:
         ok = gen_fscaleb_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100010:
         ok = gen_fscaleb_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100101:
         ok = gen_fcopysign_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100110:
         ok = gen_fcopysign_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101000:
         switch (SLICE(insn, 14, 10)) {
            case 0b00001:
               ok = gen_fabs_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b00010:
               ok = gen_fabs_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b00101:
               ok = gen_fneg_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b00110:
               ok = gen_fneg_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01001:
               ok = gen_flogb_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b01010:
               ok = gen_flogb_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01101:
               ok = gen_fclass_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b01110:
               ok = gen_fclass_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b10001:
               ok = gen_fsqrt_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b10010:
               ok = gen_fsqrt_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b10101:
               ok = gen_frecip_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b10110:
               ok = gen_frecip_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b11001:
               ok = gen_frsqrt_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b11010:
               ok = gen_frsqrt_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0101001:
         switch (SLICE(insn, 14, 10)) {
            case 0b00101:
               ok = gen_fmov_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b00110:
               ok = gen_fmov_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01001:
               ok = gen_movgr2fr_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b01010:
               ok = gen_movgr2fr_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01011:
               ok = gen_movgr2frh_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b01101:
               ok = gen_movfr2gr_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b01110:
               ok = gen_movfr2gr_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01111:
               ok = gen_movfrh2gr_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b10000:
               ok = gen_movgr2fcsr(dres, insn, archinfo, abiinfo);
               break;
            case 0b10010:
               ok = gen_movfcsr2gr(dres, insn, archinfo, abiinfo);
               break;
            case 0b10100:
               if (SLICE(insn, 4, 3) == 0b00) {
                  ok = gen_movfr2cf(dres, insn, archinfo, abiinfo);
               } else {
                  ok = False;
               }
               break;
            case 0b10101:
               if (SLICE(insn, 9, 8) == 0b00) {
                  ok = gen_movcf2fr(dres, insn, archinfo, abiinfo);
               } else {
                  ok = False;
               }
               break;
            case 0b10110:
               if (SLICE(insn, 4, 3) == 0b00) {
                  ok = gen_movgr2cf(dres, insn, archinfo, abiinfo);
               } else {
                  ok = False;
               }
               break;
            case 0b10111:
               if (SLICE(insn, 9, 8) == 0b00) {
                  ok = gen_movcf2gr(dres, insn, archinfo, abiinfo);
               } else {
                  ok = False;
               }
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0110010:
         switch (SLICE(insn, 14, 10)) {
            case 0b00110:
               ok = gen_fcvt_s_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01001:
               ok = gen_fcvt_d_s(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0110100:
         switch (SLICE(insn, 14, 10)) {
            case 0b00001:
               ok = gen_ftintrm_w_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b00010:
               ok = gen_ftintrm_w_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01001:
               ok = gen_ftintrm_l_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b01010:
               ok = gen_ftintrm_l_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b10001:
               ok = gen_ftintrp_w_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b10010:
               ok = gen_ftintrp_w_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b11001:
               ok = gen_ftintrp_l_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b11010:
               ok = gen_ftintrp_l_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0110101:
         switch (SLICE(insn, 14, 10)) {
            case 0b00001:
               ok = gen_ftintrz_w_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b00010:
               ok = gen_ftintrz_w_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01001:
               ok = gen_ftintrz_l_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b01010:
               ok = gen_ftintrz_l_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b10001:
               ok = gen_ftintrne_w_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b10010:
               ok = gen_ftintrne_w_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b11001:
               ok = gen_ftintrne_l_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b11010:
               ok = gen_ftintrne_l_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0110110:
         switch (SLICE(insn, 14, 10)) {
            case 0b00001:
               ok = gen_ftint_w_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b00010:
               ok = gen_ftint_w_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b01001:
               ok = gen_ftint_l_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b01010:
               ok = gen_ftint_l_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0111010:
         switch (SLICE(insn, 14, 10)) {
            case 0b00100:
               ok = gen_ffint_s_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b00110:
               ok = gen_ffint_s_l(dres, insn, archinfo, abiinfo);
               break;
            case 0b01000:
               ok = gen_ffint_d_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b01010:
               ok = gen_ffint_d_l(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0111100:
         switch (SLICE(insn, 14, 10)) {
            case 0b10001:
               ok = gen_frint_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b10010:
               ok = gen_frint_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_0000 ( DisResult* dres, UInt insn,
                                               const VexArchInfo* archinfo,
                                               const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 25, 22)) {
      case 0b0000:
         ok = disInstr_LOONGARCH64_WRK_00_0000_0000(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001:
         ok = disInstr_LOONGARCH64_WRK_00_0000_0001(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010:
         ok = gen_bstrins_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0011:
         ok = gen_bstrpick_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100:
         ok = disInstr_LOONGARCH64_WRK_00_0000_0100(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000:
         ok = gen_slti(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001:
         ok = gen_sltui(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010:
         ok = gen_addi_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011:
         ok = gen_addi_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100:
         ok = gen_lu52i_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101:
         ok = gen_andi(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110:
         ok = gen_ori(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111:
         ok = gen_xori(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_1010 ( DisResult* dres, UInt insn,
                                               const VexArchInfo* archinfo,
                                               const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 25, 22)) {
      case 0b0000:
         ok = gen_ld_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001:
         ok = gen_ld_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010:
         ok = gen_ld_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0011:
         ok = gen_ld_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100:
         ok = gen_st_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101:
         ok = gen_st_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110:
         ok = gen_st_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111:
         ok = gen_st_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000:
         ok = gen_ld_bu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001:
         ok = gen_ld_hu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010:
         ok = gen_ld_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011:
         ok = gen_preld(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100:
         ok = gen_fld_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101:
         ok = gen_fst_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110:
         ok = gen_fld_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111:
         ok = gen_fst_d(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_1011 ( DisResult* dres, UInt insn,
                                               const VexArchInfo* archinfo,
                                               const VexAbiInfo*  abiinfo )
{
   Bool ok;

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LSX)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   switch (SLICE(insn, 23, 22)) {
      case 0b00:
         ok = gen_vld(dres, insn, archinfo, abiinfo); break;
      case 0b01:
         ok = gen_vst(dres, insn, archinfo, abiinfo); break;
      case 0b10:
         ok = gen_xvld(dres, insn, archinfo, abiinfo); break;
      case 0b11:
         ok = gen_xvst(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_1100 ( DisResult* dres, UInt insn,
                                               const VexArchInfo* archinfo,
                                               const VexAbiInfo*  abiinfo )
{
   Bool ok;

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LSX)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   switch (SLICE(insn, 25, 24)) {
      case 0b00:
         ok = gen_vldrepl(dres, insn, archinfo, abiinfo); break;
      case 0b01:
         ok = gen_vstelm(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_1110_0000 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 21, 15)) {
      case 0b0000000:
         ok = gen_ldx_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001000:
         ok = gen_ldx_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010000:
         ok = gen_ldx_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0011000:
         ok = gen_ldx_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100000:
         ok = gen_stx_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101000:
         ok = gen_stx_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110000:
         ok = gen_stx_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111000:
         ok = gen_stx_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000000:
         ok = gen_ldx_bu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001000:
         ok = gen_ldx_hu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010000:
         ok = gen_ldx_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011000:
         ok = gen_preldx(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100000:
         ok = gen_fldx_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101000:
         ok = gen_fldx_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110000:
         ok = gen_fstx_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111000:
         ok = gen_fstx_d(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00_1110_0001 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 21, 15)) {
      case 0b1000000:
         ok = gen_amswap_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000001:
         ok = gen_amswap_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000010:
         ok = gen_amadd_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000011:
         ok = gen_amadd_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000100:
         ok = gen_amand_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000101:
         ok = gen_amand_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000110:
         ok = gen_amor_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000111:
         ok = gen_amor_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001000:
         ok = gen_amxor_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001001:
         ok = gen_amxor_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001010:
         ok = gen_ammax_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001011:
         ok = gen_ammax_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001100:
         ok = gen_ammin_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001101:
         ok = gen_ammin_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001110:
         ok = gen_ammax_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001111:
         ok = gen_ammax_du(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010000:
         ok = gen_ammin_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010001:
         ok = gen_ammin_du(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010010:
         ok = gen_amswap_db_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010011:
         ok = gen_amswap_db_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010100:
         ok = gen_amadd_db_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010101:
         ok = gen_amadd_db_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010110:
         ok = gen_amand_db_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010111:
         ok = gen_amand_db_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011000:
         ok = gen_amor_db_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011001:
         ok = gen_amor_db_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011010:
         ok = gen_amxor_db_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011011:
         ok = gen_amxor_db_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011100:
         ok = gen_ammax_db_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011101:
         ok = gen_ammax_db_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011110:
         ok = gen_ammin_db_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011111:
         ok = gen_ammin_db_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100000:
         ok = gen_ammax_db_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100001:
         ok = gen_ammax_db_du(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100010:
         ok = gen_ammin_db_wu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100011:
         ok = gen_ammin_db_du(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100100:
         ok = gen_dbar(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100101:
         ok = gen_ibar(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101000:
         ok = gen_fldgt_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101001:
         ok = gen_fldgt_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101010:
         ok = gen_fldle_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101011:
         ok = gen_fldle_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101100:
         ok = gen_fstgt_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101101:
         ok = gen_fstgt_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101110:
         ok = gen_fstle_s(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101111:
         ok = gen_fstle_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110000:
         ok = gen_ldgt_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110001:
         ok = gen_ldgt_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110010:
         ok = gen_ldgt_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110011:
         ok = gen_ldgt_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110100:
         ok = gen_ldle_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110101:
         ok = gen_ldle_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110110:
         ok = gen_ldle_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110111:
         ok = gen_ldle_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111000:
         ok = gen_stgt_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111001:
         ok = gen_stgt_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111010:
         ok = gen_stgt_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111011:
         ok = gen_stgt_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111100:
         ok = gen_stle_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111101:
         ok = gen_stle_h(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111110:
         ok = gen_stle_w(dres, insn, archinfo, abiinfo);
         break;
      case 0b1111111:
         ok = gen_stle_d(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_FCMP_S ( DisResult* dres, UInt insn,
                                              const VexArchInfo* archinfo,
                                              const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (get_cond(insn)) {
      case 0x0:
         ok = gen_fcmp_caf_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x1:
         ok = gen_fcmp_saf_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x2:
         ok = gen_fcmp_clt_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x3:
         ok = gen_fcmp_slt_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x4:
         ok = gen_fcmp_ceq_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x5:
         ok = gen_fcmp_seq_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x6:
         ok = gen_fcmp_cle_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x7:
         ok = gen_fcmp_sle_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x8:
         ok = gen_fcmp_cun_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x9:
         ok = gen_fcmp_sun_s(dres, insn, archinfo, abiinfo);
         break;
      case 0xa:
         ok = gen_fcmp_cult_s(dres, insn, archinfo, abiinfo);
         break;
      case 0xb:
         ok = gen_fcmp_sult_s(dres, insn, archinfo, abiinfo);
         break;
      case 0xc:
         ok = gen_fcmp_cueq_s(dres, insn, archinfo, abiinfo);
         break;
      case 0xd:
         ok = gen_fcmp_sueq_s(dres, insn, archinfo, abiinfo);
         break;
      case 0xe:
         ok = gen_fcmp_cule_s(dres, insn, archinfo, abiinfo);
         break;
      case 0xf:
         ok = gen_fcmp_sule_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x10:
         ok = gen_fcmp_cne_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x11:
         ok = gen_fcmp_sne_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x14:
         ok = gen_fcmp_cor_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x15:
         ok = gen_fcmp_sor_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x18:
         ok = gen_fcmp_cune_s(dres, insn, archinfo, abiinfo);
         break;
      case 0x19:
         ok = gen_fcmp_sune_s(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_FCMP_D ( DisResult* dres, UInt insn,
                                              const VexArchInfo* archinfo,
                                              const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (get_cond(insn)) {
      case 0x0:
         ok = gen_fcmp_caf_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x1:
         ok = gen_fcmp_saf_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x2:
         ok = gen_fcmp_clt_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x3:
         ok = gen_fcmp_slt_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x4:
         ok = gen_fcmp_ceq_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x5:
         ok = gen_fcmp_seq_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x6:
         ok = gen_fcmp_cle_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x7:
         ok = gen_fcmp_sle_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x8:
         ok = gen_fcmp_cun_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x9:
         ok = gen_fcmp_sun_d(dres, insn, archinfo, abiinfo);
         break;
      case 0xa:
         ok = gen_fcmp_cult_d(dres, insn, archinfo, abiinfo);
         break;
      case 0xb:
         ok = gen_fcmp_sult_d(dres, insn, archinfo, abiinfo);
         break;
      case 0xc:
         ok = gen_fcmp_cueq_d(dres, insn, archinfo, abiinfo);
         break;
      case 0xd:
         ok = gen_fcmp_sueq_d(dres, insn, archinfo, abiinfo);
         break;
      case 0xe:
         ok = gen_fcmp_cule_d(dres, insn, archinfo, abiinfo);
         break;
      case 0xf:
         ok = gen_fcmp_sule_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x10:
         ok = gen_fcmp_cne_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x11:
         ok = gen_fcmp_sne_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x14:
         ok = gen_fcmp_cor_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x15:
         ok = gen_fcmp_sor_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x18:
         ok = gen_fcmp_cune_d(dres, insn, archinfo, abiinfo);
         break;
      case 0x19:
         ok = gen_fcmp_sune_d(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_00 ( DisResult* dres, UInt insn,
                                          const VexArchInfo* archinfo,
                                          const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 29, 26)) {
      case 0b0000:
         ok = disInstr_LOONGARCH64_WRK_00_0000(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010:
         switch (SLICE(insn, 25, 20)) {
            case 0b000001:
               ok = gen_fmadd_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b000010:
               ok = gen_fmadd_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b000101:
               ok = gen_fmsub_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b000110:
               ok = gen_fmsub_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b001001:
               ok = gen_fnmadd_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b001010:
               ok = gen_fnmadd_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b001101:
               ok = gen_fnmsub_s(dres, insn, archinfo, abiinfo);
               break;
            case 0b001110:
               ok = gen_fnmsub_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0011:
         switch (SLICE(insn, 25, 20)) {
            case 0b000001:
               if (SLICE(insn, 4, 3) == 0b00) {
                  ok = disInstr_LOONGARCH64_WRK_FCMP_S(dres, insn, archinfo, abiinfo);
               } else {
                  ok = False;
               }
               break;
            case 0b000010:
               if (SLICE(insn, 4, 3) == 0b00) {
                  ok = disInstr_LOONGARCH64_WRK_FCMP_D(dres, insn, archinfo, abiinfo);
               } else {
                  ok = False;
               }
               break;
            case 0b010000:
               if (SLICE(insn, 19, 18) == 0b00) {
                  ok = gen_fsel(dres, insn, archinfo, abiinfo);
               } else {
                  ok = False;
               }
               break;
            case 0b010101:
               ok = gen_vshuf_b(dres, insn, archinfo, abiinfo); break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0100:
         ok = gen_addu16i_d(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101:
         if (SLICE(insn, 25, 25) == 0) {
            ok = gen_lu12i_w(dres, insn, archinfo, abiinfo);
         } else {
            ok = gen_lu32i_d(dres, insn, archinfo, abiinfo);
         }
         break;
      case 0b0110:
         if (SLICE(insn, 25, 25) == 0) {
            ok = gen_pcaddi(dres, insn, archinfo, abiinfo);
         } else {
            ok = gen_pcalau12i(dres, insn, archinfo, abiinfo);
         }
         break;
      case 0b0111:
         if (SLICE(insn, 25, 25) == 0) {
            ok = gen_pcaddu12i(dres, insn, archinfo, abiinfo);
         } else {
            ok = gen_pcaddu18i(dres, insn, archinfo, abiinfo);
         }
         break;
      case 0b1000:
         switch (SLICE(insn, 25, 24)) {
            case 0b00:
               ok = gen_ll_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b01:
               ok = gen_sc_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b10:
               ok = gen_ll_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b11:
               ok = gen_sc_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b1001:
         switch (SLICE(insn, 25, 24)) {
            case 0b00:
               ok = gen_ldptr_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b01:
               ok = gen_stptr_w(dres, insn, archinfo, abiinfo);
               break;
            case 0b10:
               ok = gen_ldptr_d(dres, insn, archinfo, abiinfo);
               break;
            case 0b11:
               ok = gen_stptr_d(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b1010:
         ok = disInstr_LOONGARCH64_WRK_00_1010(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011:
         ok = disInstr_LOONGARCH64_WRK_00_1011(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100:
         ok = disInstr_LOONGARCH64_WRK_00_1100(dres, insn, archinfo, abiinfo);
         break;
      case 0b1110:
         switch (SLICE(insn, 25, 22)) {
            case 0b0000:
               ok = disInstr_LOONGARCH64_WRK_00_1110_0000(dres, insn, archinfo, abiinfo);
               break;
            case 0b0001:
               ok = disInstr_LOONGARCH64_WRK_00_1110_0001(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1100_0000 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 21, 18)) {
      case 0b0000: case 0b0001:
         ok = gen_vcmp_integer(dres, insn, archinfo, abiinfo); break;
      case 0b0010:
         if (SLICE(insn, 17, 17) == 0b0)
            ok = gen_vcmp_integer(dres, insn, archinfo, abiinfo);
         else
            ok = False;
         break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1100_0001 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 21, 18)) {
      case 0b1100: case 0b1101:
         ok = gen_vmax_vmin(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1100_0100 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 21, 17)) {
      case 0b01011: case 0b01100:
      case 0b01101: case 0b01110:
      case 0b01111: case 0b10000:
         ok = gen_evod(dres, insn, archinfo, abiinfo); break;
      case 0b10011: case 0b10100:
         ok = gen_logical_v(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1100_1010_0111 ( DisResult* dres, UInt insn,
                                                         const VexArchInfo* archinfo,
                                                         const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 17, 14)) {
      case 0b0001:
         ok = gen_vmsk(dres, insn, archinfo, abiinfo); break;
      case 0b0010:
         ok = gen_vset(dres, insn, archinfo, abiinfo); break;
      case 0b1100:
         ok = gen_vreplgr2vr(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1100_1010 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 21, 18)) {
      case 0b0111:
         ok = disInstr_LOONGARCH64_WRK_01_1100_1010_0111(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1100_1100 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 21, 18)) {
      case 0b0100: case 0b0101: case 0b0110:
         ok = gen_vbiti(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1100 ( DisResult* dres, UInt insn,
                                               const VexArchInfo* archinfo,
                                               const VexAbiInfo*  abiinfo )
{
   Bool ok;

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LSX)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   switch (SLICE(insn, 25, 22)) {
      case 0b0000:
         ok = disInstr_LOONGARCH64_WRK_01_1100_0000(dres, insn, archinfo, abiinfo); break;
      case 0b0001:
         ok = disInstr_LOONGARCH64_WRK_01_1100_0001(dres, insn, archinfo, abiinfo); break;
      case 0b0100:
         ok = disInstr_LOONGARCH64_WRK_01_1100_0100(dres, insn, archinfo, abiinfo); break;
      case 0b1010:
         ok = disInstr_LOONGARCH64_WRK_01_1100_1010(dres, insn, archinfo, abiinfo); break;
      case 0b1100:
         ok = disInstr_LOONGARCH64_WRK_01_1100_1100(dres, insn, archinfo, abiinfo); break;
      default:
         ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1101_1010_0111 ( DisResult* dres, UInt insn,
                                                         const VexArchInfo* archinfo,
                                                         const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 17, 14)) {
      case 0b0001:
         ok = gen_xvmsk(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1101_1010 ( DisResult* dres, UInt insn,
                                                    const VexArchInfo* archinfo,
                                                    const VexAbiInfo*  abiinfo )
{
   Bool ok;

   switch (SLICE(insn, 21, 18)) {
      case 0b0111:
         ok = disInstr_LOONGARCH64_WRK_01_1101_1010_0111(dres, insn, archinfo, abiinfo); break;
      default: ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01_1101 ( DisResult* dres, UInt insn,
                                               const VexArchInfo* archinfo,
                                               const VexAbiInfo*  abiinfo )
{
   Bool ok;

   if (!(archinfo->hwcaps & VEX_HWCAPS_LOONGARCH_LASX)) {
      dres->jk_StopHere = Ijk_SigILL;
      dres->whatNext    = Dis_StopHere;
      return True;
   }

   switch (SLICE(insn, 25, 22)) {
      case 0b1010:
         ok = disInstr_LOONGARCH64_WRK_01_1101_1010(dres, insn, archinfo, abiinfo); break;
      default:
         ok = False; break;
   }

   return ok;
}

static Bool disInstr_LOONGARCH64_WRK_01 ( DisResult* dres, UInt insn,
                                          const VexArchInfo* archinfo,
                                          const VexAbiInfo*  abiinfo )
{
   Bool ok;
   switch (SLICE(insn, 29, 26)) {
      case 0b0000:
         ok = gen_beqz(dres, insn, archinfo, abiinfo);
         break;
      case 0b0001:
         ok = gen_bnez(dres, insn, archinfo, abiinfo);
         break;
      case 0b0010:
         switch (SLICE(insn, 9, 8)) {
            case 0b00:
               ok = gen_bceqz(dres, insn, archinfo, abiinfo);
               break;
            case 0b01:
               ok = gen_bcnez(dres, insn, archinfo, abiinfo);
               break;
            default:
               ok = False;
               break;
         }
         break;
      case 0b0011:
         ok = gen_jirl(dres, insn, archinfo, abiinfo);
         break;
      case 0b0100:
         ok = gen_b(dres, insn, archinfo, abiinfo);
         break;
      case 0b0101:
         ok = gen_bl(dres, insn, archinfo, abiinfo);
         break;
      case 0b0110:
         ok = gen_beq(dres, insn, archinfo, abiinfo);
         break;
      case 0b0111:
         ok = gen_bne(dres, insn, archinfo, abiinfo);
         break;
      case 0b1000:
         ok = gen_blt(dres, insn, archinfo, abiinfo);
         break;
      case 0b1001:
         ok = gen_bge(dres, insn, archinfo, abiinfo);
         break;
      case 0b1010:
         ok = gen_bltu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1011:
         ok = gen_bgeu(dres, insn, archinfo, abiinfo);
         break;
      case 0b1100:
         ok = disInstr_LOONGARCH64_WRK_01_1100(dres, insn, archinfo, abiinfo);
         break;
      case 0b1101:
         ok = disInstr_LOONGARCH64_WRK_01_1101(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }
   return ok;
}

static Bool disInstr_LOONGARCH64_WRK ( /*MB_OUT*/DisResult* dres,
                                       const UChar* guest_instr,
                                       const VexArchInfo* archinfo,
                                       const VexAbiInfo*  abiinfo,
                                       Bool sigill_diag )
{
   /* Set result defaults. */
   dres->whatNext    = Dis_Continue;
   dres->len         = 4;
   dres->jk_StopHere = Ijk_INVALID;
   dres->hint        = Dis_HintNone;

   /* At least this is simple on LOONGARCH64: insns are all 4 bytes long,
      and 4-aligned.  So just fish the whole thing out of memory right now
      and have done. */
   UInt insn = getUInt(guest_instr);
   DIP("\t0x%llx:\t0x%08x\t", (Addr64)guest_PC_curr_instr, insn);
   vassert((guest_PC_curr_instr & 3ULL) == 0);

   /* Spot "Special" instructions (see comment at top of file). */
   Bool ok = disInstr_LOONGARCH64_WRK_special(dres, guest_instr);
   if (ok)
      return ok;

   /* Main LOONGARCH64 instruction decoder starts here. */
   switch (SLICE(insn, 31, 30)) {
      case 0b00:
         ok = disInstr_LOONGARCH64_WRK_00(dres, insn, archinfo, abiinfo);
         break;
      case 0b01:
         ok = disInstr_LOONGARCH64_WRK_01(dres, insn, archinfo, abiinfo);
         break;
      default:
         ok = False;
         break;
   }

   /* If the next-level down decoders failed, make sure |dres| didn't
      get changed. */
   if (!ok) {
      vassert(dres->whatNext    == Dis_Continue);
      vassert(dres->len         == 4);
      vassert(dres->jk_StopHere == Ijk_INVALID);
   }
   return ok;
}


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_LOONGARCH64 ( IRSB*              irsb_IN,
                                 const UChar*       guest_code_IN,
                                 Long               delta_IN,
                                 Addr               guest_IP,
                                 VexArch            guest_arch,
                                 const VexArchInfo* archinfo,
                                 const VexAbiInfo*  abiinfo,
                                 VexEndness         host_endness_IN,
                                 Bool               sigill_diag_IN )
{
   DisResult dres;
   vex_bzero(&dres, sizeof(dres));

   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchLOONGARCH64);

   irsb                = irsb_IN;
   host_endness        = host_endness_IN;
   guest_PC_curr_instr = (Addr64)guest_IP;

   /* Try to decode */
   Bool ok = disInstr_LOONGARCH64_WRK(&dres,
                                      &guest_code_IN[delta_IN],
                                      archinfo, abiinfo, sigill_diag_IN);

   if (ok) {
      /* All decode successes end up here. */
      vassert(dres.len == 4 || dres.len == 20);
      switch (dres.whatNext) {
         case Dis_Continue:
            putPC(mkU64(dres.len + guest_PC_curr_instr));
            break;
         case Dis_StopHere:
            break;
         default:
            vassert(0);
            break;
      }
      DIP("\n");
   } else {
      /* All decode failures end up here. */
      if (sigill_diag_IN) {
         Int   i, j;
         UChar buf[64];
         UInt  insn = getUInt(&guest_code_IN[delta_IN]);
         vex_bzero(buf, sizeof(buf));
         for (i = j = 0; i < 32; i++) {
            if (i > 0 && (i & 3) == 0)
               buf[j++] = ' ';
            buf[j++] = (insn & (1 << (31 - i))) ? '1' : '0';
         }
         vex_printf("disInstr(loongarch64): unhandled instruction 0x%08x\n", insn);
         vex_printf("disInstr(loongarch64): %s\n", buf);
      }

      /* Tell the dispatcher that this insn cannot be decoded, and so
         has not been executed, and (is currently) the next to be
         executed.  PC should be up-to-date since it is made so at the
         start of each insn, but nevertheless be paranoid and update
         it again right now. */
      putPC(mkU64(guest_PC_curr_instr));
      dres.len         = 0;
      dres.whatNext    = Dis_StopHere;
      dres.jk_StopHere = Ijk_NoDecode;
   }

   return dres;
}


/*--------------------------------------------------------------------*/
/*--- end                                 guest_loongarch64_toIR.c ---*/
/*--------------------------------------------------------------------*/
