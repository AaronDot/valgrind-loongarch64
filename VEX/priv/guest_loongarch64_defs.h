
/*---------------------------------------------------------------*/
/*--- begin                          guest_loongarch64_defs.h ---*/
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

/* Only to be used within the guest-loongarch64 directory. */

#ifndef __VEX_GUEST_LOONGARCH64_DEFS_H
#define __VEX_GUEST_LOONGARCH64_DEFS_H

#include "libvex_basictypes.h"
#include "guest_generic_bb_to_IR.h"  /* DisResult */


/*---------------------------------------------------------*/
/*--- loongarch64 to IR conversion                      ---*/
/*---------------------------------------------------------*/

/* Convert one LOONGARCH64 insn to IR.  See the type DisOneInstrFn in
   guest_generic_bb_to_IR.h. */
extern DisResult disInstr_LOONGARCH64 ( IRSB*              irsb_IN,
                                        const UChar*       guest_code_IN,
                                        Long               delta,
                                        Addr               guest_IP,
                                        VexArch            guest_arch,
                                        const VexArchInfo* archinfo,
                                        const VexAbiInfo*  abiinfo,
                                        VexEndness         host_endness_IN,
                                        Bool               sigill_diag_IN );

/* Used by the optimiser to specialise calls to helpers. */
extern IRExpr* guest_loongarch64_spechelper ( const HChar* function_name,
                                              IRExpr**     args,
                                              IRStmt**     precedingStmts,
                                              Int          n_precedingStmts );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern Bool guest_loongarch64_state_requires_precise_mem_exns ( Int minoff,
                                                                Int maxoff,
                                                                VexRegisterUpdates pxControl );

extern VexGuestLayout loongarch64Guest_layout;


/*---------------------------------------------------------*/
/*--- loongarch64 guest helpers                         ---*/
/*---------------------------------------------------------*/

enum fpop {
   FADD_S, FADD_D, FSUB_S, FSUB_D,
   FMUL_S, FMUL_D, FDIV_S, FDIV_D,
   FMADD_S, FMADD_D, FMSUB_S, FMSUB_D,
   FNMADD_S, FNMADD_D, FNMSUB_S, FNMSUB_D,
   FMAX_S, FMAX_D, FMIN_S, FMIN_D,
   FMAXA_S, FMAXA_D, FMINA_S, FMINA_D,
   FABS_S, FABS_D, FNEG_S, FNEG_D,
   FSQRT_S, FSQRT_D,
   FRECIP_S, FRECIP_D,
   FRSQRT_S, FRSQRT_D,
   FSCALEB_S, FSCALEB_D,
   FLOGB_S, FLOGB_D,
   FCMP_CAF_S, FCMP_CAF_D, FCMP_SAF_S, FCMP_SAF_D,
   FCMP_CLT_S, FCMP_CLT_D, FCMP_SLT_S, FCMP_SLT_D,
   FCMP_CEQ_S, FCMP_CEQ_D, FCMP_SEQ_S, FCMP_SEQ_D,
   FCMP_CLE_S, FCMP_CLE_D, FCMP_SLE_S, FCMP_SLE_D,
   FCMP_CUN_S, FCMP_CUN_D, FCMP_SUN_S, FCMP_SUN_D,
   FCMP_CULT_S, FCMP_CULT_D, FCMP_SULT_S, FCMP_SULT_D,
   FCMP_CUEQ_S, FCMP_CUEQ_D, FCMP_SUEQ_S, FCMP_SUEQ_D,
   FCMP_CULE_S, FCMP_CULE_D, FCMP_SULE_S, FCMP_SULE_D,
   FCMP_CNE_S, FCMP_CNE_D, FCMP_SNE_S, FCMP_SNE_D,
   FCMP_COR_S, FCMP_COR_D, FCMP_SOR_S, FCMP_SOR_D,
   FCMP_CUNE_S, FCMP_CUNE_D, FCMP_SUNE_S, FCMP_SUNE_D,
   FCVT_S_D, FCVT_D_S,
   FTINTRM_W_S, FTINTRM_W_D, FTINTRM_L_S, FTINTRM_L_D,
   FTINTRP_W_S, FTINTRP_W_D, FTINTRP_L_S, FTINTRP_L_D,
   FTINTRZ_W_S, FTINTRZ_W_D, FTINTRZ_L_S, FTINTRZ_L_D,
   FTINTRNE_W_S, FTINTRNE_W_D, FTINTRNE_L_S, FTINTRNE_L_D,
   FTINT_W_S, FTINT_W_D, FTINT_L_S, FTINT_L_D,
   FFINT_S_W, FFINT_D_W, FFINT_S_L, FFINT_D_L,
   FRINT_S, FRINT_D
};

enum vfpop {
   VFADD_S, VFADD_D, VFSUB_S, VFSUB_D,
   VFMUL_S, VFMUL_D, VFDIV_S, VFDIV_D,
   VFMADD_S, VFMADD_D, VFMSUB_S, VFMSUB_D,
   VFNMADD_S, VFNMADD_D, VFNMSUB_S, VFNMSUB_D,
   VFMAX_S, VFMAX_D, VFMIN_S, VFMIN_D,
   VFMAXA_S, VFMAXA_D, VFMINA_S, VFMINA_D,
   VFLOGB_S, VFLOGB_D,
   VFSQRT_S, VFSQRT_D,
   VFRECIP_S, VFRECIP_D,
   VFRSQRT_S, VFRSQRT_D,
   VFCMP_CAF_S, VFCMP_CAF_D, VFCMP_SAF_S, VFCMP_SAF_D,
   VFCMP_CLT_S, VFCMP_CLT_D, VFCMP_SLT_S, VFCMP_SLT_D,
   VFCMP_CEQ_S, VFCMP_CEQ_D, VFCMP_SEQ_S, VFCMP_SEQ_D,
   VFCMP_CLE_S, VFCMP_CLE_D, VFCMP_SLE_S, VFCMP_SLE_D,
   VFCMP_CUN_S, VFCMP_CULT_D, VFCMP_SULT_S, VFCMP_SULT_D,
   VFCMP_CUEQ_S, VFCMP_CUEQ_D, VFCMP_SUEQ_S, VFCMP_SUEQ_D,
   VFCMP_CULE_S, VFCMP_CULE_D, VFCMP_SULE_S, VFCMP_SULE_D,
   VFCMP_CNE_S, VFCMP_CNE_D, VFCMP_SNE_S, VFCMP_SNE_D,
   VFCMP_COR_S, VFCMP_COR_D, VFCMP_SOR_S, VFCMP_SOR_D,
   VFCMP_CUNE_S, VFCMP_CUNE_D, VFCMP_SUNE_S, VFCMP_SUNE_D,
   VFCVTL_S_H, VFCVTL_D_S, VFCVTH_S_H, VFCVTH_D_S, VFCVT_S_H, VFCVT_D_S,
   VFRINTRNE_S, VFRINTRNE_D, VFRINTRZ_S, VFRINTRZ_D,
   VFRINTRP_S, VFRINTRP_D, VFRINTRM_S, VFRINTRM_D,
   VFRINT_S, VFRINT_D,
   VFTINTRNE_W_S, VFTINTRNE_W_D, VFTINTRNEL_L_S, VFTINTRNEH_L_S, VFTINTRNE_L_D,
   VFTINTRZ_W_S, VFTINTRZ_WU_S, VFTINTRZ_W_D, VFTINTRZL_L_S, VFTINTRZH_L_S, VFTINTRZ_L_D, VFTINTRZ_LU_D,
   VFTINTRP_W_S, VFTINTRP_W_D, VFTINTRPL_L_S, VFTINTRPH_L_S, VFTINTRP_L_D,
   VFTINTRM_W_S, VFTINTRM_W_D, VFTINTRML_L_S, VFTINTRMH_L_S, VFTINTRM_L_D,
   VFTINT_W_S, VFTINT_WU_S, VFTINT_W_D, VFTINTL_L_S, VFTINTH_L_S, VFTINT_L_D, VFTINT_LU_D,
   VFFINT_S_W, VFFINT_S_WU, VFFINTL_D_W, VFFINTH_D_W, VFFINT_S_L, VFFINT_D_L, VFFINT_D_LU,
   XVFADD_S, XVFADD_D, XVFSUB_S, XVFSUB_D,
   XVFMUL_S, XVFMUL_D, XVFDIV_S, XVFDIV_D,
   XVFMADD_S, XVFMADD_D, XVFMSUB_S, XVFMSUB_D,
   XVFNMADD_S, XVFNMADD_D, XVFNMSUB_S, XVFNMSUB_D,
   XVFMAX_S, XVFMAX_D, XVFMIN_S, XVFMIN_D,
   XVFMAXA_S, XVFMAXA_D, XVFMINA_S, XVFMINA_D,
   XVFLOGB_S, XVFLOGB_D,
   XVFSQRT_S, XVFSQRT_D,
   XVFRECIP_S, XVFRECIP_D,
   XVFRSQRT_S, XVFRSQRT_D,
   XVFCMP_CAF_S, XVFCMP_CAF_D, XVFCMP_SAF_S, XVFCMP_SAF_D,
   XVFCMP_CLT_S, XVFCMP_CLT_D, XVFCMP_SLT_S, XVFCMP_SLT_D,
   XVFCMP_CEQ_S, XVFCMP_CEQ_D, XVFCMP_SEQ_S, XVFCMP_SEQ_D,
   XVFCMP_CLE_S, XVFCMP_CLE_D, XVFCMP_SLE_S, XVFCMP_SLE_D,
   XVFCMP_CUN_S, XVFCMP_CULT_D, XVFCMP_SULT_S, XVFCMP_SULT_D,
   XVFCMP_CUEQ_S, XVFCMP_CUEQ_D, XVFCMP_SUEQ_S, XVFCMP_SUEQ_D,
   XVFCMP_CULE_S, XVFCMP_CULE_D, XVFCMP_SULE_S, XVFCMP_SULE_D,
   XVFCMP_CNE_S, XVFCMP_CNE_D, XVFCMP_SNE_S, XVFCMP_SNE_D,
   XVFCMP_COR_S, XVFCMP_COR_D, XVFCMP_SOR_S, XVFCMP_SOR_D,
   XVFCMP_CUNE_S, XVFCMP_CUNE_D, XVFCMP_SUNE_S, XVFCMP_SUNE_D,
   XVFCVTL_S_H, XVFCVTL_D_S, XVFCVTH_S_H, XVFCVTH_D_S, XVFCVT_S_H, XVFCVT_D_S,
   XVFRINTRNE_S, XVFRINTRNE_D, XVFRINTRZ_S, XVFRINTRZ_D,
   XVFRINTRP_S, XVFRINTRP_D, XVFRINTRM_S, XVFRINTRM_D,
   XVFRINT_S, XVFRINT_D,
   XVFTINTRNE_W_S, XVFTINTRNE_W_D, XVFTINTRNEL_L_S, XVFTINTRNEH_L_S, XVFTINTRNE_L_D,
   XVFTINTRZ_W_S, XVFTINTRZ_WU_S, XVFTINTRZ_W_D, XVFTINTRZL_L_S, XVFTINTRZH_L_S, XVFTINTRZ_L_D, XVFTINTRZ_LU_D,
   XVFTINTRP_W_S, XVFTINTRP_W_D, XVFTINTRPL_L_S, XVFTINTRPH_L_S, XVFTINTRP_L_D,
   XVFTINTRM_W_S, XVFTINTRM_W_D, XVFTINTRML_L_S, XVFTINTRMH_L_S, XVFTINTRM_L_D,
   XVFTINT_W_S, XVFTINT_WU_S, XVFTINT_W_D, XVFTINTL_L_S, XVFTINTH_L_S, XVFTINT_L_D, XVFTINT_LU_D,
   XVFFINT_S_W, XVFFINT_S_WU, XVFFINTL_D_W, XVFFINTH_D_W, XVFFINT_S_L, XVFFINT_D_L, XVFFINT_D_LU
};

extern ULong loongarch64_calculate_cpucfg      ( ULong src );
extern ULong loongarch64_calculate_revb_2h     ( ULong src );
extern ULong loongarch64_calculate_revb_4h     ( ULong src );
extern ULong loongarch64_calculate_revb_2w     ( ULong src );
extern ULong loongarch64_calculate_revb_d      ( ULong src );
extern ULong loongarch64_calculate_revh_2w     ( ULong src );
extern ULong loongarch64_calculate_revh_d      ( ULong src );
extern ULong loongarch64_calculate_bitrev_4b   ( ULong src );
extern ULong loongarch64_calculate_bitrev_8b   ( ULong src );
extern ULong loongarch64_calculate_bitrev_w    ( ULong src );
extern ULong loongarch64_calculate_bitrev_d    ( ULong src );
extern ULong loongarch64_calculate_crc         ( ULong old, ULong msg, ULong len );
extern ULong loongarch64_calculate_crcc        ( ULong old, ULong msg, ULong len );
extern ULong loongarch64_calculate_fclass_s    ( ULong src );
extern ULong loongarch64_calculate_fclass_d    ( ULong src );
extern ULong loongarch64_calculate_FCSR        ( enum fpop op, ULong src1,
                                                 ULong src2, ULong src3 );
extern ULong loongarch64_calculate_VFCSR       ( enum vfpop op,
                                                 ULong v1Hi, ULong v1Lo,
                                                 ULong v2Hi, ULong v2Lo,
                                                 ULong v3Hi, ULong v3Lo );
extern ULong loongarch64_calculate_XVFCSR      ( enum vfpop op,
                                                 ULong v1_0, ULong v1_1, ULong v1_2, ULong v1_3,
                                                 ULong v2_0, ULong v2_1, ULong v2_2, ULong v2_3,
                                                 ULong v3_0, ULong v3_1, ULong v3_2, ULong v3_3 );
extern ULong loongarch64_calculate_negative_id ( ULong insSz, ULong sHi, ULong sLo );

#endif /* ndef __VEX_GUEST_LOONGARCH64_DEFS_H */


/*---------------------------------------------------------------*/
/*--- end                            guest_loongarch64_defs.h ---*/
/*---------------------------------------------------------------*/
