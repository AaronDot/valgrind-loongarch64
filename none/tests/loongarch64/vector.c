#include <stdio.h>

#define NUM 32

unsigned long long mem[32] = {
   0x0000000000000000ull, 0x0000000000000000ull,
   0xffffffffffffffffull, 0xffffffffffffffffull,
   0x0000000080000000ull, 0x8000000000000000ull,
   0x8000800080008000ull, 0x7fff7fff7fff7fffull,
   0x8080808080808080ull, 0x8080808080808080ull,
   0x7070707070707070ull, 0x7070707070707070ull,
   0x7f7f7f7f7f7f7f7full, 0x7f7f7f7f7f7f7f7full,
   0x0706050403020100ull, 0x0f0e0d0c0b0a0908ull,
   0x77665544332211ffull, 0xeeddccbbaa998877ull,
   0x0000000000000001ull, 0x00000000000001ffull,
   0x0000000000000001ull, 0x00000000000000ffull,
   0xffffffffffffffffull, 0x0000000000000000ull,
   0x0000000100000001ull, 0x0000000100000001ull,
   0x1234567890abcdefull, 0xfedbca9876543210ull,
   0x0403020114131211ull, 0x2423222134333231ull,
   0x8483828194939291ull, 0xa4a3a2a1b4b3b2b1ull,
};

   //0x3736353433323130ull, 0x3f3e3d3c3b3a3938ull, ->ok
   //0x4746454443424140ull, 0x4f4e4d4c4b4a4948ull, ->0
   //0x5756555453525150ull, 0x5f5e5d5c5b5a5958ull, ->0
   //0x6766656463626160ull, 0x6f6e6d6c6b6a6968ull, ->0
   //0x7776757473727170ull, 0x7f7e7d7c7b7a7978ull, ->0
   //0x8786858483828180ull, 0x8f8e8d8c8b8a8988ull, ->0
   //0x1716151413121110ull, 0x1f1e1d1c1b1a1918ull,
unsigned long long out[2];

unsigned long long fpout[2];

unsigned int fcsr;

unsigned int  flt  = 0;
unsigned int  flto = 0;
unsigned long long dbl = 0;

unsigned int reg;

unsigned long long datad[] = {
   0xfff0000000000000, 0x3fe0000000000000, // -inf, 0.5
   0xc3e0000020000000, 0xc005395810624dd3, // -9223373136366403584, -2.653
   0x8000006000000000, 0x4085e5999999999a, // -1e-49, 700.7
   0x8000000000000000, 0xc088f8f5c28f5c29, // -0, -799.12
   0x0000000000000000, 0x407b13ae147ae148, // +0, 433.23
   0x0000006000000000, 0xbfe6666666666666, // 1e-49, -0.7
   0x4141a828c51eb852, 0x3ff199999999999a, // 2314321.54, 1.1
   0x7ff0000000000000, 0xc1b59e0837b33333, // +inf, -362678327.7
   0x4082000000000000, 0x40e6252666666666, // 576.0, 45353.2
   0x4082000000000000, 0x4082000000000000, // 576.0, 576.0
   0x7ff0000000006000, 0x40e6252666666666, // qnan, 45353.2
   0x7ff8000000006000, 0x4082000000000000  // snan, 576.0
};

unsigned long long dataf[] = {
   0xff8000003f000000, // -inf, 0.5
   0xcf000001c029cac1, // -2147483904, -2.653
   0x80500000442f2ccd, // -sub, 700.7
   0x80000000c447c7ae, // -0, -799.12
   0x0000000043d89d71, // +0, 433.23
   0x00500000bf333333, // sub, -0.7
   0x46b4a3143f8ccccd, // 23121.54, 1.1
   0x7f800000c70dabb3, // +inf, -36267.7
   0xc029cac1c791bfa9, // -2.653, -74623.321
   0x44100000458db99a, // 576.0, 4535.2
   0x4410000044100000, // 576.0, 576.0
   0x7f800060458db99a, // qnan, 4535.2
   0x7fc0006044100000, // snan, 576.0
};

unsigned long long data16[] = {
   0x80006179fc00c14e,
   0xfc00c14efc003800,
   0x7e005ec57fffe23e,
   0x8000e23effff6179,
   0x75a53c660004b99a,
   0x0000b99a00005ec5,
   0x7dff6c6e7c00f86d,
   0x7c00f86d75a53c66,
   0xc14efc007dff6080,
   0x7dff60807dff6c6e,
   0x800100013c66f5a5,
   0x3c66f5a5c14efc00,
};

#define EPS 0.000001

union {
   unsigned long long i[2];
   double d[2];
   float f[4];
} frsqrt_out, frsqrt_exp;

typedef enum {
   TO_NEAREST = 0,
   TO_ZERO,
   TO_PLUS_INFINITY,
   TO_MINUS_INFINITY
} round_mode_t;

static inline void set_fcsr(round_mode_t mode)
{
   __asm__ __volatile__("movgr2fcsr $r0, %0" : : "r" (mode << 8));

   const char *round_mode_name[] = { "near", "zero", "+inf", "-inf" };
   printf("roundig mode: %s\n", round_mode_name[mode]);
}

#define TESTINST_LOAD_CR(insn, VJ, offs) \
   {                                                      \
      int fcc;                              \
      __asm__ __volatile__(                               \
        "vld $"#VJ", %1, "#offs"\n"                      \
        insn " $fcc0, $"#VJ"  \n"       \
        "movcf2gr %0, $fcc0    \n\t"       \
	 : "=r" (fcc)                                     \
	 : "r" (mem)                                      \
         : "$fcc0", "memory");              \
         printf(insn " 0, "#VJ": "               \
		"fcc0: %d, "#VJ": %016llx%016llx \n",   \
	        fcc, mem[offs / 8 + 1],  mem[offs / 8]);\
   }

#define TESTINST_LOAD_RI(insn, addr, offs, VD) \
   {                                            \
      __asm__ __volatile__(                     \
	insn " $"#VD", %1, "#offs"\n"           \
	"vst $"#VD", %0\n"                      \
	 : "=m" (out)                           \
	 : "r" (mem)                            \
         : "memory");                           \
         printf(insn" "#VD": %llx%llx  rj: %llx%llx\n",   \
	       out[1], out[0], mem[offs / 8 + 1],  mem[offs / 8]);\
   }

#define TESTINST_LOAD_RR(insn, VD, VJ, offs) \
   {                                            \
      __asm__ __volatile__(                     \
        "vld $"#VJ", %1, "#offs"\n"                      \
	insn " $"#VD", $"#VJ"\n"                  \
	"vst $"#VD", %0\n"                      \
	 : "=m" (out)                           \
	 : "r" (mem)                            \
         : "memory");                           \
         printf(insn" "#VD": %016llx%016llx  rj: %016llx%016llx\n",   \
	       out[1], out[0], mem[offs / 8 + 1],  mem[offs / 8]);\
   }

#define TESTINST_LOAD_RRR(insn, VD, VJ, VK, offs1, offs2) \
   {                                                      \
      __asm__ __volatile__(                               \
        "vld $"#VJ", %1, "#offs1"\n"                      \
        "vld $"#VK", %1, "#offs2"\n"                      \
	insn " $"#VD", $"#VJ", $"#VK"\n"                  \
	"vst $"#VD", %0\n"                                \
	 : "=m" (out)                                     \
	 : "r" (mem)                                      \
         : "memory");                                     \
         printf(insn " "#VD", "#VJ", "#VK": "               \
		""#VD": %016llx%016llx "#VJ": %016llx%016llx, "#VK": %016llx%016llx\n",   \
	        out[0], out[1], mem[offs1 / 8 ],  mem[offs1 / 8 + 1], mem[offs2 / 8 ],  mem[offs2 / 8+1]);\
   }

#define TESTINST_LOAD_RRRR(insn, VD, VJ, VK, VA, offs1, offs2, offs3) \
   {                                                      \
      __asm__ __volatile__(                               \
        "vld $"#VJ", %1, "#offs1"\n"                      \
        "vld $"#VK", %1, "#offs2"\n"                      \
        "vld $"#VA", %1, "#offs3"\n"                      \
	insn " $"#VD", $"#VJ", $"#VK", $"#VA"\n"                  \
	"vst $"#VD", %0\n"                                \
	 : "=m" (out)                                     \
	 : "r" (mem)                                      \
         : "memory");                                     \
         printf(insn " "#VD", "#VJ", "#VK", "#VA": "               \
		""#VD": %016llx%016llx "#VJ": %016llx%016llx, "#VK": %016llx%016llx\n, "#VA": %016llx%016llx\n",   \
	        out[1], out[0], mem[offs1 / 8 + 1],  mem[offs1 / 8], mem[offs2 / 8 + 1],  mem[offs2 / 8 ], mem[offs3 / 8 + 1],  mem[offs3 / 8]);\
   }

#define TESTINST_LOAD_RRU(insn, VD, RJ, offs1, offs2) \
   {                                            \
     unsigned long rj; \
      __asm__ __volatile__(                     \
        "ld.d $"#RJ", %2, "#offs1"\n"                      \
	insn " $"#VD", $"#RJ", "#offs2"\n"                  \
	"move %1, $"#RJ"      \n\t" \
	"vst $"#VD", %0\n"                      \
	 : "=m" (out), "=r" (rj)                           \
	 : "r" (mem)                            \
         : "memory");                           \
         printf(insn" "#VD": %016llx%016llx  "#RJ": %08lx \n",   \
	       out[1], out[0] , rj);\
   }

#define TESTINST_FFF_S(insn, VD, VJ, VK, offs1, offs2)        \
   {                                        \
      __asm__ __volatile__(                 \
         "vld $"#VJ", %2, "#offs1"\n"       \
         "vld $"#VK", %2, "#offs2"\n"       \
         insn " $"#VD", $"#VJ", $"#VK"   \n"          \
         "vst $"#VD", %0\n"                      \
         "movfcsr2gr %1, $r0 \n\t"          \
	      : "=m" (fpout), "=r" (fcsr)          \
	      : "r" (datad)                      \
         : "memory");                       \
         printf(insn" $"#VD", $"#VJ", $"#VK" "#VD":"                  \
          " %016llx%016llx  "#VJ": %016llx%016llx  "#VK": "            \
          "%016llx%016llx fcsr %08x\n", fpout[1], fpout[0],              \
          datad[offs1 / 8 + 1], datad[offs1 / 8],                    \
          datad[offs2 / 8 + 1], datad[offs2 / 8], fcsr);           \
   }

static inline void show(void)
{
   int i;
   printf("memory block:\n");
   for (i = 0; i < NUM; i++)
      printf("0x%lx:\t%#018llx\n", i * sizeof(unsigned long long), mem[i]);
}


void test(void)
{
   show();

#if 0
   int tmp = 0x1;

   __asm__ __volatile__ (
	"vldi $vr1, -3648\n"
	"vst $vr1, %0\n"
	 : "=m" (out)
     );

     printf("tmp = %b, vr1: %016llx%016llx\n", tmp, out[1], out[0]);
#endif

   /* ---------------- ld.h rd, rj, si12 ---------------- */
//   printf("test vld: \n");
//   TESTINST_LOAD_RI("vld", mem, 0x30, vr1);
//
//   printf("test vilvl.h: ");
//   TESTINST_LOAD_RRR("vilvl.h", vr2, vr3, vr4, 0x40, 0x30);
//
//   printf("test vreplgr2vr.b: ");
//   TESTINST_LOAD_RRK("vreplgr2vr.b", vr2, r28, 0x50);
//
//   printf("test vmax.bu: ");
//   TESTINST_LOAD_RRR("vmax.bu", vr2, vr3, vr4, 0x40, 0x30);
//
//   printf("test vseteqz.v: ");
//   TESTINST_LOAD_CR("vseteqz.v", vr2, 0x0);

   printf("test vadd.b: ");
   TESTINST_LOAD_RRR("vadd.b", vr2, vr3, vr4, 0x40, 0x30);

   printf("test vmskgez.b: ");
   TESTINST_LOAD_RR("vmskgez.b", vr2, vr3, 0x10);

   printf("test vmsknz.b: ");
   TESTINST_LOAD_RR("vmsknz.b", vr2, vr3, 0x70);

   printf("test vshuf.b: ");
   TESTINST_LOAD_RRRR("vshuf.b", vr2, vr3, vr4, vr5, 0x80, 0x70, 0xe0);

   printf("test vinsgr2vr.b: ");
   TESTINST_LOAD_RRU("vinsgr2vr.b", vr2, r1, 0x80, 0);

   printf("test vfadd.s: TO_NEAREST\n");
    set_fcsr(TO_NEAREST);
    TESTINST_FFF_S("vfadd.s", vr2, vr3, vr4, 0, 0);
 
   printf("test vfadd.s: TO_ZERO\n");
   set_fcsr(TO_ZERO);
   TESTINST_FFF_S("vfadd.s", vr2, vr3, vr4, 0, 0);

   printf("test vfadd.s: TO_PLUS_INFINITY\n");
   set_fcsr(TO_PLUS_INFINITY);
   TESTINST_FFF_S("vfadd.s", vr2, vr3, vr4, 0, 0);

   printf("test vfadd.s: TO_MINUS_INFINITY\n");
   set_fcsr(TO_MINUS_INFINITY);
   TESTINST_FFF_S("vfadd.s", vr2, vr3, vr4, 0, 0);


//
//   printf("test vseq.b: ");
//   TESTINST_LOAD_RRR("vseq.b", vr2, vr3, vr4, 0x40, 0x30);
//
//   printf("test vand.v: ");
//   TESTINST_LOAD_RRR("vand.v", vr2, vr3, vr4, 0x40, 0x30);

//   printf("%hd ", s16);
//   TESTINST_LOAD_RRI("ld.h", s16, mem, 24);
//   printf("%hd\n", s16);

//   show();
}

int main(void)
{
   test();
   return 0;
}
