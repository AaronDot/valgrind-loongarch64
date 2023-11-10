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
