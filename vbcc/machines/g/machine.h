#ifndef MACHINE_H
#define MACHINE_H
/*  Example backend for vbcc, it models a generic 32bit RISC or CISC
    CPU.

    Configurable at build-time are:
    - number of (32bit) general-purpose-registers
    - number of (64bit) floating-point-registers
    - number of (8bit) condition-code-registers
    - mechanism for stack-arguments (moving ot fixed sp)

    It allows to select as run-time-options:
    - two- or three-address code
    - memory operands or load-store-architecture
    - number of register-arguments
    - number of caller-save-registers
*/

extern int DEBUG;

/* buil-time configurable options: */
//#define NUM_GPRS 32
//#define NUM_GPRS 10
//#define NUM_FPRS 32
//#define NUM_CCRS 8    // CONDITION CODE REGISTER?? JL

// FIXED SP means (I think) SP is frozen at entry to fn and all vars are via offsets to SP
// I thought this scheme included a FramePointer FP that was fixed at entry and not stack??
#define FIXED_SP 1

#include "dt.h"


/*  This struct can be used to implement machine-specific           */
/*  addressing-modes.                                               */
/*  Currently possible are (const,gpr) and (gpr,gpr)                */
struct AddressingMode{
      int notused;
};

enum {
    R_NONE = 0,
    R_GTMP1, R_GTMP2,
    R_FTMP1, R_FTMP2,
    R_G0, R_G1, R_G2, R_G3, R_G4, R_G5, R_G6, R_G7, R_G8, R_G9, R_GA, R_GB, R_GC, R_GD, R_GE, R_GF,
    R_G10, R_G11, R_G12, R_G13, R_G14, R_G15, R_G16, R_G17, R_G18, R_G19, R_G1A, R_G1B, R_G1C, R_G1D, R_G1E, R_G1F,
    R_F0, R_F1, R_F2, R_F3, R_F4, R_F5, R_F6, R_F7, R_F8, R_F9, R_FA, R_FB, R_FC, R_FD, R_FE, R_FF,
    SP_STASH,
    SP
};

//#define FPVLA_REG FP
#undef FPVLA_REG

/*  The number of registers of the target machine.                  */
#define MAXR SP
//NUM_GPRS+NUM_FPRS//+NUM_CCRS

/*  Number of commandline-options the code-generator accepts.       */
#define MAXGF 1

/*  If this is set to zero vbcc will not generate ICs where the     */
/*  target operand is the same as the 2nd source operand.           */
/*  This can sometimes simplify the code-generator, but usually     */
/*  the code is better if the code-generator allows it.             */
#define USEQ2ASZ 1

/*  This specifies the smallest integer type that can be added to a */
/*  pointer.                                                        */
#define MINADDI2P INT

/*  If the bytes of an integer are ordered most significant byte    */
/*  byte first and then decreasing set BIGENDIAN to 1.              */
#define BIGENDIAN 0

/*  If the bytes of an integer are ordered lest significant byte    */
/*  byte first and then increasing set LITTLEENDIAN to 1.           */
#define LITTLEENDIAN 1

/*  Note that BIGENDIAN and LITTLEENDIAN are mutually exclusive.    */

/*  If switch-statements should be generated as a sequence of       */
/*  SUB,TST,BEQ ICs rather than COMPARE,BEQ ICs set this to 1.      */
/*  This can yield better code on some machines.                    */
#define SWITCHSUBS 0

/*  In optimizing compilation certain library memcpy/strcpy-calls   */
/*  with length known at compile-time will be inlined using an      */
/*  ASSIGN-IC if the size is less or equal to INLINEMEMCPY.         */
/*  The type used for the ASSIGN-IC will be UNSIGNED|CHAR.          */
#define INLINEMEMCPY 1024

/*  Parameters are sometimes passed in registers without __reg.     */
#undef HAVE_REGPARMS
//#define HAVE_REGPARMS
#define REGPARM_COUNT 1

/*  Parameters on the stack should be pushed in order rather than   */
/*  in reverse order.                                               */
//#define ORDERED_PUSH FIXED_SP
//#define ORDERED_PUSH 1
//undef ORDERED_PUSH

/*  Structure for reg_parm().                                       */
struct reg_handle{
    unsigned long gregs;
    unsigned long fregs;
};

/*  We have some target-specific variable attributes.               */
// FIXME undef this
#define HAVE_TARGET_ATTRIBUTES

/* We have target-specific pragmas */
// FIXME undef this
#define HAVE_TARGET_PRAGMAS

/*  We keep track of all registers modified by a function.          */
#define HAVE_REGS_MODIFIED 1

/* We have a implement our own cost-functions to adapt
   register-allocation */

//#define HAVE_TARGET_RALLOC 1
#undef HAVE_TARGET_RALLOC
/*
#define cost_move_reg(x,y) 1
#define cost_load_reg(x,y) 2
#define cost_save_reg(x,y) 2
#define cost_pushpop_reg(x) 3
 */

/* size of buffer for asm-output, this can be used to do
   peephole-optimizations of the generated assembly-output */
#define EMIT_BUF_LEN 1024 /* should be enough */
/* number of asm-output lines buffered */
//#define EMIT_BUF_DEPTH 4
#define EMIT_BUF_DEPTH 1

/*  We have no asm_peephole to optimize assembly-output */
#undef HAVE_TARGET_PEEPHOLE

/* we do not have a mark_eff_ics function, this is used to prevent
   optimizations on code which can already be implemented by efficient
   assembly */
#undef HAVE_TARGET_EFF_IC

/* we only need the standard data types (no bit-types, different pointers
   etc.) */
#undef HAVE_EXT_TYPES
#undef HAVE_TGT_PRINTVAL

/* we do not need extra elements in the IC */
#undef HAVE_EXT_IC

/* we do not use unsigned int as size_t (but unsigned long, the default) */
#define HAVE_INT_SIZET 1

/* we do not need register-pairs */
#undef HAVE_REGPAIRS


/* do not create CONVERT ICs from integers smaller than int to floats */
#define MIN_INT_TO_FLOAT_TYPE INT

/* do not create CONVERT ICs from floats to ints smaller than int */
#define MIN_FLOAT_TO_INT_TYPE INT

/* do not create CONVERT_ICs from floats to unsigned integers */
#define AVOID_FLOAT_TO_UNSIGNED 1

/* do not create CONVERT_ICs from unsigned integers to floats */
#define AVOID_UNSIGNED_TO_FLOAT 1

/* convert multiplications/division by powers of two to shifts */
#define HAVE_POF2OPT 1

#endif
