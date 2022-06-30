
/*
 * Explanation of stack and arg passing lacking/poor (eg FIXED_SP).
 * Not professional quality code.
 * Many global vars instead of using local makes all the functions riddled with side effects
 * makes the code much harder to understand.
 * Lack of useful commenting.
 *
 * look at the back ends not written by Volker as these are generally much cleaner, for example:
 * fire16, falco16 & i386 backend is cleaner than generic or 6502.
 * mark cpu - has comments on registers usage
 * z looks interesting - see dump function
 * judge the code by the number of global vars, functions with side effects and massive functions
 *
 * fire16 looks like the best structured / cleanest code but there's too much reg_pair stuff for me to rip apart so I'll persist with what I have based on "generic" and just improve it
 *
 * regs and regused - not explained.
 * regs[] is initialised at start of function to contain regsa[] which are those reg's already in use.
 *
 * regtype = no discussion on this nor why it's appropriate to set only the "flags" fields of Typ in the llong/ldbl/lchar initialiser
 *
 * compiling with -O0 causes registers to only be used transiently for instructions processing by the compiler
 * so state is not carried between instructions in registers. So in my case I had a problem where registers
 * were not being preserved across a subroutine as sub was trampling it and my back end wasn't backing them up
 * but when running with -O0 it worked fine. Useful for debugging.
 *
 * */
void push(int ignored) {
    // JL added to make ti compile
}

#include "supp.h"
#include "stdint.h"
#include "../../supp.h"

#define BASETYPE(x) ((x & NQ))
#define ISUNSIGNED(t) ((t) & UNSIGNED)
#define ISSIGNED(t) (!ISUNSIGNED(t))
#define ISCHAR(t) ((t&NQ)==CHAR)
#define ISSHORT(t) ((t&NQ)==SHORT||(t&NQ)==INT||(t&NQ)==POINTER)
#define ISFPOINTER(t) ((t&NQ)==FPOINTER)
#define ISNEGATIVEBYTE(x)   ( (x) & 128 )

// truncate to a byte size value - removes any leading sign extension
#define BYTE(x) ((x) & 0xff)

void konstCopy(FILE *f, char *targName, int targSize, struct obj *src, int srcType);

void memCopy(FILE *f, int isSigned, char *targName, char *srcName, int targSize, int srcSize);

int firstFn = 1;

/*
#define KONST 1
The object is a constant. Its value is in the corresponding (to typf or typf2)
member of val.

#define VAR 2
The object is a variable. The pointer to its struct Var is in v. val.vlong vontains an offset that has to be added to it. Fur further details, see Section 16.3.3
[Variables], page 153.
JL: The address of the variable will be SP+-OFFSET

#define DREFOBJ 32
The content of the location in memory the object points to is used. dtyp
contains the type of the pointer. In systems with only one pointer type, this
will always be POINTER.

#define REG 64
The object is a register. reg contains its number.
JL: In SPAM1 I treat this just like a static variable but I should add some H/W registers and set the pref to use them

#define VARADR 128
The address of the object is to be used. Only together with static variables (i.e.
storage_class == STATIC or EXTERN).
 */


// 16.3.2
// 0 (no object)    - what is it then?
// KONST            - a literal value
// KONST|DREFOBJ    - a literal value that is a pointer to some value
// REG              - the value is held in a register
// VAR              - the value is a variable and may be in memory or in a register
// VAR|REG          - the value is a variable held in a register
// REG|DREFOBJ      - the value is
// KONST|DREFOBJ    **** duplicate entry ****
// VAR|DREFOBJ
// VAR|REG|DREFOBJ
// VARADR

int isReg(struct obj *x) {
    return ((x->flags & (REG | DREFOBJ)) == REG);
}

int isKonst(struct obj *x) {
    return ((x->flags & (KONST | DREFOBJ)) == KONST);
}

int isVar(struct obj *x) {
    return (x->flags & (VAR | REG)) == VAR;
}

int involvesKonst(struct obj *x) {
    return (x->flags & KONST);
}

int involvesReg(struct obj *x) {
    return (x->flags & REG);
}

int involvesVar(struct obj *x) {
    return (x->flags & VAR);
}

int isSpecialPurposeHalt(int r) {
 return r >= R_HALT1 && r <= R_HALT2;
}

//#define involvesreg(x) ((p->x.flags&(REG))==REG)

// is a KONST but not DEREFOBJ
#define isconst(x) ((p->x.flags & (KONST | DREFOBJ)) == KONST)
#define iskonstish(x) (p->x.flags & KONST)
// is a KONST pointer
#define isconstderef(x) ((p->x.flags & (KONST | DREFOBJ)) == (KONST|DREFOBJ))
// is a REG but not DEREFOBJ
#define isreg(x) ((p->x.flags & (REG | DREFOBJ)) == REG)
#define isWHAT(x) ((p->x.flags & (REG | DREFOBJ)) == DREFOBJ)
// is a VAR that's not a REG
#define isregderef(x) ((p->x.flags & (REG | DREFOBJ)) == (REG|DREFOBJ))
#define isvar(x) ((p->x.flags & (VAR | REG)) == VAR)
// is a VAR and REG
#define isvarreg(x) ((p->x.flags & (VAR | REG)) == (VAR|REG))

int getReg(struct obj *o) {
//    if (isReg(o))
    if (involvesReg(o))
        return o->reg;
    return 0;
}


int q1Reg(struct IC *p) {
    if (isReg(&p->q1))
        return p->q1.reg;
    else
        terror("JL - picking q1 reg when not a register\n");
    //return 0;
}

int q2Reg(struct IC *p) {
    if (isReg(&p->q2))
        return p->q2.reg;
    else
        terror("JL - picking q2 reg when not a register\n");
    //return 0;
}

int zReg(struct IC *p) {

    if (isReg(&p->z)) { // && (THREE_ADDR || !compare_objects(&p->q2, &p->z))) {
        return p->z.reg;
    } else {
        terror("JL - picking z reg wen not a register\n");
        // FIXME = JL IS THIS NEEDED - WHY NOT 0? WHY NOT JUST ACCEPT THE vbcc value which was presumanbly 0 or undef or n/a
        //if (ISFLOAT(ztyp(p))) {
        //    return R_FTMP1;
        //} else {
        //    return R_GTMP1;
        //}
    }
}


void dumpIC(FILE *f, struct IC *p);

void dumpObj(FILE *f, char *label, struct obj *o, int otyp, struct IC *p);

static long real_offset(struct obj *o);

int freturn(struct Typ *t);

void emitvalJL(FILE *f, union atyps *p, int t, int byteToEmit);

static char FILE_[] = __FILE__;

/*  Public data that MUST be there.                             */

/* Name and copyright. */
char cg_copyright[] = "vbcc generic code-generator V0.1b (c) in 2001 by Volker Barthelmann";

/*  Commandline-flags the code-generator accepts:
0: just a flag
VALFLAG: a value must be specified
STRINGFLAG: a string can be specified
FUNCFLAG: a function will be called
apart from FUNCFLAG, all other versions can only be specified once */
int g_flags[MAXGF] = {0};

/* the flag-name, do not use names beginning with l, L, I, D or U, because
   they collide with the frontend */
char *g_flags_name[MAXGF] = {};
/* the results of parsing the command-line-flags will be stored here */
union ppi g_flags_val[MAXGF];

/*  Alignment-requirements for all types in bytes.              */
zmax align[MAX_TYPE + 1];

/*  Alignment that is sufficient for every object.              */
zmax maxalign;

/*  CHAR_BIT for the target machine.                            */
zmax char_bit;

/*  sizes of the basic types (in bytes) */
zmax sizetab[MAX_TYPE + 1];

/*  Minimum and Maximum values each type can have.              */
/*  Must be initialized in init_cg().                           */
zmax t_min[MAX_TYPE + 1];
zumax t_max[MAX_TYPE + 1];
zumax tu_max[MAX_TYPE + 1];

/*  Names of all registers. will be initialized in init_cg(),
    register number 0 is invalid, valid registers start at 1 */
char *regnames[MAXR + 1];

/*  The Size of each register in bytes.                         */
zmax regsize[MAXR + 1];

/*  a type which can store each register. */
struct Typ *regtype[MAXR + 1];

/*  regsa[reg]!=0 if a certain register is allocated and should */
/*  not be used by the compiler pass.                           */
int regsa[MAXR + 1];

/*  Specifies which registers may be scratched by functions.    */
int regscratch[MAXR + 1];

/* specifies the priority for the register-allocator, if the same
   estimated cost-saving can be obtained by several registers, the
   one with the highest priority will be used */
int reg_prio[MAXR + 1];

/* an empty reg-handle representing initial state */
struct reg_handle empty_reg_handle = {0, 0};

/* Names of target-specific variable attributes.                */
char *g_attr_name[] = {"__interrupt", 0};

/****************************************/
/*  Private data and functions.         */
/****************************************/

#define byte  char

#define LOAD_STORE 0
#define THREE_ADDR 1
#define IMM_IND    (0)
#define GPR_IND    (0)
#define GPR_ARGS   (0)
#define FPR_ARGS   (0)
#define USE_COMMONS (0)

// JL aligns with the indexes of the "typenames" in supp.h (approx line 14) but index 0 is filled with a non-type called "??" whose purpose I don't understand
// Also see typname at top of in supp.c
/* alignment of basic data-types, used to initialize align[] */
static long malign[MAX_TYPE + 1] = {0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
/* sizes of basic data-types, used to initialize sizetab[] */
static long msizetab[MAX_TYPE + 1] = {0, 1, 2, 4, 4, 8, 4, 8, 8, 0, 4, 0, 0, 0, 4, 0};

/* used to initialize regtyp[] */
static struct Typ llong = {LONG}, ldbl = {DOUBLE}, lchar = {CHAR};

/* macros defined by the backend */
static char *marray[] = {"__section(x)=__vattr(\"section(\"#x\")\")",
                         "__GENERIC__",
                         0};

static char *sdt[MAX_TYPE + 1] = {"??", "c", "s", "i", "l", "ll", "f", "d", "ld", "v", "p"};
static char *udt[MAX_TYPE + 1] = {"??", "uc", "us", "ui", "ul", "ull", "f", "d", "ld", "v", "p"};

const char *dt(int t) {
    return ((t) & UNSIGNED) ? udt[(t) & NQ] : sdt[(t) & NQ];
}

/* sections */
#define DATA 0
#define BSS 1
#define CODE 2
#define RODATA 3
#define SPECIAL 4

static long stack;
static int stack_valid;

static int section = -1, newobj;
static char *codename = "\t.text\n",
        *dataname = "\t.data\n",
        *bssname = "",
        *rodataname = "\t.section\t.rodata\n";

/* return-instruction */
//static char *ret;

/* label at the end of the function (if any) */
static int exit_label;

/* assembly-prefixes for labels and external identifiers */
static char *labprefix = "_L", *idprefix = "_";

/* variables to calculate the size and partitioning of the stack-frame
   in the case of FIXED_SP */
static long frameoffset, maxpushed, framesize;

static long localsize, pushed, rsavesize, argsize;

struct gc_state {
    FILE *f;

    int reg_busy[MAXR + 1];    /* Track used registers */

    int op_save;        /* Saved register to restore before op end */
    long val_rv;        /* Return Value immediate (must be small enough) */
    int reg_rv;        /* Return Value register, 0=none */
    int reg_lw;        /* Register written in the last emitted op code */

    int s_argsize;
    int s_localsize;
    int s_savesize;
};


static void emit_obj(FILE *f, struct obj *p, int t);

/* calculate the actual current offset of an object relative to the
   stack-pointer; we use a layout like this:

When inside the function (ie the callee) ....

   ------------------------------------------------
   | arguments to this function                   | Set by caller
   ------------------------------------------------
   | return-address [size=2]                      | Set by caller
   ------------------------------------------------
   | caller-save registers [size=rsavesize]       | Set by caller - nb: x86 has callee save the FP of the caller
   ------- FP POINTS HERE ------------------------- << I DONT USE AN FP, I JUST REWIND - BECAUSE I USE FIXED_SP
   | local variables [size=localsize]             | Callee moves the SP to just after it's local vars
   ------- SP POINTS HERE ------------------------- << SP IS FIXED HERE AT START OF FN AND ALL STACK REFS ARE RELATIVE TO FIXED SP
   | arguments to called functions [size=argsize] | Args to any functions called by this one
   ------------------------------------------------

   All sizes will be aligned as necessary.
   In the case of FIXED_SP, the stack-pointer will be adjusted at
   function-entry to leave enough space for the arguments and have it
   aligned to 16 bytes.

   !! JL 16 bytes is only for x86 type stacks and even that it configurable in GCC
   !! DOES VBCC REALY ALIGN LIKE THAT

   Therefore, when calling a function, the stack-pointer is always aligned to 16 bytes.
   For a moving stack-pointer, the stack-pointer will usually point
   to the bottom of the area for local variables, but will move while
   arguments are put on the stack.

   This is just an example layout. Other layouts are also possible.

   JL: Good doco here .. https://people.cs.rutgers.edu/~pxk/419/notes/frames.html
   JL: Fixed vs Moving. GCC generates code that moves the SP after function entry to leave space for local vars.
   However, an alternative would have been to not move the SP preemptively and instead move it incrementally for each local var.
   I think this is what they mean by fixed vs moving SP.
   If stack is positioned leaving space for all local vars then it doesn't need to move as those vars are encountered (wound and unwound).
   Local vars are essentially statically located relative to the FP.
 */

static long real_offset(struct obj *o) {
    long off = zm2l(o->v->offset);
    if (off < 0) {
        //    fprintf(stderr, "real offset -ve off %ld\n", off);
        /* function parameter */
        // 4 bytes to skips return address
        //off = localsize + rsavesize + 4 - off - zm2l(maxalign);
//        off = localsize + rsavesize - off - zm2l(maxalign);

        // SP is always one byte below last byte pushed (ie points to where to write next) so add 1 to the offset to 'undo' that SP movement
        off = -off + localsize + rsavesize + 1;
//        fprintf(stderr, "real offset -ve off now %ld  localsise %ld rsavesize %ld maxalign %ld\n", off, localsize, rsavesize, maxalign);
    }

    off += argsize;
    //       fprintf(stderr, "real offset plus argsize  %ld  off now %ld\n", argsize, off);
    off += zm2l(o->val.vmax);

    // fprintf(stderr, "real offset plus vmax  %ld  off now %ld\n", o->val.vmax, off);
    return off;
}

/*  Initializes an addressing-mode structure and returns a pointer to
    that object. Will not survive a second call! */
/*
static struct obj *cam(int flags,int base,long offset)
{
static struct obj obj;
//static struct AddressingMode am;
obj.am=0;
// obj.am=&am;
// NOTUSED am.flags=flags;
// am.base=base;
//am.offset=offset;
return &obj;
}
*/

/* changes to a special section, used for __section() */
/*
static int special_section(FILE *f, struct Var *v) {
    char *sec;
    if (!v->vattr) return 0;
    sec = strstr(v->vattr, "section(");
    if (!sec) return 0;
    sec += strlen("section(");
    emit(f, "\t.section\t");
    while (*sec && *sec != ')') emit_char(f, *sec++);
    emit(f, "\n");
    if (f) section = SPECIAL;
    return 1;
}
 */

/* generate code to load the address of a variable into register targ */
static void load_address(FILE *f, int targ, struct obj *o, int type)
/*  Generates code to load the address of a variable into register targ.   */
{
    if (!(o->flags & VAR)) ierror(0);

    if (o->v->storage_class == AUTO || o->v->storage_class == REGISTER) {
        long offset = real_offset(o);
        //         DUMP q1 < VAR( storage:auto:0(sp) ) > 'value'
        // address is the (SP + the offset) so make that calc
        emit(f, "\t[:%s+0] = MARLO + (> %d) _S\n", regnames[targ], offset);
        emit(f, "\t[:%s+1] = MARHI A_PLUS_B_PLUS_C (< %d)\n", regnames[targ], offset);
        emit(f, "\t[:%s+2] = 0\n", regnames[targ]);
        emit(f, "\t[:%s+3] = 0\n", regnames[targ]);

    } else {
        terror("load_address not auto");
        emit(f, "\tmov.%s\t%s,", dt(POINTER), regnames[targ]);
        emit_obj(f, o, type);
        emit(f, "\n");
    }
}


byte getByteL(long x, int byteToEmit) {
    int mask = 0xff << (byteToEmit * 8);

    // LSB = byte 0
    byte v = (x & mask) >> (byteToEmit * 8);
    return v;
}

byte getByteLD(dt13f x, int byteToEmit) {
    // https://stackoverflow.com/questions/1723575/how-to-perform-a-bitwise-operation-on-floating-point-numbers

    return x.a[8 - byteToEmit - 1];
}

void emitByteHex(FILE *f, byte v) {
    emit(f, "$%02x", BYTE(v));
}

byte extractByte(union atyps *p, int t, int byteToEmit)
/*  Gibt atyps aus.                                     */
{
    zmax vmax;
    zumax vumax;
    zldouble vldouble;

    t &= NU;
    if (t == CHAR) {
        return getByteL(zc2zm(p->vchar), byteToEmit);
    } else if (t == SHORT) {
        return getByteL(zs2zm(p->vshort), byteToEmit);
    } else if (t == INT) {
        return getByteL(zi2zm(p->vint), byteToEmit);
    } else if (t == LONG) {
        return getByteL(zl2zm(p->vlong), byteToEmit);
    } else if (t == LLONG) {
        return getByteL(zll2zm(p->vllong), byteToEmit);
    } else if (t == MAXINT) {
        fprintf(stderr, "JL - NO IDEA WHAT MAXINT MEANS");
        return getByteL(p->vmax, byteToEmit);
    } else if (t == (UNSIGNED | CHAR)) {
        return getByteL(zuc2zum(p->vuchar), byteToEmit);
    } else if (t == (UNSIGNED | SHORT)) {
        return getByteL(zus2zum(p->vushort), byteToEmit);
    } else if (t == (UNSIGNED | INT)) {
        return getByteL(zui2zum(p->vuint), byteToEmit);
    } else if (t == (UNSIGNED | LONG)) {
        return getByteL(zul2zum(p->vulong), byteToEmit);
    } else if (t == (UNSIGNED | LLONG)) {
        fprintf(stderr, "JL - NOT HANDLING 8 Byte values tet\n");
        ierror(1);
        //return getByteL(zull2zum(p->vullong), byteToEmit);
    } else if (t == (UNSIGNED | MAXINT)) {
        return getByteL(p->vumax, byteToEmit);
    } else {
        if (t == FLOAT) {
            fprintf(stderr, "JL - NOT HANDLING FLOATS\n");
            return getByteLD(zf2zld(p->vfloat), byteToEmit);
        } else if (t == DOUBLE) {
            fprintf(stderr, "JL - NOT HANDLING FLOATS\n");
            return getByteLD(zd2zld(p->vdouble), byteToEmit);
        } else if (t == LDOUBLE) {
            fprintf(stderr, "JL - NOT HANDLING FLOATS\n");
            fprintf(stderr, "JL - NOT HANDLING 8 Byte values tet\n");
            ierror(1);
//        return getInt8F(p->vldouble, byteToEmitó);
        }
            /*FIXME*/
        else if (t == POINTER) {
            fprintf(stderr, "JL - APPARENTY POINTER HAS A PROBLEM - AS THERES A FIXME\n");
            return getByteL(zul2zum(p->vulong), byteToEmit);
        } else {
            fprintf(stderr, "JL - NOT KNOWN TYPE %d\n", t);
            ierror(1);
        }
    }
}


void emitvalJL(FILE *f, union atyps *p, int t, int byteToEmit) {
    byte v = extractByte(p, t, byteToEmit);
    emitByteHex(f, v);
}

/* Generates code to load a memory object into register targReg.
 * */
static void load_reg(FILE *f, int targReg, struct obj *o, int type) {
    type &= NU;
    if (o->flags & VARADR) {
        emit(f, "\t; load_reg VARADR - %s(%s) <= address\n", regnames[targReg], dt(type));
        load_address(f, targReg, o, POINTER);
        return;
    }

    if (isReg(o) && o->reg == targReg) {
        emit(f, "\t; load_reg skipping redundant self-copy of %s\n", regnames[targReg]);
        // avoid copying reg to self if source is a reg and the same reg is the target
        return;
    }

    char *targRegName = regnames[targReg];
    int targRegSize = regsize[targReg];
    struct Typ *pTargType = regtype[targReg];
    int typeSize = msizetab[BASETYPE(type)];

    if (isKonst(o)) {
        emit(f, "\t; load_reg targ from KONST - reg size:%d    src data size:%d\n", targRegSize, typeSize);
        if (ISINT(type)) {
            konstCopy(f, targRegName, targRegSize, o, type);
            return;
        } else {
            fprintf(stderr, "FIXME - TYPE NOT HANDLED\n");
        };
        return;
    } else if (isReg(o)) {
        emit(f, "\t; load_reg targ from REG - reg size:%d    src data size:%d\n", targRegSize,
             msizetab[BASETYPE(type)]);
        int srcReg = o->reg;
        char *srcRegName = regnames[srcReg];
        int srcRegSize = regsize[srcReg];
        memCopy(f,
                ISSIGNED(o->flags),
                targRegName,
                srcRegName,
                targRegSize,
                srcRegSize);
        return;
    } else if (isVar(o)) {
        emit(f, "\t; load_reg targ from VAR - reg size:%d    src data size:%d\n", targRegSize,
             msizetab[BASETYPE(type)]);

        /* Doc Says ....
            The only case where typf == ARRAY should be in automatic initializations.
            It is also possible that (typf&NQ) == CHAR but the size is != 1. This is created
            for an inline memcpy/strcpy where the type is not known.
         */
//        if (ISPOINTER(type) || (ISCHAR(type) && zm2l(o->val.vmax) != 1)) {
//        if (ISARRAY(type) || (ISCHAR(type) && zm2l(o->val.vmax) != 1)) {
//            if ((ISCHAR(type) && zm2l(o->val.vmax) != 1)) {
        if (ISARRAY(type)) {
            // what is this
            fprintf(stderr, "[[[[[[ %ld ]]]]]\n", o->val.vmax);
            emit(f, "ERROR IF ASSIGN & (TYPE>POINTER|(TYPE=CHAR & VAL %d!=1))\n", zm2l(o->val.vmax));
//            ierror(0);
        }

        // read from load bytes relative to stack ptr
        if (o->v->storage_class != AUTO && o->v->storage_class != REGISTER) {
            fprintf(stderr, "FIXME - AUTO and REGISTER storage classes only\n");
            //JLierror(1);
        }

        // save marlo which hold the SP
        emit(f, "\t; stash SP\n");
        emit(f, "\t[:sp_stash]   = MARLO\n");
        emit(f, "\t[:sp_stash+1] = MARHI\n");

        // adjust the MAR according to the offset
        int offset = real_offset(o);

        emit(f, "\t; adjust SP by offset %d to point at var\n", offset);
        emit(f, "\tMARLO = MARLO + (> %d) _S            ; add lo byte of offset\n", offset);
        emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C (< %d) ; add hi byte of offset plus any carry\n", offset);

        // MAR now points at low address base of value

        int size = msizetab[BASETYPE(type)];
        if (size == 0) {
            fprintf(stderr, "CANT COPY 0 BYTES\n");
            //JLierror(1);
        }

        emit(f, "\t; copy %d bytes\n", size);
        int pos = 0;
        while (pos < size) {
            emit(f, "\tREGA     = RAM\n");
            emit(f, "\t[:%s+%d] = REGA\n", regnames[targReg], pos);

            emit(f, "\tMARLO = MARLO + 1 _S\n");
            emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C 0\n");

            pos++;
        }

        // restore marlo which hold the SP
        emit(f, "\t; restore SP\n");
        emit(f, "\tMARLO = [:sp_stash]\n");
        emit(f, "\tMARHI = [:sp_stash+1]\n");
        return;
    } else if ((o->flags & (REG | DREFOBJ)) == (REG | DREFOBJ)) {
        // eg         DUMP q1 < REG(gpr6) DREFOBJ VAR( storage:auto:4(sp) ) > 'pValue'    where p is an int* and I am dereferencing it
        emit(f, "\t; stash SP\n");
        emit(f, "\t[:sp_stash]   = MARLO\n");
        emit(f, "\t[:sp_stash+1] = MARHI\n");

        char *srcRegName = regnames[o->reg];
        emit(f, "\tMARLO = [:%s]\n", srcRegName);
        emit(f, "\tMARHI = [:%s+1]\n", srcRegName);

        int pos = 0;
        while (pos < typeSize) {
            emit(f, "\tREGA  = RAM\n");
            emit(f, "\t[:%s+%d] = REGA\n", targRegName, pos);
            emit(f, "\tMARLO = MARLO + 1 _S\n");
            emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C 0\n");
            pos++;
        }

        emit(f, "\tMARLO = [:sp_stash]\n");
        emit(f, "\tMARHI = [:sp_stash+1]\n");

        return;
    }

    emit(f, "\t; load_reg targ from VAR - reg size:%d    src data size:%d\n", targRegSize, msizetab[BASETYPE(type)]);
    fprintf(stderr, "FIXME - CLASS NOT HANDLED\n");

    fprintf(stderr, "\t; ORIGINAL : mov.%s\t%s,", dt(type), regnames[targReg]);
    emit_obj(f, o, type);
    fprintf(stderr, "\n");
    ierror(1);
}

/*  Generates code to store register r into memory object o. */
static void store_reg(FILE *f, int r, struct obj *o, int type) {
    type &= NQ;
    emit(f, "\tmov.%s\t", dt(type));
    emit_obj(f, o, type);
    emit(f, ",%s\n", regnames[r]);
}

/*  Yields log2(x)+1 or 0. */
static long pof2(zumax x) {
    zumax p;
    int ln = 1;
    p = ul2zum(1L);
    while (ln <= 32 && zumleq(p, x)) {
        if (zumeqto(x, p)) return ln;
        ln++;
        p = zumadd(p, p);
    }
    return 0;
}

static void function_top(FILE *, struct Var *, long);

static void function_bottom(FILE *f, struct Var *, long);

void gc_getreturn(FILE *f, struct IC *p);

void gc_call(FILE *f, struct IC *p);

void gc_convert(FILE *f, struct IC *p);

void gc_address(FILE *f, struct IC *p);

void gc_bra(FILE *f, struct IC *p);

void gc_arithmetic(FILE *f, struct IC *p);

void gc_push(FILE *f, struct IC *p);

void gc_assign(FILE *f, struct IC *p);

void gc_branch_conditional(FILE *f, struct IC *p);

void gc_setreturn(FILE *f, struct IC *p);

void gc_allocreg(FILE *f, struct IC *p);

void gc_freereg(FILE *f, struct IC *p);

void gc_compare(FILE *f, struct IC *p);

static char *ccs[] = {"eq", "ne", "lt", "ge", "le", "gt", ""};
static char *logicals[] = {"or", "xor", "and"};
static char *arithmetics[] = {"slw", "srw", "add", "sub", "mullw", "divw", "mod"};

/* save the result (held in srcReg) into p->z */
void save_result(FILE *f, struct IC *p, int srcReg) {
    fprintf(stderr, "NOT IMPL save_Result\n");
    ierror(1);
    /*
    char *srcRegName = regnames[srcReg];

    emit(f, "; save_result stored in %s\n", srcRegName);
    if ((p->z.flags & (REG | DREFOBJ)) == DREFOBJ && !p->z.am) {
        // the value to be loaded is actually at the far end of the pointer that's in z
        // so we'll deref the pointer and pull the value into a temporary register
        emit(f, "; save_result DREFOBJ loading_reg\n");
        p->z.flags &= ~DREFOBJ;
        load_reg(f, R_GTMP2, &p->z, POINTER);

        // We've loaded the value into a tmp reg so update the instruction for further processing.
        // The value is now in a tmp register so update the flags to reflect that.
        p->z.reg = R_GTMP2;
        p->z.flags |= (REG | DREFOBJ);
    }

    // if targ is a register
    if (isreg(z)) {
        int targReg = p->z.reg;
        char *targRegName = regnames[targReg];
        int targRegSize = regsize[targReg];
        int srcRegSize = regsize[srcReg];
        memCopy(f,
                ISSIGNED(p->z.flags),
                targRegName,
                srcRegName,
                targRegSize,
                srcRegSize);

    } else {
        emit(f, "; save_result ELSE store_reg\n");
        store_reg(f, srcReg, &p->z, ztyp(p));
    }
     */
}

/* save the result (held in srcReg) into p->z */
void memCopy(FILE *f, int isSigned, char *targName, char *srcName, int targSize, int srcSize) {

    if (srcSize == 0) {
        fprintf(stderr, "memCopy srcSize cannot be 0\n");
        ierror(1);
    }
    if (targSize == 0) {
        fprintf(stderr, "memCopy targSize cannot be 0\n");
        ierror(1);
    }

    // if different names
    if (strcmp(srcName, targName) != 0) {

        emit(f, "\t; memCopy : move %d bytes  %s <- %s  \n", srcSize, targName, srcName);

        if (srcSize > targSize) {
            fprintf(stderr, "COPYING BETWEEN REG:  srcReg (%s) -> targReg (%s)\n", srcName, targName);
            fprintf(stderr, "FIXME - SOURCE DATA SIZE %d WILL NOT FIT IN TARGET SIZE %d\n", srcSize, targSize);
            ierror(1);
        }


        int needsSignExtension = isSigned && srcSize > targSize;
        int pos = 0;
        while (pos < srcSize) {
            emit(f, "\tREGA = [:%s+%d]\n", srcName, pos);
            emit(f, "\t[:%s+%d] = REGA%s\n", targName, pos, needsSignExtension ? " _S" : "");
            pos++;
        }

        while (pos < targSize) {
            /*
             * logic is
             *  set reg to 0
             *  if neg flag is set then overwrite with $ff
             * */
            emit(f, "\t[:%s+%d] = 0 ; padding\n", targName, pos);
            if (isSigned) {
                // extend sign left if this is signed negative value
                // last byte copied used the _S flag so the status register knows if it was negative
                // so optionally set it to FF if the NEG (_N) flag is set
                emit(f, "\t[:%s+%d] = $ff _N ; padding NEG as top bit of was a 1\n", targName, pos);
            }
            pos++;
        }
        return;
    } else {
        emit(f, "\t; targ %s and src %s are same - so nothing to do\n", targName, srcName);
    }

}

void konstCopy(FILE *f, char *targName, int targSize, struct obj *src, int srcType) {
    int srcSize = msizetab[BASETYPE(srcType)];

    if (ISINT(src->flags)) {

        if (srcSize > targSize) {
            fprintf(stderr, "FIXME - SOURCE DATA SIZE %d WILL NOT FIT IN TARGET SIZE %d", srcSize, targSize);
            ierror(1);
        }

        int pos = 0;
        byte v;
        while (pos < srcSize) {
            v = extractByte(&src->val, srcType, pos);
            emit(f, "\t[:%s+%d] = $%02x", targName, pos, BYTE(v));
            emit(f, "\t\n");
            pos++;
        }

        // extend sign left if this is signed negative value
        byte pad = 0;
        if (ISSIGNED(srcType) && ISNEGATIVEBYTE(v)) {
            pad = 0xff;
        }

        while (pos < targSize) {
            emit(f, "\t[:%s+%d] = $%02x ; padding\n", targName, pos, BYTE(pad));
            pos++;
        }
        return;
    }

    fprintf(stderr, "FIXME - konstCopy is not int\n");
    ierror(1);
}

/* prints an object */
// !!!!!!!!!!! was working on load_reg and want to copy over bte by byte for types longer than 1
// but need emit obj to elit those byte sized portions
//
//static void emit_objJL(FILE *f, struct obj *p, int t, int offset) {
//    if ((p->flags & (KONST | DREFOBJ)) == (KONST | DREFOBJ)) {  // const pointer
//        emitvalJL(f, &p->val, p->dtyp & NU, offset);
//        return;
//    }
//    if (p->flags & DREFOBJ || p->flags & REG) emit(f, "[");  // non-const pointer follows
//
//    if (p->flags & REG) {
//        emit(f, ":%s + %d", regnames[p->reg], offset);
//    } else if (p->flags & VAR) {
//        if (p->v->storage_class == AUTO || p->v->storage_class == REGISTER) {
//            emit(f, "NOT IMPL VAR AUTO\n");
//            emit(f, "%ld(%s)", real_offset(p), regnames[SP]);
//        } else {
//            emit(f, "NOT IMPL VAR\n");
//            if (!zmeqto(l2zm(0L), p->val.vmax)) {
//                emitval(f, &p->val, LONG);
//                emit(f, "+");
//            }
//            if (p->v->storage_class == STATIC) {
//                emit(f, "%s%ld", labprefix, zm2l(p->v->offset));
//            } else {
//                emit(f, "%s%s", idprefix, p->v->identifier);
//            }
//        }
//    }
//    if (p->flags & KONST) {
//        // value is an ordinary const - (can't be a const pointer as that would have be triggered above)
//        emitvalJL(f, &p->val, t & NU, offset);
//    }
//    if (p->flags & DREFOBJ || p->flags & REG) emit(f, "]");  // non-const pointer was previous
//}

static void emit_obj(FILE *f, struct obj *p, int t) {
    if ((p->flags & (KONST | DREFOBJ)) == (KONST | DREFOBJ)) {
        emitval(f, &p->val, p->dtyp & NU);
        return;
    }
    if (p->flags & DREFOBJ) emit(f, "(");
    if (p->flags & REG) {
        emit(f, "%s", regnames[p->reg]);
    } else if (p->flags & VAR) {
        if (p->v->storage_class == AUTO || p->v->storage_class == REGISTER)
            emit(f, "%ld(%s)", real_offset(p), regnames[SP]);
        else {
            if (!zmeqto(l2zm(0L), p->val.vmax)) {
                emitval(f, &p->val, LONG);
                emit(f, "+");
            }
            if (p->v->storage_class == STATIC) {
                emit(f, "%s%ld", labprefix, zm2l(p->v->offset));
            } else {
                emit(f, "%s%s", idprefix, p->v->identifier);
            }
        }
    }
    if (p->flags & KONST) {
        emitval(f, &p->val, t & NU);
    }
    if (p->flags & DREFOBJ) emit(f, ")");
}

/*  Test if there is a sequence of FREEREGs containing FREEREG reg.
    Used by peephole. */
static int exists_freereg(struct IC *p, int reg) {
    while (p && (p->code == FREEREG || p->code == ALLOCREG)) {
        if (p->code == FREEREG && p->q1.reg == reg) return 1;
        p = p->next;
    }
    return 0;
}


// dont preserve regsa reg's because they were already in use before we entered the current fn and we've not used them so some call stack parent will have preserved them if needed.
// dont preserve scratch reg's.
// only consider preserving regused ones that we'll use.
// don't preserve the halt regs as these have a special purpose and so never need preserving.
static int shouldPreserve(int i) { return (i != R_GRET) && !regsa[i] && !regscratch[i] && regused[i] && !isSpecialPurposeHalt(i); }

/* generates the function entry code */
static void function_top(FILE *f, struct Var *v, long offset) {
    int isMain = strcmp(v->identifier, "main") == 0;

    if (firstFn && !isMain) {
        emit(f, "; jump to _main\n");
        emit(f, "PCHITMP   = < :_main\n");
        emit(f, "PC        = > :_main\n");
    }
    firstFn = 0;


    if (v->storage_class == EXTERN) {
        if ((v->flags & (INLINEFUNC | INLINEEXT)) != INLINEFUNC)
            //         emit(f, "\t.global\t%s%s\n", idprefix, v->identifier);
            emit(f, "%s%s:\n", idprefix, v->identifier);
    } else
        emit(f, "%s%ld:\n", labprefix, zm2l(v->offset));

    if (isMain) {
        emit(f, "; _main routine\n");
        // stack starts at top of 64k memory
        emit(f, "; initialise registers\n");
        emit(f, "[:sp]   = $ff\n");
        emit(f, "[:sp+1] = $ff\n");
        emit(f, "MARLO   = [:sp]\n");
        emit(f, "MARHI   = [:sp+1]\n");
    }

    // save any registers that this function trashes
    rsavesize = 0;
    for (int i = 1; i < MAXR; i++) {
        if (shouldPreserve(i)) {
            //if (regused[i] && !regscratch[i]) {
            emit(f, "\t; saving register %s\n", regnames[i]);
            int pushSz = regsize[i];
            if (pushSz != 4) {
                fprintf(stderr, "JL NOTE - For now only pushing 4 byte regs - easier to understand stack\n");
                ierror(1);
            }
            int pos = 0;
            while (pos < pushSz) {
                emit(f, "\tREGA  = [:%s+%d]\n", regnames[i], pushSz - pos - 1);
                emit(f, "\tRAM   = REGA\n");
                emit(f, "\tMARLO = MARLO - 1 _S\n");
                emit(f, "\tMARHI = MARHI A_MINUS_B_MINUS_C 0\n");

                pos++;
            }

            rsavesize += pushSz;
        }
    }


    if (offset) {
        emit(f, "\t; adjust SP to skip over the local variable stack by offset %d\n", offset);
        emit(f, "\tMARLO = MARLO - (> %d) _S            ; sub lo byte of offset\n", offset);
        emit(f, "\tMARHI = MARHI A_MINUS_B_MINUS_C (< %d) ; sub hi byte of offset plus any carry\n", offset);
    }
    emit(f, "\t; ------ caller stack frame complete \n");
}

/* generates the function exit code */
static void function_bottom(FILE *f, struct Var *v, long offset) {
    emit(f, "; function bottom logic\n");
    if (offset) {
        emit(f, "\t; rewind stack over local variable : bytes %d\n", offset);
        emit(f, "\tMARLO = MARLO + (> %d) _S            \n", offset);
        emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C (< %d) \n", offset);
    }

    if (rsavesize) {
        emit(f, "\t; rewind stack over saved register : bytes %d\n", rsavesize);
        // go in reverse
        for (int i = MAXR-1; i > 0; i--) {
            if (shouldPreserve(i)) {
                emit(f, "\t;   restoring register %s by popping\n", regnames[i]);
                int popSz = regsize[i];
                if (popSz != 4) {
                    fprintf(stderr, "JL NOTE - For now only popping 4 byte regs - easier to understand stack\n");
                    ierror(1);
                }
                int pos = 0;
                while (pos < popSz) {
                    emit(f, "\tMARLO = MARLO + 1 _S\n");
                    emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C 0\n");
                    emit(f, "\tREGA = RAM\n");
                    emit(f, "\t[:%s+%d] = REGA\n", regnames[i], pos);
                    pos++;
                }
            }
        }
    }


    if (strcmp(v->identifier, "main") == 0) {
        emit(f, "\t; end of program - halt with main exit code\n");
        int retReg = freturn(v->vtyp);
        emit(f, "\tHALT = [:%s]\n", regnames[retReg]);
    } else {
        emit(f, "\t; do return from function\n");

        emit(f, "\t; pop PCHITMP\n");
        emit(f, "\tMARLO   = MARLO + 1 _S\n");
        emit(f, "\tMARHI   = MARHI A_PLUS_B_PLUS_C 0\n");
        emit(f, "\tPCHITMP = RAM\n");
        emit(f, "\t; pop PC\n");
        emit(f, "\tMARLO   = MARLO + 1 _S\n");
        emit(f, "\tMARHI   = MARHI A_PLUS_B_PLUS_C 0\n");
        emit(f, "\tREGA    = RAM\n");
        emit(f, "\t; pop 2 unused PC byes\n");
        emit(f, "\tMARLO   = MARLO + 2 _S\n");
        emit(f, "\tMARHI   = MARHI A_PLUS_B_PLUS_C 0\n");
        emit(f, "\t; do jump\n");
        emit(f, "\tPC      = REGA\n");
    }
}

/****************************************/
/*  End of private data and functions.  */
/****************************************/

/*  Does necessary initializations for the code-generator. Gets called  */
/*  once at the beginning and should return 0 in case of problems.      */
int init_cg(void) {
    int i;
    /*  Initialize some values which cannot be statically initialized   */
    /*  because they are stored in the target's arithmetic.             */
    maxalign = l2zm(4L);
    //maxalign = l2zm(8L);

    char_bit = l2zm(8L);
    stackalign = l2zm(4);

    for (i = 0; i <= MAX_TYPE; i++) {
        sizetab[i] = l2zm(msizetab[i]);
        align[i] = l2zm(malign[i]);
    }

    for (i = R_GTMP1; i <= R_GTMP2; i++) {
        regnames[i] = mymalloc(10);
        sprintf(regnames[i], "gtmp%d", i - R_GTMP1 + 1);
        regsize[i] = l2zm(4L);
        regtype[i] = &llong;
    }
    for (i = R_FTMP1; i <= R_FTMP2; i++) {
        regnames[i] = mymalloc(10);
        sprintf(regnames[i], "ftmp%d", i - R_FTMP1 + 1);
        regsize[i] = l2zm(8L);
        regtype[i] = &ldbl;
    }
    for (i = R_HALT1; i <= R_HALT3; i++) {
        regnames[i] = mymalloc(10);
        sprintf(regnames[i], "ghalt%d", i - R_HALT1 + 1);
        regsize[i] = l2zm(4L);
        regtype[i] = &llong;

        regsa[i] = 1;
        regscratch[i] = 1;
    }
    for (i = R_G0; i <= R_G1F; i++) {
        regnames[i] = mymalloc(10);
        sprintf(regnames[i], "gpr%d", i - R_G0);
        regsize[i] = l2zm(4L);
        regtype[i] = &llong;
    }
    for (i = R_F0; i <= R_FF; i++) {
        regnames[i] = mymalloc(10);
        sprintf(regnames[i], "fpr%d", i - R_F0);
        regsize[i] = l2zm(8L);
        regtype[i] = &ldbl;
    }

    regnames[SP] = mymalloc(10);
    sprintf(regnames[SP], "sp");
    regsize[SP] = l2zm(2L);
    regtype[SP] = &llong;

    regnames[SP_STASH] = mymalloc(10);
    sprintf(regnames[SP_STASH], "sp_stash");
    regsize[SP_STASH] = l2zm(2L);
    regtype[SP_STASH] = &llong;

    /*  Use multiple ccs.   */
    multiple_ccs = 0;

    /*  Initialize the min/max-settings. Note that the types of the     */
    /*  host system may be different from the target system and you may */
    /*  only use the smallest maximum values ANSI guarantees if you     */
    /*  want to be portable.                                            */
    /*  That's the reason for the subtraction in t_min[INT]. Long could */
    /*  be unable to represent -2147483648 on the host system.          */
    t_min[CHAR] = l2zm(-128L);
    t_min[SHORT] = l2zm(-32768L);
    t_min[INT] = zmsub(l2zm(-2147483647L), l2zm(1L));
    t_min[LONG] = t_min(INT);
    t_min[LLONG] = zmlshift(l2zm(1L), l2zm(63L));
    t_min[MAXINT] = t_min(LLONG);
    t_max[CHAR] = ul2zum(127L);
    t_max[SHORT] = ul2zum(32767UL);
    t_max[INT] = ul2zum(2147483647UL);
    t_max[LONG] = t_max(INT);
    t_max[LLONG] = zumrshift(zumkompl(ul2zum(0UL)), ul2zum(1UL));
    t_max[MAXINT] = t_max(LLONG);
    tu_max[CHAR] = ul2zum(255UL);
    tu_max[SHORT] = ul2zum(65535UL);
    tu_max[INT] = ul2zum(4294967295UL);
    tu_max[LONG] = t_max(UNSIGNED | INT);
    tu_max[LLONG] = zumkompl(ul2zum(0UL));
    tu_max[MAXINT] = t_max(UNSIGNED | LLONG);

    /*  Reserve a few registers for use by the code-generator.      */
    /*  This is not optimal but simple.                             */
    /*  Mark them as in use.                             */
    regsa[R_GTMP1] = regsa[R_GTMP2] = 1;
    regscratch[R_GTMP1] = regscratch[R_GTMP2] = 0;

    regsa[R_FTMP1] = regsa[R_FTMP2] = 1;
    regscratch[R_FTMP1] = regscratch[R_FTMP2] = 0;

    regsa[SP] = 1;
    regscratch[SP] = 0;

    regsa[SP_STASH] = 1;
    regscratch[SP_STASH] = 0;

    regnames[R_GRET] = mymalloc(10);
    sprintf(regnames[R_GRET], "greturn");
    regsize[R_GRET] = l2zm(4L);
    regtype[R_GRET] = &llong;
    regsa[R_GRET] = 1;
    regscratch[R_GRET] = 0;

    for (i = R_G0; i <= R_G1F; i++)
        regscratch[i] = 0;
    for (i = R_G10; i <= R_G1F; i++)
        regscratch[i] = 1;

    for (i = R_F0; i <= R_FF; i++)
        regscratch[i] = 0;
    for (i = R_F0; i <= R_FF / 2; i++)
        regscratch[i] = 1;

    target_macros = marray;

    return 1;
}

void init_db(FILE *f) {
}

int freturn(struct Typ *t)
/*  Returns the register in which variables of type t are returned. */
/*  If the value cannot be returned in a register returns 0.        */
/*  A pointer MUST be returned in a register. The code-generator    */
/*  has to simulate a pseudo register if necessary.                 */
{
    if (ISFLOAT(t->flags))
        return R_F0;
    //return R_F0 + 2;
    if (ISSTRUCT(t->flags) || ISUNION(t->flags))
        return 0;
    if (zmleq(szof(t), l2zm(4L)))
        return R_GRET;
    else
        return 0;
}

int reg_pair(int r, struct rpair *p)
/* Returns 0 if the register is no register pair. If r  */
/* is a register pair non-zero will be returned and the */
/* structure pointed to p will be filled with the two   */
/* elements.                                            */
{
    return 0;
}

/* estimate the cost-saving if object o from IC p is placed in
   register r */
/*
int cost_savings(struct IC *p, int r, struct obj *o) {
return 0;
int c = p->code;
if (o->flags & VKONST) {
    if (!LOAD_STORE)
        return 0;
    if (o == &p->q1 && p->code == ASSIGN && (p->z.flags & DREFOBJ))
        return 4;
    else
        return 2;
}
if (o->flags & DREFOBJ)
    return 4;
if (c == SETRETURN && r == p->z.reg && !(o->flags & DREFOBJ)) return 3;
if (c == GETRETURN && r == p->q1.reg && !(o->flags & DREFOBJ)) return 3;
return 2;
}
 */

int regok(int r, int t, int mode)
/*  Returns 0 if register r cannot store variables of   */
/*  type t. If t==POINTER and mode!=0 then it returns   */
/*  non-zero only if the register can store a pointer   */
/*  and dereference a pointer to mode.                  */
{
    if (r == 0)
        return 0;
    t &= NQ;
    if (ISFLOAT(t) && r >= R_F0 && r <= R_FF)
        return 1;
    if (t == POINTER && r >= R_G0 && r <= R_G1F)
        return 1;
    if (t >= CHAR && t <= LONG && r >= R_G0 && r <= R_G1F)
        return 1;
    if (t >= CHAR && t <= LONG && r >= R_HALT1 && r <= R_HALT3)
        return 1;
    return 0;
}

int dangerous_IC(struct IC *p)
/*  Returns zero if the IC p can be safely executed     */
/*  without danger of exceptions or similar things.     */
/*  vbcc may generate code in which non-dangerous ICs   */
/*  are sometimes executed although control-flow may    */
/*  never reach them (mainly when moving computations   */
/*  out of loops).                                      */
/*  Typical ICs that generate exceptions on some        */
/*  machines are:                                       */
/*      - accesses via pointers                         */
/*      - division/modulo                               */
/*      - overflow on signed integer/floats             */
{
    int c = p->code;
    if ((p->q1.flags & DREFOBJ) || (p->q2.flags & DREFOBJ) || (p->z.flags & DREFOBJ))
        return 1;
    if ((c == DIV || c == MOD) && !isconst(q2))
        return 1;
    return 0;
}

int must_convert(int o, int t, int const_expr)
/*  Returns zero if code for converting np to type t    */
/*  can be omitted.                                     */
/*  On the PowerPC cpu pointers and 32bit               */
/*  integers have the same representation and can use   */
/*  the same registers.                                 */
{
    int op = o & NQ, tp = t & NQ;
    if ((op == INT || op == LONG || op == POINTER) && (tp == INT || tp == LONG || tp == POINTER))
        return 0;
    if (op == DOUBLE && tp == LDOUBLE) return 0;
    if (op == LDOUBLE && tp == DOUBLE) return 0;
    return 1;
}

void gen_ds(FILE *f, zmax size, struct Typ *t)
//  This function has to create <size> bytes of storage
//  initialized with zero.
{
/*
    if (newobj && section != SPECIAL)
        emit(f, "%ld\n", zm2l(size));
    else
        emit(f, "\t.space\t%ld\n", zm2l(size));
    newobj = 0;
    */
}

void gen_align(FILE *f, zmax align)
//  This function has to make sure the next data is
//  aligned to multiples of <align> bytes.
{
//JL    if (zm2l(align) > 1) emit(f, "\t.align\t2\n");
}


/* JL: for example in an array initialisation    int arr[] = {1,2,3};
 * then this function is required to prints a label for that initializer data and gen_dc()
 * will emit the actual bytes.
 * presumably the label is referred to be the array var ?
 * */
void gen_var_head(FILE *f, struct Var *v)
//  This function has to create the head of a variable
//  definition, i.e. the label and information for
//  linkage etc.
{

    emit(f, "\t;JL gen_var_head\n");

    int constflag;
    char *sec;
    if (v->clist) constflag = is_const(v->vtyp);
    if (v->storage_class == STATIC) {
        if (ISFUNC(v->vtyp->flags)) return;
//        if (!special_section(f, v))
        {
            emit(f, "\t;JL static\n");
            if (v->clist && (!constflag || (g_flags[2] & USEDFLAG)) && section != DATA) {
                emit(f, "\t;JL !const & !data\n");
                //emit(f, dataname);
                if (f) section = DATA;
            }
            if (v->clist && constflag && !(g_flags[2] & USEDFLAG) && section != RODATA) {
                emit(f, "\t;JL const & !data\n");
                //emit(f, rodataname);
                if (f) section = RODATA;
            }
            if (!v->clist && section != BSS) {
                emit(f, "\t;JL bss\n");
                //emit(f, bssname);
                if (f) section = BSS;
            }
        }
        if (v->clist || section == SPECIAL) {
            //gen_align(f, falign(v->vtyp));
            //emit(f, "EQU %s%ld:\n", labprefix, zm2l(v->offset));
            emit(f, "%s%ld:\n", labprefix, zm2l(v->offset));
        } else {
            emit(f, "\t.lcomm\t%s%ld,", labprefix, zm2l(v->offset));
            // not impl
            ierror(1);
        }
        newobj = 1;
    }
    if (v->storage_class == EXTERN) {
        emit(f, "\t;JL exterrb\n");
        emit(f, "\t.globl\t%s%s\n", idprefix, v->identifier);
        if (v->flags & (DEFINED | TENTATIVE)) {
            // if (!special_section(f, v))
            {
                if (v->clist && (!constflag || (g_flags[2] & USEDFLAG)) && section != DATA) {
                    emit(f, "\t;JL data\n");
                    emit(f, dataname);
                    if (f) section = DATA;
                }
                if (v->clist && constflag && !(g_flags[2] & USEDFLAG) && section != RODATA) {
                    emit(f, "\t;JL rodata\n");
                    emit(f, rodataname);
                    if (f) section = RODATA;
                }
                if (!v->clist && section != BSS) {
                    emit(f, "\t;JL bss\n");
                    emit(f, bssname);
                    if (f) section = BSS;
                }
            }
            if (v->clist || section == SPECIAL) {
                gen_align(f, falign(v->vtyp));
                emit(f, "%s%s:\n", idprefix, v->identifier);
            } else
                emit(f, "\t.global\t%s%s\n\t.%scomm\t%s%s,", idprefix, v->identifier, (USE_COMMONS ? "" : "l"),
                     idprefix, v->identifier);
            newobj = 1;
        }
    }
//    emit(f, "\t;JL gen_var_head end\n");
}

/* array init
13.3.3
 isstatic(storage_class) != 0
The variable can be addressed through a numbered label. The label number is
stored in offset.
val.vlong+’l’offset
 */
void gen_dc(FILE *f, int t, struct const_list *p)
//  This function has to create static storage
//  initialized with const-list p.
{
    // NOTE: The label name will already have been written by gen_var_head - here we just write the data
//    emit(f, "\t;JL gen_dc\n");
//    emit(f, "\t; dc.%s\n", dt(t & NQ) );
    int datatypeSize = msizetab[BASETYPE(t)];

    emit(f, "BYTES [");
    for (int pos = 0; pos < datatypeSize; pos++) {
        unsigned char c = BYTE(extractByte(&p->val, t, pos));
        emit(f, "$%02x", c);
        if (pos + 1 < datatypeSize) {
            emit(f, ", ");
        }
    }
    emit(f, "]\n");
/*
    if (!p->tree) {
        if (ISFLOAT(t)) {
            //  auch wieder nicht sehr schoen und IEEE noetig
            unsigned char *ip;
            ip = (unsigned char *) &p->val.vdouble;
            emit(f, "0x%02x%02x%02x%02x", ip[0], ip[1], ip[2], ip[3]);
            if ((t & NQ) != FLOAT) {
                emit(f, ",0x%02x%02x%02x%02x", ip[4], ip[5], ip[6], ip[7]);
            }
        } else {
            emitval(f, &p->val, t & NU);
        }
    } else {
        emit_obj(f, &p->tree->o, t & NU);
    }
    */
    emit(f, "\n");
    newobj = 0;
}

// JL6502 impl flips the sense of the comparisons - presumably for cpu efficiency
int findBranch(FILE *f, IC *p) {
    IC *b;
    //puts("!!!!!!!!!!!!!!!!!!!!!!!!!!!!! look for branch");
    //printf("\t>>>>>>> DUMP code = %s(%d)\n", ename[p->code], p->code);

    for (b = p->next; b; b = b->next) {
        //printf( "\t>>>>>>> DUMP code = %s(%d)\n", ename[b->code], b->code);
        if (!b) {
            puts("end of file while searching for branch");
            emit(f, "end of file while searching for branch");
            ierror(0);
        }
        if (b->code == LABEL) {
            puts("found LABEL while searching for branch");
            emit(f, "found LABEL while searching for branch");
            ierror(0);
        }
        if (b->code == CALL) {
            puts("found CALL while searching for branch");
            emit(f, "found CALL while searching for branch");
            ierror(0);
        }

        int bc = b->code;

        if (bc >= BEQ && bc <= BGT) {
            return bc;
        }
    }
    return -1;
}

/*  The main code-generation routine.                   */
/*  f is the stream the code should be written to.      */
/*  p is a pointer to a doubly linked list of ICs       */
/*  containing the function body to generate code for.  */
/*  v is a pointer to the function.                     */
/*  offset is the size of the stackframe the function   */
/*  needs for local variables.                          */
void dumpreg();

void gen_code(FILE *f, struct IC *p, struct Var *v, zmax offset)
/*  The main code-generation.                                           */
{
    emit(f, ";================================================================================= FUNCTION TOP %s\n", v->identifier);

    //fixme - should this code should step PS over the vars?
    //fixme - or is code gen using -ve offsets relative to SP value at fn entry?
    int c, i;
    struct IC *m;
    argsize = 0;
    if (DEBUG & 1) printf("gen_code()\n");

    printf("gen_code() %s frame=%ld\n", v->identifier, offset);
    fflush(stdout);

    if (strcmp(v->identifier, "main") != 0) {
        fflush(stdout);
//        int CORE = *((int*)(0));
    }

    // JL REGSA = LIST OF REG IN USE AT START OF FN - IE ALREADY IN USE
    // JL REGS  = LIST OF REG IN USE DURING FN - SO INIT FROM REGSA
    for (c = 1; c <= MAXR; c++) regs[c] = regsa[c];

    maxpushed = 0;

    for (c = 1; c <= MAXR; c++) {
        if (regsa[c] || regused[c]) {
            BSET(regs_modified, c);
        }
    }

    localsize = (zm2l(offset) + 3) / 4 * 4;
    function_top(f, v, localsize);

    pushed = 0;

    for (; p; p = p->next) {

        c = p->code;

        emit(f, "\n; ------------------------------------------------------------------------  %s (%d)\n", ename[c], c);
        dumpIC(f, p);

        if (c == NOP) {
            p->z.flags = 0;
            continue;
        }
        if (c == ALLOCREG) {
            gc_allocreg(f, p);
            continue;
        }
        if (c == FREEREG) {
            gc_freereg(f, p);
            continue;
        }
        if (c == LABEL) {
            int t = p->typf;
            emit(f, "%s%d:\n", labprefix, t);
            continue;
        }
        if (c == BRA) {  // branch always - ie jump
            gc_bra(f, p);
            continue;
        }
        if (c >= BEQ && c < BRA) {
            gc_branch_conditional(f, p);

            continue;
        }
        if (c == MOVETOREG) {
            // MOVETOREG is used for register preservation

            if (isSpecialPurposeHalt(p->q1.reg)) {
                // MOVETOREG is generated for the halt registers because they are marked as regsa=1 AND also used as register parameters to the halt
                // functions so vbcc thinks it needs to preserve them on the way into the function
                emit(f, "; MOVETOREG  ignored for special purpose register %s\n", regnames[p->q1.reg]);
                continue;
            }
            /*
            emit(f, "MOVETOREG  REG=%d   Q1=%d  FLAGS=%d\n", p->z.reg, &p->q1, regtype[p->z.reg]->flags);
            load_reg(f, p->z.reg, &p->q1, regtype[p->z.reg]->flags);
            */
            continue;
        }
        if (c == MOVEFROMREG) {
            // MOVEFROMREG is used for register preservation

            if (isSpecialPurposeHalt(p->q1.reg)) {
                // MOVEFROMREG is generated for the halt registers because they are marked as regsa=1 AND also used as register parameters to the halt
                // functions so vbcc thinks it needs to preserve them on the way into the function
               emit(f, "; MOVEFROMREG  ignored for special purpose register %s\n", regnames[p->q1.reg]);
               continue;
            }
            emit(f, "MOVEFROMREG  NOT SUPPORTED\n");
            ierror(1);
            emit(f, "MOVEFROMREG  REG=%d   Q1=%d  FLAGS=%d\n", p->z.reg, &p->q1, regtype[p->z.reg]->flags);

            store_reg(f, p->z.reg, &p->q1, regtype[p->z.reg]->flags);
            continue;
        }

        // FIXME ??? NEEDED
        {
            //int t = p->typf;
            //if ((c == ASSIGN || c == PUSH) && ((t & NQ) > POINTER || ((t & NQ) == CHAR && zm2l(p->q2.val.vmax) != 1))) {
            //    // what is this
            //    emit(f, "ERROR IF (ASSIGN|PUSH) & (TYPE>POINTER|(TYPE=CHAR & VAL %d!=1))\n", zm2l(p->q2.val.vmax));
            //    ierror(0);
            // }
            /*
             int t = p->typf;
             if ((c == ASSIGN || c == PUSH) && ((t & NQ) > POINTER || ((t & NQ) == CHAR && zm2l(p->q2.val.vmax) != 1))) {
                 // what is this
                 emit(f, "ERROR IF (ASSIGN|PUSH) & (TYPE>POINTER|(TYPE=CHAR & VAL %d!=1))\n", zm2l(p->q2.val.vmax));
                 ierror(0);
             }
             */
        }

        if (c == CONVERT) {
            gc_convert(f, p);
            continue;
        }
        if (c == KOMPLEMENT) {
            ierror(1);
            /*
            int t = p->typf;
            emit(f, "KOMPLEMENT\n");

            int zreg = zReg(p);
            load_reg(f, zreg, &p->q1, t);
            emit(f, "\tcpl.%s\t%s\n", dt(t), regnames[zreg]);
            save_result(f, p, zreg);
             */
            continue;
        }
        if (c == SETRETURN) {
            gc_setreturn(f, p);

            continue;
        }
        if (c == GETRETURN) {
            gc_getreturn(f, p);

            continue;
        }
        if (c == CALL) {
            //if(argsize<zm2l(m->q2.val.vmax)) argsize=zm2l(m->q2.val.vmax);
            gc_call(f, p);
            //if (!calc_regs(p, f != 0) && v->fi) v->fi->flags &= ~ALL_REGS;
            continue;
        }
        if (c == PUSH) {
            gc_push(f, p);
            continue;
        }
        if (c == ASSIGN) {
            gc_assign(f, p);
            continue;
        }
        if (c == ADDRESS) {
            gc_address(f, p);
            continue;
        }
        if (c == MINUS) {
            ierror(1);
            /*
            int t = p->typf;
            emit(f, "MINUS\n");
            int zreg = zReg(p);
            load_reg(f, zreg, &p->q1, t);
            emit(f, "\tneg.%s\t%s\n", dt(t), regnames[zreg]);
            save_result(f, p, zreg);
             */
            continue;
        }
        if (c == TEST) {
            int t = p->typf;
            emit(f, "TEST\n");
            int zreg = zReg(p);
            emit(f, "\ttst.%s\t", dt(t));
            if (multiple_ccs)
                emit(f, "%s,", regnames[zreg]);
            emit_obj(f, &p->q1, t);
            emit(f, "\n");
            if (multiple_ccs)
                save_result(f, p, zreg);
            continue;
        }
        if (c == COMPARE) {
            gc_compare(f, p);
            continue;

        }


        if (c == SUBPFP || c == ADDI2P || c == SUBIFP || (c >= OR && c <= AND) || (c >= LSHIFT && c <= MOD)) {
            gc_arithmetic(f, p);
            continue;
        }

        // ELSE
        fprintf(stderr, "ERROR\n");
        fprintf(stderr, "CODE: %s\n", ename[p->code]);
        pric2(stdout, p);
        ierror(0);
    }

    emit(f, "\n; ------------------------------------------------------------------------  bottom\n");
    function_bottom(f, v, localsize);
    if (stack_valid) {
        if (!v->fi) v->fi = new_fi();
        v->fi->flags |= ALL_STACK;
        v->fi->stack1 = stack;
    }
    //emit(f, ";JL# stacksize=%lu%s\n", zum2ul(stack), stack_valid ? "" : "+??");

    dumpreg();

    emit(f, ";================================================================================= FUNCTION BOT %s\n", v->identifier);
}

/*
Compare and set condition codes. q1,q2(->z).

Compare the operands and set the condition code, so that BEQ, BNE, BLT, BGE, BLE or BGT works as desired.

 If z.flags == 0 (this is always the case unless the backend sets multiple_ccs to 1 and -no-multiple-ccs is not used)
the condition codes will be evaluated only by an IC immediately following
the COMPARE, i.e. the next instruction (except possible FREEREGs) will be a conditional branch.

 However, if a target supports several condition code registers and sets the global
variable multiple_ccs to 1, vbcc might use those registers and perform certain
optimizations. In this case z may be non-empty and the condition codes have to be stored in z.
Note that even if multiple_ccs is set, a backend must nevertheless be able to deal with z == 0.
 */
void gc_compare(FILE *f, struct IC *p) {
    int t = p->typf;
    emit(f, "; COMPARE START ========\n");

    emit(f, "\t; ORIGINAL ASM: \tcmp.%s\t", dt(t));
    emit_obj(f, &p->q1, t);
    emit(f, ",");
    emit_obj(f, &p->q2, t);
    emit(f, "\n");

    if (!ISINT(t)) {
        emit(f, "ONLY SUPPORT INT BUT GOT type:%s size:%d\n", dt(t), opsize(p));
        ierror(1);
    }

    struct obj *q1 = &p->q1;

    int q1Reg = getReg(&(p->q1));
    int q2Reg = getReg(&(p->q2));

    // const usually in q2, one source is always non-const
/*
fprintf(stderr, "OBJ DUMP: ");
pric2(stderr, p);

if (p->q1.flags) {
    fprintf(stderr, " @@q1 ");
    probj(stderr, &p->q1, 0);
    fprintf(stderr, "\n");
}
if (p->q2.flags) {
    fprintf(stderr, " @@q2 ");
    probj(stderr, &p->q2, 0);
    fprintf(stderr, "\n");
}
if (p->z.flags) {
    fprintf(stderr, " @@z ");
    probj(stderr, &p->z, 0);
    fprintf(stderr, "\n");
}

if (isreg(z)) fprintf(stderr, "z  ISREG (%s)\n", regnames[p->z.reg]);
if (isreg(q1)) fprintf(stderr, "q1 ISREG (%s)\n", regnames[p->q1.reg]);
if (isreg(q2)) fprintf(stderr, "q2 ISREG (%s)\n", regnames[p->q2.reg]);
if (isconst(z)) fprintf(stderr, "z  ISCONST (%s)\n", regnames[p->z.reg]);
if (isconst(q1)) fprintf(stderr, "q1 ISCONST (%s)\n", regnames[p->q1.reg]);
if (isconst(q2)) fprintf(stderr, "q2 ISCONST (%s)\n", regnames[p->q2.reg]);
if (isconstderef(z)) fprintf(stderr, "z  ISCONST|DREFOBJ (%s)\n", regnames[p->z.reg]);
if (isconstderef(q1)) fprintf(stderr, "q1 ISCONST|DREFOBJ (%s)\n", regnames[p->q1.reg]);
if (isconstderef(q2)) fprintf(stderr, "q2 ISCONST|DREFOBJ (%s)\n", regnames[p->q2.reg]);
if (isreg(q2)) fprintf(stderr, "q2 ISREG (%s)\n", regnames[p->q2.reg]);
if (isvar(q2)) fprintf(stderr, "q2 ISVAR|DREFOBJ (%s)\n", regnames[p->q2.reg]);
if (isvarreg(q2)) fprintf(stderr, "q2 ISVAR|REG (%s)\n", regnames[p->q2.reg]);
if (isregderef(q2)) fprintf(stderr, "q2 ISREG|DEREF (%s)\n", regnames[p->q2.reg]);

if (!isreg(q1)) fprintf(stderr, "ONLY handliong q1 as reg\n");
*/
    // current inst is a COMPARE but since SPAM1 is 8 bit I can't do a single compare of values.
// for multibyte values I need to know what kind of BR* instruction that will occur off the back of this COMPARE
// so that I can arrange a multibyte algorithm that sets the needed status reg flag.
    int branchType = findBranch(f, p);
    emit(f, "\t; BRANCH-TYPE-WILL-BE %s\n", ename[branchType]);

    int leftReg = R_GTMP1;
    char *leftRegName = regnames[leftReg];

    load_reg(f, leftReg, &p->q1, q1typ(p));
    BSET(regs_modified, leftReg); // sets the given bit in an array of bytes

    if (isReg(&p->q2)) {
        char *q2Regname = regnames[p->q2.reg];

        if (branchType == BEQ || branchType == BNE || branchType == BLT || branchType == BGT) {
            emit(f, "\t; NOTE ! This is a magnitude comparison NOT a subtraction so we start with the top digit\n");
            emit(f, "\tREGA=[:%s+3]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B_SIGNEDMAG [:%s+3] _S\n", q2Regname);
            emit(f, "\tREGA=[:%s+2]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B           [:%s+2] _EQ_S\n", q2Regname);
            emit(f, "\tREGA=[:%s+1]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B           [:%s+1] _EQ_S\n", q2Regname);
            emit(f, "\tREGA=[:%s+0]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B           [:%s+0] _EQ_S\n", q2Regname);
            emit(f, "\t; aggregate flags into register\n");
            emit(f, "\t; NOT NEEDED REGA=0\n");
            emit(f, "\t; NOT NEEDED REGA = REGA A_OR_B 1 _LT\n");
            emit(f, "\t; NOT NEEDED REGA = REGA A_OR_B 2 _GT\n");
            emit(f, "\t; NOT NEEDED REGA = REGA A_OR_B 4 _NE\n");
            emit(f, "\t; NOT NEEDED REGA = REGA A_OR_B 8 _EQ\n");
        } else {
            ierror(1);
        }
    } else if (isKonst(&p->q2)) {

        if (branchType == BEQ || branchType == BNE || branchType == BLT || branchType == BGT) {
            emit(f, "\t; NOTE ! This is a magnitude comparison NOT a subtraction so we start with the top digit\n");

            emit(f, "\tREGA=[:%s+3]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B_SIGNEDMAG $%02x _S\n", BYTE(extractByte(&p->q2.val, q2typ(p), 3)));

            emit(f, "\tREGA=[:%s+2]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B           $%02x _EQ_S\n", BYTE(extractByte(&p->q2.val, q2typ(p), 2)));

            emit(f, "\tREGA=[:%s+1]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B           $%02x _EQ_S\n", BYTE(extractByte(&p->q2.val, q2typ(p), 1)));

            emit(f, "\tREGA=[:%s+0]\n", leftRegName);
            emit(f, "\tNOOP = REGA A_MINUS_B           $%02x _EQ_S\n", BYTE(extractByte(&p->q2.val, q2typ(p), 0)));
        } else {
            ierror(1);
        }
    } else {
        fprintf(stderr, "COMPARE NOT SUPPORTED\n");
        ierror(1);
    }

// if(multiple_ccs) {
//     emit(f,"!!! %s,",regnames[zreg]);
// }
// NOT_USED = A - B
//           int isUnsigned =  A_MINIUS_B_SIGNEDMAG
/*
            if (ISINT(t)) { // should this be ISSCALAR??
                int datatypeSize = msizetab[BASETYPE(t)];
                int pos = 0;
                while (pos < datatypeSize) {
                    emit(f,"\t[:%s+%d] = ",regnames[r], pos);
                    emitvalJL(f,&o->val,type, pos);
                    emit(f,"\t\n");
                    pos++;
                }
                continue;
            }
            */
}

void gc_allocreg(FILE *f, struct IC *p) {
    fprintf(stderr, "; ALLOCREG - %s\n", regnames[getReg(&p->q1)]);
    regs[p->q1.reg] = 1;

    // logging into asm
    /*
   fprintf(stderr, "\t; allocated: ");
    int allocCount = 0;
    for (int r = 0; r < MAXR; r++) {
        if (regs[r]) {
            emit(f, "%s ", regnames[r]);
            allocCount++;
        }
    }
    if (!allocCount) {
        emit(f, "none");
    }
    emit(f, "\n");
     */
}

void gc_freereg(FILE *f, struct IC *p) {
    fprintf(stderr, "; FREEREG - %s\n", regnames[p->q1.reg]);
    regs[p->q1.reg] = 0;

    // logging into asm
    /*
    fprintf(stderr, "\t; freed: ");
    int allocCount = 0;
    for (int r = 0; r < MAXR; r++) {
        if (regs[r]) {
            emit(f, "%s ", regnames[r]);
            allocCount++;
        }
    }
    if (!allocCount) {
        emit(f, "none");
    }
    emit(f, "\n");
     */
}

void gc_setreturn(FILE *f, struct IC *p) {
    int t = p->typf;
    emit(f, "; SETRETURN - zreg = %s\n", regnames[p->z.reg]);

    // check size of data in instruction vs target register
    if (sizetab[ztyp(p) & NQ] > regsize[p->z.reg]) terror("-- cg_setreturn: size mismatch");

    load_reg(f, p->z.reg, &p->q1, t);

    // I assume set because the value carried here persists until the fn returns to caller
    // FIXME GLOBAL !
    BSET(regs_modified, p->z.reg); // sets the given bit in an array of bytes
}

void gc_branch_conditional(FILE *f, struct IC *p) {
    int c = p->code;
    // "typf specifies the label where program execution shall continue"
    int t = p->typf;
    emit(f, "; BRANCH BLOCK %s\n", ccs[c - BEQ]);
    char *flag = 0;
    if (c == BEQ)
        flag = "_EQ";
    else if (c == BNE)
        flag = "_NE";
    else if (c == BLT)
        flag = "_LT";
    else if (c == BGT)
        flag = "_GT";

    if (flag != 0) {
        emit(f, "\tPCHITMP = <:%s%d\n", labprefix, t);
        emit(f, "\tPC      = >:%s%d %s\n", labprefix, t, flag);
    } else if (c == BGE) {
        emit(f, "\tPCHITMP = <:%s%d\n", labprefix, t);
        emit(f, "\tPC      = >:%s%d _GT\n", labprefix, t);
        emit(f, "\tPC      = >:%s%d _EQ\n", labprefix, t);
    } else if (c == BLE) {
        emit(f, "\tPCHITMP = <:%s%d\n", labprefix, t);
        emit(f, "\tPC      = >:%s%d _LT\n", labprefix, t);
        emit(f, "\tPC      = >:%s%d _EQ\n", labprefix, t);
    } else {
        printf("ILLEGAL BRANCH COMMAND %d\n", c);
        ierror(0);
    }

    // emit(f,"\tb%s\t",ccs[c-BEQ]);
    if (isreg(q1)) {
        emit(f, "; BRANCH OBJ\n");
        emit_obj(f, &p->q1, 0);
        emit(f, ",");
    }
    emit(f, "; BRANCH TO LABEL %s%d\n", labprefix, t);
}

/*
Copy q1 to z. q1->z.
q2.val.vmax contains the size of the objects (this is necessary if it is an array or a struct).
It should be accessed using the opsize()-macro.
typf does not have to be an elementary type!
The only case where typf == ARRAY should be in automatic initializations.
It is also possible that (typf&NQ) == CHAR but the size is != 1. This is created for an inline memcpy/strcpy where the type is not known
 * */
void gc_assign(FILE *f, struct IC *p) {
    int t = p->typf;
    emit(f, "; ASSIGN type:%s size:%d\n", dt(t), opsize(p));

    //puts(*((char*)0));

    if (isReg(&p->z)) {
        int zreg = getReg(&p->z);
        emit(f, "\t; assign into a register\n");
        load_reg(f, zreg, &p->q1, t);

        if (isReg(&p->q1) && p->q1.reg == zreg) {
            // avoid copying reg to self if source is a reg and the same reg is the target
            return;
        }

        BSET(regs_modified, zreg);
    } else if (isVar(&p->z)) {
        // eg         DUMP z  < VAR( storage:auto:0(sp) ) >
        if (ISSCALAR(ztyp(p))) {
            emit(f, "\t; assign scalar into a var\n");

            emit(f, "; assign step 1 - load to temp reg\n");
            load_reg(f, R_GTMP2, &p->q1, q1typ(p));

            emit(f, "; assign step 2 - store to var\n");

            // save marlo which hold the SP
            emit(f, "\t; stash SP\n");
            emit(f, "\t[:sp_stash]   = MARLO\n");
            emit(f, "\t[:sp_stash+1] = MARHI\n");

            // adjust the MAR according to the offset
            int offset = real_offset(&p->z);
            if (offset != 0) {
                emit(f, "\t; adjust SP by offset %d to point to var\n", offset);
                emit(f, "\tMARLO = MARLO + (> %d) _S            ; add lo byte of offset\n", offset);
                emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C (< %d) ; add hi byte of offset plus any carry\n", offset);
            } else {
                emit(f, "\t; no need to adjust SP to point to var\n", offset);
            }
            // MAR now points at lsb base of value

            fprintf(stderr, "ISSCALAR=%d\n", BASETYPE(ISSCALAR(ztyp(p))));
            fprintf(stderr, "BTZ=%d\n", BASETYPE(ztyp(p)));
            fprintf(stderr, "BTQ1=%d\n", BASETYPE(q1typ(p)));
            int srcSize = msizetab[BASETYPE(q1typ(p))];
            if (srcSize == 0) {
                fprintf(stderr, "CANT COPY 0 BYTES\n");
                ierror(1);
            }

            emit(f, "\t; copy %d bytes from temp register to memory\n", srcSize);
            int pos = 0;
            while (pos < srcSize) {
                emit(f, "\tREGA  = [:%s+%d]\n", regnames[R_GTMP2], pos);
                emit(f, "\tRAM   = REGA\n");

                emit(f, "\tMARLO = MARLO + 1 _S\n");
                emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C 0\n");

                pos++;
            }

            // restore marlo which hold the SP
            emit(f, "\t; restore SP\n");
            emit(f, "\tMARLO = [:sp_stash]\n");
            emit(f, "\tMARHI = [:sp_stash+1]\n");
            return;

        } else if (ISARRAY(ztyp(p))) {

            // for example an array
            int bytesToCopy = opsize(p);
            if (bytesToCopy == 0) {
                fprintf(stderr, "CANT COPY 0 BYTES\n");
                ierror(1);
            }

            char srcLabel[10];
            sprintf(srcLabel, "%s%ld", labprefix, zm2l(p->q1.v->offset));

            // save mar which hold the SP
            emit(f, "\t; stash SP\n");
            emit(f, "\t[:sp_stash]   = MARLO\n");
            emit(f, "\t[:sp_stash+1] = MARHI\n");

            // adjust the MAR according to the targetOffset
            int targetOffset = real_offset(&p->z);
            if (targetOffset != 0) {
                emit(f, "\t; adjust SP by targetOffset %d to point to var\n", targetOffset);
                emit(f, "\tMARLO = MARLO + (> %d) _S            ; add lo byte of targetOffset\n", targetOffset);
                emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C (< %d) ; add hi byte of targetOffset plus any carry\n",
                     targetOffset);
            } else {
                emit(f, "\t; no need to adjust SP to point to var\n", targetOffset);
            }
            // MAR now points at lsb base of value

            emit(f, "\t; copy %d bytes from label %s to memory SP+%d\n", bytesToCopy, srcLabel, targetOffset);
            int pos = 0;
            while (pos < bytesToCopy) {
                emit(f, "\tREGA  = [:%s+%d]\n", srcLabel, pos);
                emit(f, "\tRAM   = REGA\n");

                emit(f, "\tMARLO = MARLO + 1 _S\n");
                emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C 0\n");

                pos++;
            }

            // restore marlo which hold the SP
            emit(f, "\t; restore SP\n");
            emit(f, "\tMARLO = [:sp_stash]\n");
            emit(f, "\tMARHI = [:sp_stash+1]\n");
            return;

        } else {
            terror("var Assign with unsupported src\n")
        }

    } else {
        terror("Assign with unsupported target\n")
    }
}

/*
Push q1 on the stack (for argument passing). q1.
q2.val.vmax contains the size of the object (should be accessed using the opsize()-macro),
z.val.vmax contains the size that has to be pushed (access it using the pushsize()-macro).
These sizes may differ due to alignment issues.
q1 does not have to be an elementary type (see ASSIGN).
Also, q1 can be empty. This is used for ABIs which require stack-slots to be omitted.

Depending on ORDERED_PUSH the PUSH ICs are generated starting with the first or the last arguments.
The direction of the stack-growth can be chosen by the backend.
Note that this is only used for function-arguments, they can be pushed in opposite direction of the real stack.
*/
void gc_push(FILE *f, struct IC *p) {
    int t = p->typf;

    if (isKonst(&p->q1)) {

        emit(f, "; PUSH KONST %d\n", p->q1.val.vmax);
        int pushSz = pushsize(p);

        int pos = 0;
        while (pos < pushSz) {
            // push so that high order bytes are higher in memory so that we keep to little endian for bytes everywhere
            int konstByte = BYTE(extractByte(&p->q1.val, q1typ(p), pushSz - pos - 1));
            emit(f, "\tRAM = $%02x\n", konstByte);

            emit(f, "\tMARLO = MARLO - 1 _S\n");
            emit(f, "\tMARHI = MARHI A_MINUS_B_MINUS_C 0\n");

            pos++;
        }

        // FIXME global
        pushed += pushSz;
    } else if (isReg(&p->q1)) {

        emit(f, "; PUSH REG %s\n", regnames[p->q1.reg]);
        int pushSz = pushsize(p);

        int pos = 0;
        while (pos < pushSz) {
            // push so that high order bytes are higher in memory so that we keep to little endian for bytes everywhere
            emit(f, "\tREGA = [:%s+%d]\n", regnames[p->q1.reg], pushSz - pos - 1);
            emit(f, "\tRAM = REGA\n");

            emit(f, "\tMARLO = MARLO - 1 _S\n");
            emit(f, "\tMARHI = MARHI A_MINUS_B_MINUS_C 0\n");

            pos++;
        }

        // FIXME global
        pushed += pushSz;
    } else {
        fprintf(stderr, "\nPUSH: not supported except for KONST and REG\n");
        ierror(1);
    }

}

void gc_arithmetic(FILE *f, struct IC *p) {
    int c = p->code;
    int t = p->typf;

    emit(f, "; ARITHMETIC : %s %s = %s - ", ename[c], regnames[p->z.reg], regnames[p->q1.reg]);
    emit_obj(f, &p->q2, t);
    emit(f, "\n");

    int targSize = msizetab[ztyp(p)];

    // Subtract a pointer from a pointer. q1,q2->z.
    // q1 and q2 are pointers (of type typf2) and z is an integer of type typf. z has
    // to be q1 - q2 in bytes.
    if (c == SUBPFP) c = SUB;

    // Add an integer to a pointer. q1,q2->z.
    // q1 and z are always pointers (of type typf2) and q2 is an integer of type typf.
    // z has to be q1 increased by q2 bytes.
    //if (c == ADDI2P) c = ADD;

    // Subtract an Integer from a pointer. q1,q2->z.
    // q1 and z are always pointers (of type typf2) and q2 is an integer of type typf.
    // z has to be q1 decreased by q2 bytes.
    // z(ptr) = q1(ptr) - q2(konst)
    if (c == SUBIFP) c = SUB;

    if (c == ADD || c == ADDI2P) {

        if (isKonst(&p->q2)) {
            int constSize = msizetab[q2typ(p)];
            if (constSize != targSize) {
                fprintf(stderr, "FIXME - CONST SIZE %d NOT SAME AS TARGET SIZE %d\n", constSize, targSize);
                ierror(1);
            }

            emit(f, "\t; clear flags\n");
            emit(f, "\tREGA = 0 _S\n");
            int pos = 0;
            emit(f, "\t; sum reg and konst\n");
            while (pos < constSize) {
                // can I avoid the sign extend stuff by preparing the extractByte properly to do extensions?

                emit(f, "\tREGA = [:%s+%d]\n", regnames[p->q1.reg], pos);
                int konstByte = BYTE(extractByte(&p->q2.val, q2typ(p), pos));
                emit(f, "\t[:%s+%d] = REGA A_PLUS_B_PLUS_C $%02x _S\n", regnames[p->z.reg], pos, konstByte);
                pos++;
            }

            /*
            int isSigned = c==ADD && ISSIGNED(p->z.flags); // add needs sign extension but not addi2p
            while(pos < targSize) {
                emit(f, "[:%s+%d] = 0; padding\n", regnames[p->z.reg]);

                if (isSigned) {
                    // extend sign left if this is signed negative value.
                    // last byte copied used the _S flag so the status register knows if it was negative.
                    // so optionally set it to FF if the NEG (_N) flag is set.
                    emit(f, "\t[:%s+%d] = $ff _N ; padding NEG as top bit of was a 1\n", regnames[p->z.reg], pos);
                }
            }
             */
        } else if (isReg(&p->q2)) {

            int srcSize = msizetab[q2typ(p)];
            if (srcSize != targSize) {
                fprintf(stderr, "FIXME - SRC REG SIZE %d NOT SAME AS TARGET SIZE %d\n", srcSize, targSize);
                ierror(1);
            }

            emit(f, "\t; clear flags\n");
            emit(f, "\tREGA = 0 _S\n");
            int pos = 0;
            emit(f, "\t; sum reg and reg\n");
            while (pos < srcSize) {
                // can I avoid the sign extend stuff by preparing the extractByte properly to do extensions?

                emit(f, "\tREGA = [:%s+%d]\n", regnames[p->q1.reg], pos);
                emit(f, "\tREGB = [:%s+%d]\n", regnames[p->q2.reg], pos);
                emit(f, "\t[:%s+%d] = REGA A_PLUS_B_PLUS_C REGB _S\n", regnames[p->z.reg], pos);
                pos++;
            }

            /*
            int isSigned = c==ADD && ISSIGNED(p->z.flags); // add needs sign extension but not addi2p
            while(pos < targSize) {
                emit(f, "[:%s+%d] = 0; padding\n", regnames[p->z.reg]);

                if (isSigned) {
                    // extend sign left if this is signed negative value.
                    // last byte copied used the _S flag so the status register knows if it was negative.
                    // so optionally set it to FF if the NEG (_N) flag is set.
                    emit(f, "\t[:%s+%d] = $ff _N ; padding NEG as top bit of was a 1\n", regnames[p->z.reg], pos);
                }
            }
             */
        } else {
            fprintf(stderr, "!!!!! q2 arith src %d not supported\n", p->q2.flags);
            ierror(1);
        }

        return;
    }

    fprintf(stderr, "!!!!! arith %s not supported\n", ename[p->code]);
    ierror(1);

    /*emit(f, "; OR AND SHIFT MOD \n");
    int zreg = zReg(p);

    if (c >= OR && c <= AND)
        emit(f, "\t; ORIG %s.%s\t%s,", logicals[c - OR], dt(t), regnames[zreg]);
    else
        emit(f, "\t; ORIG %s.%s\t%s,", arithmetics[c - LSHIFT], dt(t), regnames[zreg]);
    if (THREE_ADDR) {
        emit_obj(f, &p->q1, t);
        emit(f, ",");
    }
    emit_obj(f, &p->q2, t);
    emit(f, "\n");
    save_result(f, p, zreg);
     */
}

void gc_bra(FILE *f, struct IC *p) {
    int t = p->typf;
    emit(f, "; BRA %s%d\n", labprefix, t);
    emit(f, "\tPCHITMP = <:%s%d\n", labprefix, t);
    emit(f, "\tPC      = >:%s%d\n", labprefix, t);
}

/*
Get the address of an object. q1->z.
z is always a pointer and q1 is always an auto variable
 */
void gc_address(FILE *f, struct IC *p) {
    emit(f, "; ADDRESS todo\n");
    if (isReg(&p->z)) {
        int zreg = zReg(p);
        load_address(f, zreg, &p->q1, POINTER);
        //save_result(f, p, zreg);
        return;
    }

    terror("ADDRESS target type not handled\n");
}

/*
Convert one type to another. q1->z.
z is always of the type typf, q1 of type typf2.
Conversions between floating point and pointers do not occur, neither do conversions to and from structs, unions, arrays or void.
 */
void gc_convert(FILE *f, struct IC *p) {
    emit(f, "; CONVERT   z <- q1\n");
    ierror(1);

    int zreg = zReg(p);

    if (ISFLOAT(q1typ(p)) || ISFLOAT(ztyp(p))) ierror(0);

    // if source type is smaller than targ type
    if (sizetab[q1typ(p) & NQ] < sizetab[ztyp(p) & NQ]) {
        if (q1typ(p) & UNSIGNED)
            emit(f, "\tzext.%s\t%s\n", dt(q1typ(p)), regnames[zreg]);
        else
            emit(f, "\tsext.%s\t%s\n", dt(q1typ(p)), regnames[zreg]);
    }
    save_result(f, p, zreg);

}

/*
 * push FP
 * move SP -> FP locking the bottom (high address) of the frame
 *
 * */
void gc_call(FILE *f, struct IC *p) {
    emit(f, "; ---------------------------------\n");
    int t = p->typf;
    int bytesToRollback = pushedargsize(p);

    int reg;
    if ((p->q1.flags & (VAR | DREFOBJ)) == VAR && p->q1.v->fi && p->q1.v->fi->inline_asm) {
        char *inlineAsm = p->q1.v->fi->inline_asm;
        emit(f, "; CALL INLINE ASM : %s(..)\n", p->q1.v->identifier);
        printf("ASM: %s\n", inlineAsm); // the emit function doesn't echo the ASM so we'll fake it with a log line here
        emit_inline_asm(f, inlineAsm);
    } else {
        char returnLabel[10];
        sprintf(returnLabel, "%s%d", labprefix, ++label);
        emit(f, "; CALL %s(..) and return to %s\n", p->q1.v->identifier, returnLabel);
        emit(f, "\t; call\t");
        emit_obj(f, &p->q1, t);
        emit(f, "\n");
        pushed -= zm2l(p->q2.val.vmax);

        emit(f, "\t; vbcc assume 4 bytes of address pushed\n");
        emit(f, "\t; skip 2 return bytes\n");
        emit(f, "\tMARLO = MARLO - 2 _S\n");
        emit(f, "\tMARHI = MARHI A_MINUS_B_MINUS_C 0\n");
        emit(f, "\t; push return lo\n");
        emit(f, "\tRAM = (>:%s)\n", returnLabel);
        emit(f, "\tMARLO = MARLO - 1 _S\n");
        emit(f, "\tMARHI = MARHI A_MINUS_B_MINUS_C 0\n");
        emit(f, "\t; push return hi\n");
        emit(f, "\tRAM = (<:%s)\n", returnLabel);
        emit(f, "\tMARLO = MARLO - 1 _S\n");
        emit(f, "\tMARHI = MARHI A_MINUS_B_MINUS_C 0\n");

        char *id = p->q1.v->identifier;
        if (id == NULL) {
            fprintf(stderr, "\nNo subroutine identifier\n");
            ierror(1);
        }

        emit(f, "\t; call %s\n", id);
        emit(f, "\tPCHITMP = (<:%s%s)\n", idprefix, id);
        emit(f, "\tPC      = (>:%s%s)\n", idprefix, id);

        emit(f, "; --------------\n");
        emit(f, "\t; return location %s\n", returnLabel);
        emit(f, "%s:\n", returnLabel);

        emit(f, "\t; rewinding pushed args %d\n", bytesToRollback);
        emit(f, "\tMARLO = MARLO + %d _S\n", bytesToRollback);
        emit(f, "\tMARHI = MARHI A_PLUS_B_PLUS_C 0\n");

        //  TODO - unwind any pushed args to the function just returned from
        //  IE the size in PRIC2:  call function M0+_sub(sub) size=24 => sub

        // FIXME - something to do with caller saved regs???  - copied from some elses impl

        /*
        if ((p->q1.flags & (VAR | DREFOBJ)) == VAR && p->q1.v->fi && (p->q1.v->fi->flags & ALL_REGS)) {
            bvunite(regs_modified, p->q1.v->fi->regs_modified, RSIZE);
        } else {
            int i;
            for (i = 1; i <= MAXR; i++) {
                if (regscratch[i]) {
                    BSET(regs_modified, i);
                }
            }
        }
         */
    }


    emit(f, "\t; call unwound and complete\n", bytesToRollback);
    emit(f, "; ---------------------------------\n");
}

/*
    Get the return value of the last function call. ->z.
    If the return value is in a register, its number will be q1.reg.
    Otherwise q1.reg will be 0.
    GETRETURN immediately follows a CALL IC (except possible FREEREGs).
  */
void gc_getreturn(FILE *f, struct IC *p) {
    emit(f, "; GETRETURN\n");

    if (p->q1.reg) {
        /* Is the target a register ? */
        if (isReg(&p->z)) {

            int targReg = p->z.reg;
            int srcReg = p->q1.reg;

            char *srcRegName = regnames[srcReg];
            char *targRegName = regnames[targReg];
            int srcRegSize = regsize[srcReg];
            int targRegSize = regsize[targReg];
            memCopy(f,
                    ISSIGNED(p->z.flags),
                    targRegName,
                    srcRegName,
                    targRegSize,
                    srcRegSize);
        } else {
            fprintf(stderr, "target is not a register\n");
            ierror(0);
        }
    } else {
        //  p->z.flags = 0;
        fprintf(stderr, "src q1 is not a register\n");
        ierror(0);
    }
}

int shortcut(int code, int typ) {
    return 0;
}

/*
int reg_parm(struct reg_handle *m, struct Typ *t, int vararg, struct Typ *d) {
    int f;
    f = t->flags & NQ;
    if (f <= LONG || f == POINTER) {
        if (m->gregs >= GPR_ARGS)
            return 0;
        else
            return R_G0 + m->gregs++;
            //return R_G0 + 3 + m->gregs++;
    }
    if (ISFLOAT(f)) {
        if (m->fregs >= FPR_ARGS)
            return 0;
        else
            return R_F0 + 2 + m->fregs++;
    }
    return 0;
}
*/
int handle_pragma(const char *s) {
}

void cleanup_cg(FILE *f) {
    // write all the registers at the bottom of the asm
    emit(f, "; registers\n");
    // don't include the "noreg" as we don't want it used
    for (int i = 0; i <= MAXR; i++) {
        if (i != R_NONE) {
            emit(f, "\t%-10s:\tRESERVE %d\n", regnames[i], regsize[i]);
        }
    }
    emit(f, "END\n");
}

void cleanup_db(FILE *f) {
    //if (f) section = -1;
}

void dumpObj(FILE *f, char *label, struct obj *o, int otyp, struct IC *p) {
    fflush(f);
    emit(f, "\t;   DUMP  < [%s]", label);

    if (o->flags == 0) {
        emit(f, " FLAG:%d ", o->flags);
    }

    if (o->flags == KONST) {  // const val
        emit(f, " KONST:");
        emitval(f, &o->val, otyp);
    }
    if (o->flags == (KONST | DREFOBJ)) {  // const pointer
        emit(f, " KONST|DREFOBJ:");
        emitval(f, &o->val, otyp);
    }
    if (o->flags & REG) {
        emit(f, " REG(%s)", regnames[o->reg]);
    }
    if (o->flags & DREFOBJ) {
        emit(f, " DREFOBJ");
    }
    if (o->flags & VARADR) {
        emit(f, " VARADR");
    }
    if (o->flags & VAR) {
        emit(f, " VAR(");

        emit(f, " storage:");
        if (o->v->storage_class == AUTO) {
            emit(f, "auto");
            emit(f, ":%ld(%s)", real_offset(o), regnames[SP]);
        } else if (o->v->storage_class == REGISTER) {
            emit(f, "register");
            emit(f, ":%ld(%s)", real_offset(o), regnames[SP]);
        } else {
            // add sign
            if (!zmeqto(l2zm(0L), o->val.vmax)) {
                emitval(f, &o->val, LONG);
                emit(f, "+");
            }
            if (o->v->storage_class == STATIC) {
                emit(f, "static:%s%ld", labprefix, zm2l(o->v->offset));
            } else if (o->v->storage_class == EXTERN) {
                emit(f, "extern:%s%ld", labprefix, zm2l(o->v->offset));
            } else {
                emit(f, "nonstatic:%s%s", idprefix, o->v->identifier);
            }
        }
        emit(f, " )");
    }
    //fprintf(f, " >");
    fflush(f);
    char *id = NULL;
    if (p->code != ALLOCREG && p->code != FREEREG) {
        if (involvesVar(o)) {
            //if (o == &p->q1) // identifier is always on q1
            if (o->flags != KONST && o->flags != 0) { // simple konst passed to a function as an arg doesn't have a name
                if (o->v != NULL) {
                    if (o->v->identifier != NULL) {
                        id = o->v->identifier;
                    }
                }
            }
        }
    }


//    char* id = ((o==NULL)? "ONULL": ((o->v==NULL)?"VNULL": (o->v->identifier)==NULL?"INULL":o->v->identifier));
    if (id != NULL) emit(f, " '%s' ", id);
    fflush(stdout);
}

void dumpIC(FILE *f, struct IC *p) {
    fflush(f);
    fflush(stdout);

    int c = p->code;

    if (p->file) emit(f, "; location %s:%d\n", p->file, p->line);

    if (c == ALLOCREG || c == FREEREG) {
        return;
    }

    fprintf(stdout, "\t; PRIC2: ");
    pric2(stdout, p);
    emit(stdout, "\n");

    emit(f, "\t; DUMP code = %s    typf=%s  typf2=%s   ztyp=%s  q1typ=%s  q2typ=%s\n", ename[p->code],
         dt(p->typf), dt(p->typf2),
         dt(ztyp(p)), dt(q1typ(p)), dt(q2typ(p)));

    dumpObj(f, "z", &p->z, ztyp(p), p);
    emit(f, "\n");

    dumpObj(f, "q1", &p->q1, q1typ(p), p);
    emit(f, "\n");

    dumpObj(f, "q2", &p->q2, q2typ(p), p);
    emit(f, "\n");

    emit(f, "; -----\n");
}

int reg_parm(struct reg_handle *m, struct Typ *t, int vararg, struct Typ *d) {
    int f;
    f = t->flags & NQ;
    if (is_varargs(d))    /* Disallow register parameters for varargs functions */
        return (0);

    if (f <= LONG || f == POINTER) {
        if (m->gregs >= REGPARM_COUNT)
            return 0;
        else
            return R_GA;
    }
    if (ISFLOAT(f)) {
        return (0);
/*		if (m->fregs >= 0)
			return 0;
		else
			return FIRST_FPR + 2 + m->fregs++;
*/
    }
    return 0;
}

void dumpreg() {
    //printf("DUMPREG - DISABLED\n");
    //return;

    printf("DUMPREG..\n");
    printf("%-10s %-8s %-8s %-8s %-8s %-8s\n", "name", "regsa", "regused", "regs", "mod'd", "scratch");
    for (int c = 1; c <= MAXR; c++) {
        if (regsa[c] || regused[c] || regs[c] || BTST(regs_modified, c)) {
            printf("%-10s ", regnames[c]);
            if (regsa[c]) {
                printf("%-8s ", "regsa");
            } else {
                printf("%-8s ", "-");
            }
            if (regused[c]) {
                printf("%-8s ", "used");
            } else {
                printf("%-8s ", "-");
            }
            if (regs[c]) {
                printf("%-8s ", "regs");
            } else {
                printf("%-8s ", "-");
            }
            if (BTST(regs_modified, c)) {
                printf("%-8s ", "mod");
            } else {
                printf("%-8s ", "-");
            }
            if (regscratch[c]) {
                printf("%-8s ", "scratch");
            } else {
                printf("%-8s ", "-");
            }
            printf("\n");
        }
    }
}
