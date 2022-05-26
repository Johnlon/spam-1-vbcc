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
 * */
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

//#include "supp.h"
#include "../../supp.h"
//#include "machine.h"

#define BASETYPE(x) ((x & NQ))

//#define isRegP(x) ((x->flags & (REG | DREFOBJ)) == REG)
#define isConst(x) ((x.flags & (KONST | DREFOBJ)) == KONST)

int isRegP(struct obj *x) {
    return ((x->flags & (REG | DREFOBJ)) == REG);
}
// is a KONST but not DEREFOBJ
#define isconst(x) ((p->x.flags & (KONST | DREFOBJ)) == KONST)
// is a KONST pointer
#define isconstderef(x) ((p->x.flags & (KONST | DREFOBJ)) == (KONST|DREFOBJ))
// is a REG but not DEREFOBJ
#define isreg(x) ((p->x.flags & (REG | DREFOBJ)) == REG)
// is a VAR that's not a REG
#define isregderef(x) ((p->x.flags & (REG | DREFOBJ)) == (REG|DREFOBJ))
#define isvar(x) ((p->x.flags & (VAR | REG)) == VAR)
// is a VAR and REG
#define isvarreg(x) ((p->x.flags & (VAR | REG)) == (VAR|REG))

int getReg(struct obj *o) {
    if (isRegP(o))
        return o->reg;
    return 0;
}

void dumpIC(FILE *f, struct IC *p);

void dumpObj(FILE *f, struct obj *o, int otyp);
static long real_offset(struct obj *o);

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

/*
#define THREE_ADDR (g_flags[0] & USEDFLAG)
#define LOAD_STORE (g_flags[1] & USEDFLAG)
#define VOL_GPRS ((g_flags[2] & USEDFLAG) ? g_flags_val[2].l : NUM_GPRS / 2)
#define VOL_FPRS ((g_flags[3] & USEDFLAG) ? g_flags_val[3].l : NUM_FPRS / 2)
//#define VOL_CCRS   ((g_flags[4]&USEDFLAG)?g_flags_val[4].l:NUM_CCRS/2)
//#define IMM_IND    ((g_flags[5]&USEDFLAG)?1:0)
//#define GPR_IND    ((g_flags[6]&USEDFLAG)?2:0)
#define GPR_ARGS ((g_flags[7] & USEDFLAG) ? g_flags_val[7].l : 0)
#define FPR_ARGS ((g_flags[8] & USEDFLAG) ? g_flags_val[8].l : 0)
#define USE_COMMONS (g_flags[9] & USEDFLAG)
*/


#define LOAD_STORE 0
#define THREE_ADDR 1
#define VOL_GPRS   (NUM_GPRS/2)
#define VOL_FPRS   (NUM_FPRS/2)
#define VOL_CCRS   (NUM_CCRS/2)
#define IMM_IND    (0)
#define GPR_IND    (0)
#define GPR_ARGS   (0)
#define FPR_ARGS   (0)
#define USE_COMMONS (0)

// JL aligns with the indexex of the types in supp.h but index 0 is filled with a non-reg called "noreg" whose purpose I don't understand
/* alignment of basic data-types, used to initialize align[] */
static long malign[MAX_TYPE + 1] = {1, 1, 2, 4, 4, 4, 4, 8, 8, 1, 4, 1, 1, 1, 4, 1};
/* sizes of basic data-types, used to initialize sizetab[] */
static long msizetab[MAX_TYPE + 1] = {1, 1, 2, 4, 4, 8, 4, 8, 8, 0, 4, 0, 0, 0, 4, 0};

/* used to initialize regtyp[] */
static struct Typ ltyp = {LONG}, ldbl = {DOUBLE}, lchar = {CHAR};

/* macros defined by the backend */
static char *marray[] = {"__section(x)=__vattr(\"section(\"#x\")\")",
                         "__GENERIC__",
                         0};

/* special registers */
static int sp;         /*  Stackpointer                        */
static int t1, t2;//, t3; /*  temporary gprs */
static int f1, f2;//, f3; /*  temporary fprs */

#define dt(t) (((t)&UNSIGNED) ? udt[(t)&NQ] : sdt[(t)&NQ])
static char *sdt[MAX_TYPE + 1] = {"??", "c", "s", "i", "l", "ll", "f", "d", "ld", "v", "p"};
static char *udt[MAX_TYPE + 1] = {"??", "uc", "us", "ui", "ul", "ull", "f", "d", "ld", "v", "p"};

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
static char *ret;

/* label at the end of the function (if any) */
static int exit_label;

/* assembly-prefixes for labels and external identifiers */
static char *labprefix = "l", *idprefix = "_";

#if FIXED_SP
/* variables to calculate the size and partitioning of the stack-frame
   in the case of FIXED_SP */
static long frameoffset, pushed, maxpushed, framesize;
#else
/* variables to keep track of the current stack-offset in the case of
   a moving stack-pointer */
static long notpopped, dontpop, stackoffset, maxpushed;
#endif

static long localsize, rsavesize, argsize;

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
   ------------------------------------------------
   | arguments to this function                   |
   ------------------------------------------------
   | return-address [size=4]                      |
   ------------------------------------------------
   | caller-save registers [size=rsavesize]       |
   ------------------------------------------------
   | local variables [size=localsize]             |
   ------------------------------------------------
   | arguments to called functions [size=argsize] |
   ------------------------------------------------
   All sizes will be aligned as necessary.
   In the case of FIXED_SP, the stack-pointer will be adjusted at
   function-entry to leave enough space for the arguments and have it
   aligned to 16 bytes. Therefore, when calling a function, the
   stack-pointer is always aligned to 16 bytes.
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
        /* function parameter */
        off = localsize + rsavesize + 4 - off - zm2l(maxalign);
    }

#if FIXED_SP
    off += argsize;
#else
    off += stackoffset;
#endif
    off += zm2l(o->val.vmax);
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

/* generate code to load the address of a variable into register r */
static void load_address(FILE *f, int r, struct obj *o, int type)
/*  Generates code to load the address of a variable into register r.   */
{
    if (!(o->flags & VAR)) ierror(0);
    if (o->v->storage_class == AUTO || o->v->storage_class == REGISTER) {
        long off = real_offset(o);
        if (THREE_ADDR) {
            emit(f, "\tadd.%s\t%s,%s,%ld\n", dt(POINTER), regnames[r], regnames[sp], off);
        } else {
            emit(f, "\tmov.%s\t%s,%s\n", dt(POINTER), regnames[r], regnames[sp]);
            if (off)
                emit(f, "\tadd.%s\t%s,%ld\n", dt(POINTER), regnames[r], off);
        }
    } else {
        emit(f, "\tmov.%s\t%s,", dt(POINTER), regnames[r]);
        emit_obj(f, o, type);
        emit(f, "\n");
    }
}

void emitzmJL(FILE *f, long x, int byteToEmit) {
    int mask = 0xff << (byteToEmit * 8);

    // LSB = byte 0
    emit(f, "#%02x", (unsigned char) ((x & mask) >> (byteToEmit * 8)));
}

void emitzumJL(FILE *f, unsigned long x, int byteToEmit) {
    int mask = 0xff << (byteToEmit * 8);

    // LSB = byte 0
    emit(f, "#%02x", (unsigned char) ((x & mask) >> (byteToEmit * 8)));
}

void emitvalJL(FILE *f, union atyps *p, int t, int byteToEmit)
/*  Gibt atyps aus.                                     */
{
    zmax vmax;
    zumax vumax;
    zldouble vldouble;

    t &= NU;
    if (t == CHAR) {
        vmax = zc2zm(p->vchar);
        emitzmJL(f, vmax, byteToEmit);
    } else if (t == SHORT) {
        vmax = zs2zm(p->vshort);
        emitzmJL(f, vmax, byteToEmit);
    } else if (t == INT) {
        vmax = zi2zm(p->vint);
        emitzmJL(f, vmax, byteToEmit);
    } else if (t == LONG) {
        vmax = zl2zm(p->vlong);
        emitzmJL(f, vmax, byteToEmit);
    } else if (t == LLONG) {
        vmax = zll2zm(p->vllong);
        emitzmJL(f, vmax, byteToEmit);
    } else if (t == MAXINT) {
        emit(f, "JL - NO IDEA WHAT MAXINT MEANS");
        emitzmJL(f, p->vmax, byteToEmit);
    } else if (t == (UNSIGNED | CHAR)) {
        vumax = zuc2zum(p->vuchar);
        emitzumJL(f, vumax, byteToEmit);
    } else if (t == (UNSIGNED | SHORT)) {
        vumax = zus2zum(p->vushort);
        emitzumJL(f, vumax, byteToEmit);
    } else if (t == (UNSIGNED | INT)) {
        vumax = zui2zum(p->vuint);
        emitzumJL(f, vumax, byteToEmit);
    } else if (t == (UNSIGNED | LONG)) {
        vumax = zul2zum(p->vulong);
        emitzumJL(f, vumax, byteToEmit);
    } else if (t == (UNSIGNED | LLONG)) {
        vumax = zull2zum(p->vullong);
        emitzumJL(f, vumax, byteToEmit);
    } else if (t == (UNSIGNED | MAXINT)) {
        emitzumJL(f, p->vumax, byteToEmit);
    } else {
        emit(f, "JL - NOT HANDLED TYPE %d\n", t);
        if (t == FLOAT) {
            vldouble = zf2zld(p->vfloat);
            emitzld(f, vldouble);
        } else if (t == DOUBLE) {
            vldouble = zd2zld(p->vdouble);
            emitzld(f, vldouble);
        } else if (t == LDOUBLE) {
            emitzld(f, p->vldouble);
        }

            /*FIXME*/
        else if (t == POINTER) {
            vumax = zul2zum(p->vulong);
            emitzum(f, vumax);
        } else
            emit(f, "JL - NOT KNOWN TYPE %d\n", t);
    }
}

/* Generates code to load a memory object into register r. tmp is a
   general purpose register which may be used. tmp can be r. */
static void load_reg(FILE *f, int r, struct obj *o, int type) {
    type &= NU;
    if (o->flags & VARADR) {
        load_address(f, r, o, POINTER);
    } else {
        // if((o->flags&(REG|DREFOBJ))==REG&&o->reg==r)
        if (isRegP(o) && o->reg == r) {
            // avoid copying reg to self if source is a reg and the same reg is the target
            return;
        }

        // if((o->flags&(KONST|DREFOBJ))==(KONST|DREFOBJ)){
        if (ISINT(type)) {  // should this be ISSCALAR??
            int datatypeSize = msizetab[BASETYPE(type)];
            int pos = 0;
            while (pos < datatypeSize) {
                emit(f, "\t[:%s+%d] = ", regnames[r], pos);
                emitvalJL(f, &o->val, type, pos);
                emit(f, "\t\n");
                pos++;
            }
            return;
        }

        emit(f, "\tNOT HANDLED FOR mov.%s\t%s,", dt(type), regnames[r]);
        emit(f, "\tmov.%s\t%s,", dt(type), regnames[r]);
        emit_obj(f, o, type);

        emit(f, "\n");
    }
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

static struct IC *preload(FILE *, struct IC *);

static void function_top(FILE *, struct Var *, long);

static void function_bottom(FILE *f, struct Var *, long);

void gc_getreturn(FILE *f, struct IC *p);

void gc_call(FILE *f, struct IC *p);

static int q1reg, q2reg, zreg;

static char *ccs[] = {"eq", "ne", "lt", "ge", "le", "gt", ""};
static char *logicals[] = {"or", "xor", "and"};
static char *arithmetics[] = {"slw", "srw", "add", "sub", "mullw", "divw", "mod"};

/* compare if two objects are the same */
static int compare_objects(struct obj *o1, struct obj *o2) {
    // if((o1->flags&(REG|DREFOBJ))==REG && ((o2->flags&(REG|DREFOBJ))==REG) && (o1->reg==o2->reg)) {
    if (isRegP(o1) && isRegP(o2) && (o1->reg == o2->reg)) {
        return 1;
    }

    // same object type and same address mode - am doesn't apply to SPAM (always null)
    if (o1->flags == o2->flags && o1->am == o2->am) {
        // if (LHS is NOT a variable) OR (L & R have exactly same value  AND size is same ie vmax==vmax)
        // I don't understand sifnificance of checking the vmax (len)
        if (!(o1->flags & VAR) || (o1->v == o2->v && zmeqto(o1->val.vmax, o2->val.vmax))) {
            // if LHS is not a REG (in which case reg is irrelevant) OR it is REG and the two reg are the same
            if (!(o1->flags & REG) || o1->reg == o2->reg) {
                return 1;
            }
        }
    }
    return 0;
}

/* Does some pre-processing like fetching operands from memory to
   registers etc. */
// sets varios global variables that then influence gen_code << all incomprehensibe as usual
static struct IC *preload(FILE *f, struct IC *p) {
    int r;

    if (isreg(q1))
        q1reg = p->q1.reg;
    else
        q1reg = 0;

    if (isreg(q2))
        q2reg = p->q2.reg;
    else
        q2reg = 0;

    if (isreg(z) && (THREE_ADDR || !compare_objects(&p->q2, &p->z))) {
        zreg = p->z.reg;
    } else {
        if (ISFLOAT(ztyp(p)))
            zreg = f1;
        else
            zreg = t1;
    }

    if ((p->q1.flags & (DREFOBJ | REG)) == DREFOBJ && !p->q1.am) {
        p->q1.flags &= ~DREFOBJ;
        load_reg(f, t1, &p->q1, q1typ(p));
        p->q1.reg = t1;
        p->q1.flags |= (REG | DREFOBJ);
    }
    if (p->q1.flags && LOAD_STORE && !isreg(q1)) {
        if (p->code == ASSIGN && isreg(z))
            q1reg = p->z.reg;
        else if (ISFLOAT(q1typ(p)))
            q1reg = f1;
        else
            q1reg = t1;
        load_reg(f, q1reg, &p->q1, q1typ(p));
        p->q1.reg = q1reg;
        p->q1.flags = REG;
    }

    if ((p->q2.flags & (DREFOBJ | REG)) == DREFOBJ && !p->q2.am) {
        p->q2.flags &= ~DREFOBJ;
        load_reg(f, t1, &p->q2, q2typ(p));
        p->q2.reg = t1;
        p->q2.flags |= (REG | DREFOBJ);
    }
    if (p->q2.flags && LOAD_STORE && !isreg(q2)) {
        if (ISFLOAT(q2typ(p)))
            q2reg = f2;
        else
            q2reg = t2;
        load_reg(f, q2reg, &p->q2, q2typ(p));
        p->q2.reg = q2reg;
        p->q2.flags = REG;
    }
    return p;
}

/* save the result (in zreg) into p->z */
void save_result(FILE *f, struct IC *p) {
    if ((p->z.flags & (REG | DREFOBJ)) == DREFOBJ && !p->z.am) {
        p->z.flags &= ~DREFOBJ;
        load_reg(f, t2, &p->z, POINTER);
        p->z.reg = t2;
        p->z.flags |= (REG | DREFOBJ);
    }
    if (isreg(z)) {
        if (p->z.reg != zreg)
            emit(f, "\tmov.%s\t%s,%s\n", dt(ztyp(p)), regnames[p->z.reg], regnames[zreg]);
    } else {
        store_reg(f, zreg, &p->z, ztyp(p));
    }
}

/* prints an object */
// !!!!!!!!!!! was working on load_reg and want to copy over bte by byte for types longer than 1
// but need emit obj to elit those byte sized portions

static void emit_objJL(FILE *f, struct obj *p, int t, int offset) {
    if ((p->flags & (KONST | DREFOBJ)) == (KONST | DREFOBJ)) {  // const pointer
        emitvalJL(f, &p->val, p->dtyp & NU, offset);
        return;
    }
    if (p->flags & DREFOBJ || p->flags & REG) emit(f, "[");  // non-const pointer follows

    if (p->flags & REG) {
        emit(f, ":%s + %d", regnames[p->reg], offset);
    } else if (p->flags & VAR) {
        if (p->v->storage_class == AUTO || p->v->storage_class == REGISTER) {
            emit(f, "NOT IMPL VAR AUTO\n");
            emit(f, "%ld(%s)", real_offset(p), regnames[sp]);
        } else {
            emit(f, "NOT IMPL VAR\n");
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
        // value is an ordinary const - (can't be a const pointer as that would have be triggered above)
        emitvalJL(f, &p->val, t & NU, offset);
    }
    if (p->flags & DREFOBJ || p->flags & REG) emit(f, "]");  // non-const pointer was previous
}

static void emit_obj(FILE *f, struct obj *p, int t) {
    /* NOT USED
    if(p->am){
        if(p->am->flags&GPR_IND) emit(f,"(%s,%s)",regnames[p->am->offset],regnames[p->am->base]);
        if(p->am->flags&IMM_IND) emit(f,"(%ld,%s)",p->am->offset,regnames[p->am->base]);
        return;
    }
    */
    if ((p->flags & (KONST | DREFOBJ)) == (KONST | DREFOBJ)) {
        emitval(f, &p->val, p->dtyp & NU);
        return;
    }
    if (p->flags & DREFOBJ) emit(f, "(");
    if (p->flags & REG) {
        emit(f, "%s", regnames[p->reg]);
    } else if (p->flags & VAR) {
        if (p->v->storage_class == AUTO || p->v->storage_class == REGISTER)
            emit(f, "%ld(%s)", real_offset(p), regnames[sp]);
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


/* generates the function entry code */
static void function_top(FILE *f, struct Var *v, long offset) {
    rsavesize = 0;
    if (!special_section(f, v) && section != CODE) {
        emit(f, codename);
        if (f) section = CODE;
    }
    if (v->storage_class == EXTERN) {
        if ((v->flags & (INLINEFUNC | INLINEEXT)) != INLINEFUNC)
            emit(f, "\t.global\t%s%s\n", idprefix, v->identifier);
        emit(f, "%s%s:\n", idprefix, v->identifier);
    } else
        emit(f, "%s%ld:\n", labprefix, zm2l(v->offset));
}

/* generates the function exit code */
static void function_bottom(FILE *f, struct Var *v, long offset) {
    emit(f, ret);
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
    maxalign = l2zm(8L);
    char_bit = l2zm(8L);
    stackalign = l2zm(4);

    for (i = 0; i <= MAX_TYPE; i++) {
        sizetab[i] = l2zm(msizetab[i]);
        align[i] = l2zm(malign[i]);
    }

    regnames[0] = "noreg"; // when a register isn't in effect then the reg id is 0 and so it's convenient that there is a distinctive name at that ordinal
    for (i = R_G0; i <= R_GF; i++) {
        regnames[i] = mymalloc(10);
        sprintf(regnames[i], "gpr%d", i - R_G0);
        regsize[i] = l2zm(4L);
        regtype[i] = &ltyp;
    }
    for (i = R_F0; i <= R_FF; i++) {
        regnames[i] = mymalloc(10);
        sprintf(regnames[i], "fpr%d", i - R_F0);
        regsize[i] = l2zm(8L);
        regtype[i] = &ldbl;
    }

    regnames[SP] = mymalloc(10);
    sprintf(regnames[SP], "sp");
    regsize[SP] = l2zm(4L);
    regtype[SP] = &ltyp;

    regnames[FP] = mymalloc(10);
    sprintf(regnames[FP], "fp");
    regsize[FP] = l2zm(4L);
    regtype[FP] = &ltyp;

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
    t1 = R_G0;
    t2 = R_G0 + 1;
    f1 = R_F0;
    f2 = R_F0 + 1;
    regsa[t1] = regsa[t2] = 1;
    regsa[f1] = regsa[f2] = 1;
    regsa[SP] = 1;
    regscratch[t1] = regscratch[t2] = 0;
    regscratch[f1] = regscratch[f2] = 0;
    regscratch[SP] = 0;

    for (i = R_G0; i <= R_GF - VOL_GPRS; i++)
        regscratch[i] = 1;
    for (i = R_F0; i <= R_FF - VOL_FPRS; i++)
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
        return R_F0 + 2;
    if (ISSTRUCT(t->flags) || ISUNION(t->flags))
        return 0;
    if (zmleq(szof(t), l2zm(4L)))
        return R_G0 + 3;
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
int cost_savings(struct IC *p, int r, struct obj *o) {
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
    if (t == POINTER && r >= R_G0 && r <= R_GF)
        return 1;
    if (t >= CHAR && t <= LONG && r >= R_G0 && r <= R_GF)
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
/*  This function has to create <size> bytes of storage */
/*  initialized with zero.                              */
{
    if (newobj && section != SPECIAL)
        emit(f, "%ld\n", zm2l(size));
    else
        emit(f, "\t.space\t%ld\n", zm2l(size));
    newobj = 0;
}

void gen_align(FILE *f, zmax align)
/*  This function has to make sure the next data is     */
/*  aligned to multiples of <align> bytes.              */
{
    if (zm2l(align) > 1) emit(f, "\t.align\t2\n");
}

void gen_var_head(FILE *f, struct Var *v)
/*  This function has to create the head of a variable  */
/*  definition, i.e. the label and information for      */
/*  linkage etc.                                        */
{

    emit(f, "\n;JL gen_var_head\n");

    int constflag;
    char *sec;
    if (v->clist) constflag = is_const(v->vtyp);
    if (v->storage_class == STATIC) {
        if (ISFUNC(v->vtyp->flags)) return;
        if (!special_section(f, v)) {
            if (v->clist && (!constflag || (g_flags[2] & USEDFLAG)) && section != DATA) {
                emit(f, dataname);
                if (f) section = DATA;
            }
            if (v->clist && constflag && !(g_flags[2] & USEDFLAG) && section != RODATA) {
                emit(f, rodataname);
                if (f) section = RODATA;
            }
            if (!v->clist && section != BSS) {
                emit(f, bssname);
                if (f) section = BSS;
            }
        }
        if (v->clist || section == SPECIAL) {
            gen_align(f, falign(v->vtyp));
            emit(f, "%s%ld:\n", labprefix, zm2l(v->offset));
        } else
            emit(f, "\t.lcomm\t%s%ld,", labprefix, zm2l(v->offset));
        newobj = 1;
    }
    if (v->storage_class == EXTERN) {
        emit(f, "\t.globl\t%s%s\n", idprefix, v->identifier);
        if (v->flags & (DEFINED | TENTATIVE)) {
            if (!special_section(f, v)) {
                if (v->clist && (!constflag || (g_flags[2] & USEDFLAG)) && section != DATA) {
                    emit(f, dataname);
                    if (f) section = DATA;
                }
                if (v->clist && constflag && !(g_flags[2] & USEDFLAG) && section != RODATA) {
                    emit(f, rodataname);
                    if (f) section = RODATA;
                }
                if (!v->clist && section != BSS) {
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
}

void gen_dc(FILE *f, int t, struct const_list *p)
/*  This function has to create static storage          */
/*  initialized with const-list p.                      */
{
    emit(f, "\tdc.%s\t", dt(t & NQ));
    if (!p->tree) {
        if (ISFLOAT(t)) {
            /*  auch wieder nicht sehr schoen und IEEE noetig   */
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

void gen_code(FILE *f, struct IC *p, struct Var *v, zmax offset)
/*  The main code-generation.                                           */
{
    int c, t, i;
    struct IC *m;
    argsize = 0;
    if (DEBUG & 1) printf("gen_code()\n");

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
#if FIXED_SP
    /*FIXME: adjust localsize to get an aligned stack-frame */
#endif

    function_top(f, v, localsize);

#if FIXED_SP
    pushed = 0;
#endif

    for (; p; p = p->next) {

        c = p->code;
        t = p->typf;

        printf("\n=================================================================== NEW INST %s\n", ename[c]);
        emit(f, "\n; ------ %s\n", ename[c]);

        printf("\nDUMP...\n");
        dumpIC(stdout, p);
        printf("PRIC2...\n");
        pric2(stdout, p);
        printf("-----\n");

        if (c == NOP) {
            p->z.flags = 0;
            continue;
        }
        if (c == ALLOCREG) {
            emit(f, "; ALLOCREG - %s\n", regnames[p->q1.reg]);
            regs[p->q1.reg] = 1;
            continue;
        }
        if (c == FREEREG) {
            emit(f, "; FREEREG - %s\n", regnames[p->q1.reg]);
            regs[p->q1.reg] = 0;
            continue;
        }
        if (c == LABEL) {
            emit(f, "%s%d:\n", labprefix, t);
            continue;
        }
        if (c == BRA) {  // branch always
            if (0 /*t==exit_label&&framesize==0*/) {
                // if this is an exit label branch then merely return from function
                char *ret = "\trts\n";  // TODO JL - CODE THE RTS !! using stack pop I guess
                emit(f, ret);
            } else {
                // emit(f,"\tb\t%s%d\n",labprefix,t);
                emit(f, "\tPCHI = <:%s%d\n", labprefix, t);
                emit(f, "\tPCLO = >:%s%d\n", labprefix, t);
            }
            continue;
        }
        if (c >= BEQ && c < BRA) {
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
                emit(f, "\tPCHI = <:%s%d\n", labprefix, t);
                emit(f, "\tPCLO = >:%s%d %s\n", labprefix, t, flag);
            } else if (c == BGE) {
                emit(f, "\tPCHI = <:%s%d\n", labprefix, t);
                emit(f, "\tPCLO = >:%s%d _GT\n", labprefix, t);
                emit(f, "\tPCLO = >:%s%d _EQ\n", labprefix, t);
            } else if (c == BLE) {
                emit(f, "\tPCHI = <:%s%d\n", labprefix, t);
                emit(f, "\tPCLO = >:%s%d _LT\n", labprefix, t);
                emit(f, "\tPCLO = >:%s%d _EQ\n", labprefix, t);
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
            continue;
        }
        if (c == MOVETOREG) {
            emit(f, "MOVETOREG  REG=%d   Q1=%d  FLAGS=%d\n", p->z.reg, &p->q1, regtype[p->z.reg]->flags);
            load_reg(f, p->z.reg, &p->q1, regtype[p->z.reg]->flags);
            continue;
        }
        if (c == MOVEFROMREG) {
            emit(f, "MOVEFROMREG  REG=%d   Q1=%d  FLAGS=%d\n", p->z.reg, &p->q1, regtype[p->z.reg]->flags);

            store_reg(f, p->z.reg, &p->q1, regtype[p->z.reg]->flags);
            continue;
        }
        if ((c == ASSIGN || c == PUSH) && ((t & NQ) > POINTER || ((t & NQ) == CHAR && zm2l(p->q2.val.vmax) != 1))) {
            // what is this
            emit(f, "ERROR IF (ASSIGN|PUSH) & (TYPE>POINTER|(TYPE=CHAR & VAL %d!=1))\n", zm2l(p->q2.val.vmax));
            ierror(0);
        }
        /* switch commutative operands if suitable */
        if (c == ADD || c == MULT || c == AND || c == XOR || c == OR) {
            /*
            // IF TARG and 2ND ARG SAME
            if(compare_objects(&p->q2,&p->z)){
                emit(f,"ADD/MULT/AND/XOR/OR commute !\n");
                struct obj tmp;
                tmp=p->q1;
                p->q1=p->q2;
                p->q2=tmp;
            }
            */

            // quote : Constants will usually be in q2 if possible. One of the sources always is not constant and the target is always an lvalue.

            // SPAM 1 has const on arg 2 - so try to move it there
            if (isconst(q1)) {
                emit(f, "ADD/MULT/AND/XOR/OR commute !\n");
                struct obj tmp;
                tmp = p->q1;
                p->q1 = p->q2;
                p->q2 = tmp;
            }
        }

        p = preload(f, p);

        c = p->code;

        // Subtract a pointer from a pointer. q1,q2->z.
        if (c == SUBPFP) c = SUB;
        // Add an integer to a pointer. q1,q2->z.
        if (c == ADDI2P) c = ADD;
        // Subtract an Integer from a pointer. q1,q2->z.
        if (c == SUBIFP) c = SUB;

        if (c == CONVERT) {
            emit(f, "CONVERT\n");

            if (ISFLOAT(q1typ(p)) || ISFLOAT(ztyp(p))) ierror(0);
            if (sizetab[q1typ(p) & NQ] < sizetab[ztyp(p) & NQ]) {
                if (q1typ(p) & UNSIGNED)
                    emit(f, "\tzext.%s\t%s\n", dt(q1typ(p)), regnames[zreg]);
                else
                    emit(f, "\tsext.%s\t%s\n", dt(q1typ(p)), regnames[zreg]);
            }
            save_result(f, p);
            continue;
        }
        if (c == KOMPLEMENT) {
            emit(f, "KOMPLEMENT\n");

            load_reg(f, zreg, &p->q1, t);
            emit(f, "\tcpl.%s\t%s\n", dt(t), regnames[zreg]);
            save_result(f, p);
            continue;
        }
        if (c == SETRETURN) {
            emit(f, "; SETRETURN - zreg = %s\n", regnames[p->z.reg]);

            //load_reg(f, zreg, &p->q1, t); // don't know how zreg gets set - so directly using zreg
            load_reg(f, p->z.reg, &p->q1, t);

            // I assume set because the value carried here persists until the fn returns to caller
            BSET(regs_modified, p->z.reg); // sets the given bit in an array of bytes
            continue;
        }
        if (c == GETRETURN) {
            gc_getreturn(f, p);

            continue;
        }
        if (c == CALL) {
            gc_call(f, p);
            continue;
        }
        if (c == ASSIGN || c == PUSH) {
            emit(f, "; ASSIGN/PUSH\n");

            if (t == 0) ierror(0);
            if (c == PUSH) {
                emit(f, "; ASSIGN/PUSH = P\n");
#if FIXED_SP
                emit(f, "\t; ORIGINAL mov.%s\t%ld(%s),", dt(t), pushed, regnames[sp]);
                emit_obj(f, &p->q1, t);
                emit(f, "\n");
                pushed += zm2l(p->q2.val.vmax);
#else
                emit(f, "\tpush.%s\t", dt(t));
                emit_obj(f, &p->q1, t);
                emit(f, "\n");
                push(zm2l(p->q2.val.vmax));
#endif
                continue;
            }
            if (c == ASSIGN) {
                emit(f, "; ASSIGN %s %s\n", dt(t), regnames[p->z.reg]);
                load_reg(f, zreg, &p->q1, t);
                save_result(f, p);
            }
            continue;
        }
        if (c == ADDRESS) {
            emit(f, "ADDRESS\n");
            load_address(f, zreg, &p->q1, POINTER);
            save_result(f, p);
            continue;
        }
        if (c == MINUS) {
            emit(f, "MINUS\n");
            load_reg(f, zreg, &p->q1, t);
            emit(f, "\tneg.%s\t%s\n", dt(t), regnames[zreg]);
            save_result(f, p);
            continue;
        }
        if (c == TEST) {
            emit(f, "TEST\n");
            emit(f, "\ttst.%s\t", dt(t));
            if (multiple_ccs)
                emit(f, "%s,", regnames[zreg]);
            emit_obj(f, &p->q1, t);
            emit(f, "\n");
            if (multiple_ccs)
                save_result(f, p);
            continue;
        }
        if (c == COMPARE) {
            emit(f, "; COMPARE START ======================================================\n");

            emit(f, "\t; ORIGINAL ASM: \t");
            emit(f, "\tcmp.%s\t", dt(t));
            emit_obj(f, &p->q1, t);
            emit(f, ",");
            emit_obj(f, &p->q2, t);
            emit(f, "\n");

            struct obj *qq1 = &p->q1;

            int q1Reg = getReg(&(p->q1));
            int q2Reg = getReg(&(p->q2));

            // const usually in q2, one source is always non-const
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

            // current inst is a COMPARE but since SPAM1 is 8 bit I can't do a single compare of values.
            // for multibyte values I need to know what kind of BR* instruction that will occur off the back of this COMPARE
            // so that I can arrange a multibyte algorithm that sets the needed status reg flag.
            int branchType = findBranch(f, p);
            emit(f, "\t; BRANCH-TYPE-WILL-BE %s\n", ename[branchType]);

            char *q1Regname = regnames[p->q1.reg];
            char *q2Regname = regnames[p->q2.reg];

            if (branchType == BEQ || branchType == BNE || branchType == BLT || branchType == BGT) {
                emit(f, "\tREGA=[:%s+3]\n", q1Regname);
                emit(f, "\tNOOP = REGA A_MINUS_B_SIGNEDMAG [:%s+3] _S\n", q2Regname);
                emit(f, "\tREGA=[:%s+2]\n", q1Regname);
                emit(f, "\tNOOP = REGA A_MINUS_B           [:%s+2] _EQ_S\n", q2Regname);
                emit(f, "\tREGA=[:%s+1]\n", q1Regname);
                emit(f, "\tNOOP = REGA A_MINUS_B           [:%s+1] _EQ_S\n", q2Regname);
                emit(f, "\tREGA=[:%s+0]\n", q1Regname);
                emit(f, "\tNOOP = REGA A_MINUS_B           [:%s+0] _EQ_S\n", q2Regname);
                emit(f, "\t; aggregate flags into register\n");
                emit(f, "\tREGA=0\n");
                emit(f, "\tREGA = REGA A_OR_B 1 _LT\n");
                emit(f, "\tREGA = REGA A_OR_B 2 _GT\n");
                emit(f, "\tREGA = REGA A_OR_B 4 _NE\n");
                emit(f, "\tREGA = REGA A_OR_B 8 _EQ\n");
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
            if (ISINT(t)) {  // should this be ISSCALAR??
                int offset = 0;
                /*
                emit(f,"\t<");
                emitvalJL(f, &p->q1.val, t, 0);
                if(p->q1.flags&REG)
                    emit(f,":%s",regnames[p->q1.reg]);
                emit(f,">\n");
                emit(f,"\t<");
                emitvalJL(f, &p->q2.val, t, 0);
                if(p->q1.flags&REG)
                    emit(f,":%s",regnames[p->q1.reg]);
                emit(f,">\n");

                int datatypeSize = msizetab[BASETYPE(t)];
                while (offset < datatypeSize) {
                    emit(f, "\tREGA = ");
                    emit_objJL(f, &p->q1, t, offset);
                    emit(f, "\n");

                    emit(f, "\tNOOP = REGA - ");
                    emit_objJL(f, &p->q2, t, offset);
                    emit(f, "\t\n");
                    offset++;
                }
                */
                continue;
            } else {
                emit(f, "\t;COMPARE NOT INT NOT IMPL\n");
                emit_obj(f, &p->q1, t);
                emit(f, ",");
                emit_obj(f, &p->q2, t);
                emit(f, "\n");
                if (multiple_ccs)
                    save_result(f, p);
                continue;
            }
            continue;

        }

        if ((c >= OR && c <= AND) || (c >= LSHIFT && c <= MOD)) {
            emit(f, "; OR AND SHIFT MOD \n");
            if (!THREE_ADDR)
                load_reg(f, zreg, &p->q1, t);
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
            save_result(f, p);
            continue;
        }

        // ELSE
        fprintf(stderr, "ERROR\n");
        fprintf(stderr, "CODE: %s\n", ename[p->code]);
        pric2(stdout, p);
        ierror(0);
    }

    function_bottom(f, v, localsize);
    if (stack_valid) {
        if (!v->fi) v->fi = new_fi();
        v->fi->flags |= ALL_STACK;
        v->fi->stack1 = stack;
    }
    emit(f, "# stacksize=%lu%s\n", zum2ul(stack), stack_valid ? "" : "+??");
}

void gc_call(FILE *f, struct IC *p) {
    int t = p->typf;

    emit(f, "; CALL\n");

    int reg;
    if ((p->q1.flags & (VAR | DREFOBJ)) == VAR && p->q1.v->fi && p->q1.v->fi->inline_asm) {
        emit_inline_asm(f, p->q1.v->fi->inline_asm);
    } else {
        emit(f, "\tcall\t");
        emit_obj(f, &p->q1, t);
        emit(f, "\n");
    }
    pushed -= zm2l(p->q2.val.vmax);

    if ((p->q1.flags & (VAR | DREFOBJ)) == VAR && p->q1.v->fi && (p->q1.v->fi->flags & ALL_REGS)) {
        bvunite(regs_modified, p->q1.v->fi->regs_modified, RSIZE);
    } else {
        int i;
        for (i = 1; i <= MAXR; i++) {
            if (regscratch[i]) BSET(regs_modified, i);
        }
    }
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
        if (isRegP(&p->z)) {
            int targ = p->z.reg;
            int src = p->q1.reg;
            //save_result(f, p);

            emit(f, "\tREGA = [:%s]\n", regnames[src]);
            emit(f, "\t[:%s] = REGA\n", regnames[targ]);
        } else {
            fprintf(stderr, "z is not a register\n");
            dumpObj(f, &p->z, ztyp(p));
            ierror(0);
        }
    } else {
      //  p->z.flags = 0;
        fprintf(stderr, "not q1.reg is 0\n");
        dumpObj(f, &p->q1, q1typ(p));
        ierror(0);
    }
}

int shortcut(int code, int typ) {
    return 0;
}

int reg_parm(struct reg_handle *m, struct Typ *t, int vararg, struct Typ *d) {
    int f;
    f = t->flags & NQ;
    if (f <= LONG || f == POINTER) {
        if (m->gregs >= GPR_ARGS)
            return 0;
        else
            return R_G0 + 3 + m->gregs++;
    }
    if (ISFLOAT(f)) {
        if (m->fregs >= FPR_ARGS)
            return 0;
        else
            return R_F0 + 2 + m->fregs++;
    }
    return 0;
}

int handle_pragma(const char *s) {
}

void cleanup_cg(FILE *f) {
    // write all the registers at the bottom of the asm
    for (int i = 0; i <= MAXR; i++)
        emit(f, "\t%-6s:\tBYTES [0,0,0,0]\n", regnames[i]);
}

void cleanup_db(FILE *f) {
    if (f) section = -1;
}

void dumpObj(FILE *f, struct obj *o, int otyp) {
    emit(f, " <");

    if (o->flags == (KONST | DREFOBJ)) {  // const pointer
        emit(f, " K|D:");
        emitval(f, &o->val, otyp);
    }
    if (o->flags == KONST) {  // const val
        emit(f, " K:");
        emitval(f, &o->val, otyp);
    }
    if (o->flags & REG) {
        emit(f, " ISREG:%s", regnames[o->reg]);
    }
    if (o->flags & DREFOBJ) {
        emit(f, " DREFOBJ");
    }
    if (o->flags & VARADR) {
        emit(f, " VARADR");
    }
    if (o->flags & VAR) {
        emit(f, " VAR");
        if (o->v->storage_class == AUTO) {
            emit(f, " AUTO");
            emit(f, ":%ld(%s)", real_offset(o), regnames[sp]);
        } else if (o->v->storage_class == REGISTER) {
            emit(f, " REGISTER");
            emit(f, ":%ld(%s)", real_offset(o), regnames[sp]);
        } else {
            // add sign
            if (!zmeqto(l2zm(0L), o->val.vmax)) {
                emitval(f, &o->val, LONG);
                emit(f, "+");
            }
            if (o->v->storage_class == STATIC) {
                emit(f, " STATIC:%s%ld", labprefix, zm2l(o->v->offset));
            } else if (o->v->storage_class == EXTERN) {
                emit(f, " EXTERN:%s%ld", labprefix, zm2l(o->v->offset));
            } else {
                emit(f, " NONSTATC:%s%s", idprefix, o->v->identifier);
            }
        }
    }
    emit(f, ">");
}

void dumpIC(FILE *f, struct IC *p) {
    emit(f, "\tDUMP code = %s(%d)", ename[p->code], p->code);
    emit(f, "\n");

    emit(f, "\tDUMP z=");
    dumpObj(f, &p->z, ztyp(p));
    emit(f, "\n");

    emit(f, "\tDUMP q1=");
    dumpObj(f, &p->q1, q1typ(p));
    emit(f, "\n");

    emit(f, "\tDUMP q2=");
    dumpObj(f, &p->q2, q2typ(p));
    emit(f, "\n");
}
