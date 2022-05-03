


#include "dt.h"

// http://sun.hasenbraten.de/vbcc/docs/vbcc_13.html#SEC192

// 13.8.4 Register Parameters
struct reg_handle empty_reg_handle={0,0};


// todo
int reg_parm(struct reg_handle *, struct Typ *, int vararg) {

    if(vararg)
        return 0;

    // get unqualified type
    int  f = t->flags & NQ;

    if(ISCHAR(f)){
        // nothing yet
    }
    
    return 0;
}

