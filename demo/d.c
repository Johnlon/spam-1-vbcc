#include "stdio.h"

struct sss {int i;};

void demo() {
//move int #I1->gpr0(i) size=4
//DUMP z <'i'  REG:gpr0 VAR STORAGE:AUTO:0(sp) >
//DUMP q1 <''  KONST:1 >
//int i=1;      

//move pointer #P1->gpr0(iP) size=4
//DUMP z <'iP'  REG:gpr0 VAR STORAGE:AUTO:0(sp) >
//DUMP q1 <''  KONST:1 >
//int *iP=1;  

//move int M0+L3(staticInt)->gpr0(ii) size=4
//DUMP z <'ii'  REG:gpr0 VAR STORAGE:AUTO:0(sp) >
//DUMP q1 <'staticInt'  VAR STORAGE:STATIC:l3 >
//static int staticInt = 1;
//int ii = staticInt;

struct sss s1=10;
struct sss *sp = &s1;
}
