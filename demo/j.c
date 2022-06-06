/*int glob1;
void pop() {
    int pop1 = 1;
    glob1 = pop1;

}
*/
extern int externalFn(); 
extern int externalFn2(); 

int static1=111;
int static2=222;

int sub(int subParamA, int subParamB, int subParamC, int subParamD, int subParamE, int subParamF   );

void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";


int main() {

    // check basic int assign and verify
    int status = 666;

    if (status!=666) {
      halt(1);
    }
    if (status==666) {
      status=999;
    } else {
      halt(2);
    }
    if (status!=999) {
      halt(3);
    }

    short  myShort=0xbeaf;
    long   myLong=0xfefe;


    struct MyStruct {
      char  s1;
      long  s2;
    } *myStruct;

    myStruct->s1 = 0xa1;
    myStruct->s2 = 0xb2;

    if (myLong == myLong) {
        myLong=0xaa;
    }
    else 
    {
        myLong=0xbb + myLong;
    }

    int o = sub(11, 22, 33, 44, 55, 66);
    return (int)myLong;

  //  return myInt;
}

int sub(int subParamA, int subParamB, int subParamC, int subParamD, int subParamE, int subParamF   )
{
  int subVar1=subParamA;
  int subVar2=subParamB + subParamC + subParamD + subParamE + subParamF;
  int subVarC=subVar1;
  sub(subVar1, subVar2, subParamC, 5,6,7);
  return subVar1;
}
