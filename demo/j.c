/*int glob1;
void pop() {
    int pop1 = 1;
    glob1 = pop1;

}
*/
extern int externalFn(); 
extern int externalFn2(); 

int static1=1;
int static2=2;

int sub(int subParamA, int subParamB, int subParamC, int subParamD, int subParamE, int subParamF   );

int main() {
    short  myShort=0xbeaf;
    long myLong=0xfefe;

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
  sub(subVar1, subVar2, subParamC, 3,3,3);
  sub(subVar1, subVar2, subParamC, 3,3,3);
  return subVar1;
}
