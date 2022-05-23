extern int* extP;
//
//int pop() {
//    char cc=11;
//    unsigned char uCC=11;
//    int myInt=99;
//    long myLong=333;
//    unsigned long myULong=333;
//
//    char cc1=11;
//    char cc2=cc1+11;
//    char cc3=cc2+11;
//    char cc4=cc3+11;
//    char cc5=cc4+11;
//    char cc6=cc5+11;
//
//    int *pInt0 = (int*)22;
//    static int *pInt;
//    register int *pRegInt;
//
//    pInt = (int*)10;
//    pInt = extP;
//
//    if (cc == myLong || myLong == 22) {
//        myLong=0xaa;
//        cc1=21;
//        cc2=21;
//        cc3=21;
//        cc4=21;
//    }
//
//pop();
//    if (uCC == myULong) {
//        myULong=0xaa;
//    }
//    return cc1+cc2+cc3+cc4+cc5+cc6;
//
//  //  return myInt;
//}
//

int main() {
    char cc=11;
    unsigned char uCC=11;
    int myInt=99;
    static long myLong=333;
    unsigned long myULong=333;

    char cc1=11;
    char cc2=cc1+11;
    char cc3=cc2+11;
    char cc4=cc3+11;
    char cc5=cc4+11;
    char cc6=cc5+11;

    int *pInt0 = (int*)22;
    static int *pInt;
    register int *pRegInt;

    pInt = (int*)10;
    pInt = extP;

    if (cc == myLong || myLong == 22) {
//pop();
        myLong=0xaa;
        cc1=21;
        cc2=21;
        cc3=21;
        cc4=21;
    }
    cc4=cc3+11;
    cc5=cc4+11;
    cc6=cc5+11;

    if (uCC == myULong) {
        myULong=0xaa;
    }
    return cc1+cc2+cc3+cc4+cc5+cc6;

  //  return myInt;
}

