// cannot use \n in the asm as this terminates it !!
void halt2(__reg("ghalt1") char marlo, __reg("ghalt2") char alu) = "\tMARHI=0\tMARLO=[:ghalt1]\tHALT=[:ghalt2]\n";

int more(int unused) {
  return unused;
}

int sub(int a, int b) {
  int subA = a;
  int subB = b;

  if (a != 3)  {
    more(a);
  }

  if (a != 3)  {
    halt2(a, 13);
  }
  if (b != 4)  {
    halt2(17, 14);
  }
  if (subA != 3)  {
    halt2(subA, 23);
  }
  if (subB != 4)  {
    halt2(subB, 24);
  }
  return 99;
}

int main() {
  int localA = 10;

  int check = 88;
  // call a function - does subroutine trample local var r?
  sub(3, 4);

  if (localA != 10)  {
    halt2(localA, 10);
  }

  if (check != 88)  {
    halt2(check, 88);
  }

  return 66;
}
