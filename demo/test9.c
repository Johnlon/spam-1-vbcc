// cannot use \n in the asm as this terminates it !!
//void halt2(__reg("ghalt1") char marlo, __reg("ghalt2") char alu) = "\tMARHI=0\tMARLO=[:ghalt1]\tHALT=[:ghalt2]\n";
#include "stdio.h"
void halt2(char marlo, char alu) {
  printf("%d", alu);
}

union {
  unsigned int integer;
  char byte[4];
} u;

int main() {
  u.integer = 0x01020304;

  printf("%d\n", u.byte[0]);
  printf("%d\n", u.byte[3]);

  if (u.byte[0] != 1) {
    halt2(u.byte[0], 1);
  }

  return 66;
}
