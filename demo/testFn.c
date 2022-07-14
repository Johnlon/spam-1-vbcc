
void halt(__reg("gpr30") char) = "\tHALT = [:gpr30]\n";

// cannot use \n in the asm as this terminates it !!
void halt2(__reg("ghalt1") char marlo, __reg("ghalt2") char alu) = "\tMARHI=0\tMARLO=[:ghal1]\tHALT = [:ghalt2]\n";
void debug() = "\t; DEBUG\n";

int more(int moreParam) {
  return moreParam;
}

int sub(int subParamA, int subParamB) {
  int subLocalA = subParamA;

  int ret = more(subParamA);

  return ret;
}

int main() {
  // both these get overwrtten
  int localA = 10;

  // call a function - does subroutine trample local var r?
  int subRet = sub(3, 4);

  if (localA != 10)  {
    halt2(localA, 10);

    return 10;
  }
  return 66;
}

