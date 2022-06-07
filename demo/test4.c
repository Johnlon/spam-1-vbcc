// ???? WORKS IN SIMULATOR

void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int adder(int a, int b) {
  return a+b;
}

int main() {

    int sum = adder(1,2);

    if (sum == 3) {
      halt(0);
    }

    halt(sum);
}
