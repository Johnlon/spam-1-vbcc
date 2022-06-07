// WORKS IN SIMULATOR

void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int countUp = 0;

    while (countUp < 3) {
      countUp += 2;
    }


    if (countUp != 4) {
      halt(1);
    }

    halt(0);
}
