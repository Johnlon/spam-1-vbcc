// WORKS IN SIMULATOR

void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int value = 666;

    if (value!=666) {
      halt(1);
    }

    halt(0);
}
