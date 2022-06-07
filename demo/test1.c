// WORKS IN SIMULATOR

void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int status = 666;

    if (status!=666) {
      halt(1);
    }

    halt(0);
}
