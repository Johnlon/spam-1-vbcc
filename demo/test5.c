
// WORKS IN SIMULATOR

void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int value = 666;

    int* pValue = &value;

    int other = *pValue;

    if (other != 666) {
      halt(1);
    }
    if (other == 666) {
      halt(0);
    }

    halt(2);
}
