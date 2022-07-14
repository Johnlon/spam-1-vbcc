
void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int value[2];
    value[0] = 1234567890;
    value[1] = 987654321;

    int value1 = value[0];
    int value2 = value[1];

    if (value1 != 1234567890) {
      halt(1);
    }
    if (value2 != 987654321) {
      halt(2);
    }

    halt(0);
}
