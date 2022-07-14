
void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int value[] ={ 0x04030201, 0x09080706 };

    int value1 = value[0];
    int value2 = value[1];

    if (value[0] != 1234567890) {
      halt(1);
    }
    if (value[1] != 987654321) {
      halt(2);
    }
}
