// WORKS IN SIMULATOR

void halt(__reg("gpr0") char) = "\tHALT = [:gpr0]\n";

int main() {

    int status = 666;

    if (status!=666) {
      halt(1);
    }

    if (status==666) {
      status=999;
    } else {
      halt(2);
    }

    if (status==666) {
      halt(3);
    }

    if (status!=999) {
      halt(4);
    }


    halt(0);
}
