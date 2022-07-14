
int main() {

    // not using inline initialiser works ok
    int value[] = { 1234567890, 987654321 };

// not yet supported
    if (*value != 1234567890) {
      return 1;
     }

    return 0;
}
