
int bar(long a, long b)
{
    long x, y;

    x = 555;
    y = a+b+x;
    return y;
}

int foo(void) {
    return bar(111,222);
}
