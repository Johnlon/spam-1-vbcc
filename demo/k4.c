
int sub(int subParamA, int subParamB, int subParamC, int subParamD)
{
  int t = sub(subParamA, subParamB, subParamC, 444);
  return t;
}

int main() {
    int o = sub(11, 22, 33, 44);
    return o;
}

