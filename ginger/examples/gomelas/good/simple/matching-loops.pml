
#define y ??
#define x ??
#define k ??

init {
  chan c = [k] of {int};
  run p1(c);
  run p2(c)
}

proctype p1(chan c) {
  int i;
  for (i : 0 .. x) {
    c!0
  }
}

proctype p2(chan c) {
  int j;
  for (j : 0 .. y) {
    c?y
  }
}
