// git_link=loc-104-ginger.pml
#define  default true
#define  x1 ??
typedef Chandef {
  chan sync = [0] of {bool};
  chan enq = [0] of {bool};
  chan deq = [0] of {bool,bool};
  chan sending = [0] of {bool};
  chan rcving = [0] of {bool};
  chan closing = [0] of {bool};
  int size = 0;
  int num_msgs = 0;
  bool closed = false;
}
init {
  chan child_6 = [1] of {int};
  run fun2(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun2(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  int y16 = x1;
  chan c17 = [y16] of {int};
  run fun3(c17, child_15);
  run receiver(child_15);
  run fun4(c17, child_14);
  run receiver(child_14);
  run fun5(c17, child_13);
  run receiver(child_13);
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  c18!0;
  stop_process: skip;
  child_19!0;
}
proctype fun4(chan c25; chan child_26) {
  bool y27 = false;
  bool y28 = false;
  int y29 = 0;
  bool y30 = true;
  int y31 = 0;
  c25!0;
  stop_process: skip;
  child_26!0;
}
proctype fun5(chan c32; chan child_33) {
  bool y34 = false;
  bool y35 = false;
  int y36 = 0;
  bool y37 = true;
  int y38 = 0;
  c32!0;
  stop_process: skip;
  child_33!0;
}
proctype receiver(chan c) {
  c?0;
}
