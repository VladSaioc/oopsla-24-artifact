// git_link=loc-102-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun1(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun1(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  int y15 = x1;
  chan c16 = [y15] of {int};
  run fun2(c16, child_14);
  run receiver(child_14);
  run fun3(c16, child_13);
  run receiver(child_13);
  run fun4(c16, child_12);
  run receiver(child_12);
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun2(chan c17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  c17!0;
  stop_process: skip;
  child_18!0;
}
proctype fun3(chan c24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  c24!0;
  stop_process: skip;
  child_25!0;
}
proctype fun4(chan c31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  c31!0;
  stop_process: skip;
  child_32!0;
}
proctype receiver(chan c) {
  c?0;
}
