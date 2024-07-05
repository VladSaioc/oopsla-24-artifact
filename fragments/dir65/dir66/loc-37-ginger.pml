// git_link=loc-37-ginger.pml
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
  chan child_3 = [1] of {int};
  run fun2(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun2(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  int y10 = x1;
  chan c11 = [y10] of {int};
  goto stop_process;
  stop_process: skip;
  child_4!0;
}