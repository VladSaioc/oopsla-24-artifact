// git_link=loc-94-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun2(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  int y12 = x1;
  int y13 = y12;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c15 = [0] of {int};
  for(y8 : 0 .. (y12) - (1)) {
    for10: skip;
    run fun3(c15, child_11);
    run receiver(child_11);
    for10_end: skip;
  };
  for10_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c16; chan child_17) {
  bool y18 = false;
  bool y19 = false;
  int y20 = 0;
  bool y21 = true;
  int y22 = 0;
  c16!0;
  stop_process: skip;
  child_17!0;
}
proctype receiver(chan c) {
  c?0;
}