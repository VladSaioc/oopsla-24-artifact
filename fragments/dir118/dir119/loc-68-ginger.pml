// git_link=loc-68-ginger.pml
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
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c13 = [0] of {int};
  for(y8 : 0 .. (x1) - (1)) {
    for10: skip;
    run fun3(c13, child_11);
    run receiver(child_11);
    c13?0;
    for10_end: skip;
  };
  for10_exit: skip;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c14; chan child_15) {
  bool y16 = false;
  bool y17 = false;
  int y18 = 0;
  bool y19 = true;
  int y20 = 0;
  c14!0;
  stop_process: skip;
  child_15!0;
}
proctype receiver(chan c) {
  c?0;
}