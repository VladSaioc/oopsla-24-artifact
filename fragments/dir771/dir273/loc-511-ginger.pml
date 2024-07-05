// git_link=loc-511-ginger.pml
#define  default true
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
  run fun1(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun1(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  chan child_10 = [1] of {int};
  int y11 = 1;
  chan c12 = [y11] of {int};
  run fun2(c12, child_10);
  run receiver(child_10);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  c12?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_4!0;
}
proctype fun2(chan c15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  c15!0;
  stop_process: skip;
  child_16!0;
}
proctype receiver(chan c) {
  c?0;
}