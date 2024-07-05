// git_link=loc-228-ginger.pml
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
  run fun2(x1, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(int y5; chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c14 = [0] of {int};
  chan c15 = [0] of {int};
  for(y9 : 0 .. (y5) - (1)) {
    for10: skip;
    run fun3(c14, c15, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c17; chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  if
  :: true ->
    c18!0;
    goto stop_process;
  :: true ->


  fi;
  c17!0;
  stop_process: skip;
  child_19!0;
}
proctype receiver(chan c) {
  c?0;
}