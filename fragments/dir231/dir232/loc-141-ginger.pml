// git_link=loc-141-ginger.pml
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
  chan c13 = [y5] of {int};
  for(y9 : 0 .. (y5) - (1)) {
    for10: skip;
    run fun3(c13, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y5) - (1)) != (-(3)) ->
    for(y9 : 0 .. (y5) - (1)) {
      for20: skip;
      c13?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c13?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
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