// git_link=loc-175-ginger.pml
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
  chan c14 = [20] of {int};
  for(y9 : 0 .. (y5) - (1)) {
    for10: skip;
    c14!0;
    run fun3(c14, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((20) - (1)) != (-(3))) ->
    for(y9 : 0 .. (20) - (1)) {
      for21: skip;
      c14!0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c14!0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run close(c14);
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
  defer1: skip;
  skip;
  c16?0;
  stop_process: skip;
  child_17!0;
}
proctype receiver(chan c) {
  c?0;
}