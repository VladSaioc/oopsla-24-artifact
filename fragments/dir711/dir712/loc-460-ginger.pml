// git_link=loc-460-ginger.pml
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
  run fun2(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  chan child_13 = [1] of {int};
  int y14 = x1;
  int y15 = y14;
  chan c16 = [y15] of {int};
  for(y9 : 0 .. (y14) - (1)) {
    for10: skip;
    run fun3(c16, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun4(c16, y15, child_12);
  child_12?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  c17!0;
  stop_process: skip;
  child_18!0;
}
proctype fun4(chan c24; int y25; chan child_26) {
  bool y27 = false;
  bool y28 = false;
  int y29 = 0;
  bool y30 = true;
  int y31 = 0;
  int y32 = -(2);
  int y33 = -(2);
  if
  :: ((0) != (-(2))) && (((y25) - (1)) != (-(3))) ->
    for(y29 : 0 .. (y25) - (1)) {
      for23: skip;
      c24?0;
      for23_end: skip;
    };
    for23_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c24?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run close(c24);
  goto stop_process;
  stop_process: skip;
  child_26!0;
}
proctype receiver(chan c) {
  c?0;
}