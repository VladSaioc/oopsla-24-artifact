// git_link=loc-64-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
  run fun4(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = x2;
  int y15 = x1;
  chan c16 = [0] of {int};
  for(y10 : 0 .. (y15) - (1)) {
    for10: skip;
    run fun5(c16, x3, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y15) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y15) - (1)) {
      for23: skip;
      c16?0;
      for23_end: skip;
    };
    for23_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c16?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun5(chan c18; int y19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  c18!0;
  stop_process: skip;
  child_20!0;
}
proctype receiver(chan c) {
  c?0;
}