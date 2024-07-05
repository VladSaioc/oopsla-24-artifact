// git_link=loc-66-ginger.pml
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
  int y11 = 0;
  chan child_12 = [1] of {int};
  int y13 = x1;
  chan c14 = [0] of {int};
  for(y8 : 0 .. (y13) - (1)) {
    for10: skip;
    run fun3(c14, y13, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y13) - (1)) != (-(3))) ->
    for(y8 : 0 .. (y13) - (1)) {
      for21: skip;
      c14?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c14?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c15; int y16; chan child_17) {
  bool y18 = false;
  bool y19 = false;
  int y20 = 0;
  bool y21 = true;
  int y22 = 0;
  c15!0;
  stop_process: skip;
  child_17!0;
}
proctype receiver(chan c) {
  c?0;
}
