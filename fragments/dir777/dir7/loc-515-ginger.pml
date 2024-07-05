// git_link=loc-515-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
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
  run fun3(x1, x2, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  int y14 = y7;
  chan child_15 = [1] of {int};
  chan c16 = [y6] of {int};
  chan c17 = [y6] of {int};
  for(y11 : 0 .. (y7) - (1)) {
    for10: skip;
    c16!0;
    run fun4(c16, c17, child_15);
    run receiver(child_15);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y14) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y14) - (1)) {
      for21: skip;
      c17?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c17?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c19; chan c20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  c19?0;
  c20!0;
  stop_process: skip;
  child_21!0;
}
proctype receiver(chan c) {
  c?0;
}