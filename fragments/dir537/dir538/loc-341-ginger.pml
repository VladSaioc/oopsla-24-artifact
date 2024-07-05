// git_link=loc-341-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
#define  x4 ??
#define  x5 ??
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
  chan child_8 = [1] of {int};
  run fun6(x1, child_8);
  child_8?0;
  stop_process: skip;
}
proctype fun6(int y9; chan child_10) {
  bool y11 = false;
  bool y12 = false;
  int y13 = 0;
  bool y14 = true;
  int y15 = 0;
  chan child_16 = [1] of {int};
  int y17 = x4;
  int y18 = x3;
  int y19 = x2;
  chan c20 = [0] of {int};
  for(y13 : 0 .. (x5) - (1)) {
    for10: skip;
    run fun7(c20, y18, y17, child_16);
    run receiver(child_16);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y9) - (1)) != (-(3)) ->
    for(y13 : 0 .. (y9) - (1)) {
      for20: skip;
      c20?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for23: skip;
      c20?0;
      for23_end: skip;
    :: true ->
      break;

    od;
    for23_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_10!0;
}
proctype fun7(chan c22; int y23; int y24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  int y31 = -(2);
  if
  :: ((y24) != (-(2))) && (((y23) - (1)) != (-(3))) ->
    for(y28 : y24 .. (y23) - (1)) {
      for14: skip;
      if
      :: true ->
        goto for14_end;
      :: true ->


      fi;
      c22!0;
      for14_end: skip;
    };
    for14_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      if
      :: true ->
        goto for11_end;
      :: true ->


      fi;
      c22!0;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  stop_process: skip;
  child_25!0;
}
proctype receiver(chan c) {
  c?0;
}