// git_link=loc-459-ginger.pml
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
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [y15] of {int};
  for(y9 : 0 .. (y14) - (1)) {
    for10: skip;
    run fun3(c18, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun4(c18, y15, child_12);
  child_12?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  if
  :: true ->

  :: true ->


  fi;
  c19!0;
  stop_process: skip;
  child_20!0;
}
proctype fun4(chan c27; int y28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
  if
  :: ((0) != (-(2))) && (((y28) - (1)) != (-(3))) ->
    for(y32 : 0 .. (y28) - (1)) {
      for21: skip;
      c27?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c27?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run close(c27);
  goto stop_process;
  stop_process: skip;
  child_29!0;
}
proctype receiver(chan c) {
  c?0;
}