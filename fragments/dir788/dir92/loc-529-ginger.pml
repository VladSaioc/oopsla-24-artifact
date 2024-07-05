// git_link=loc-529-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun3(x1, x2, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(int y7; int y8; chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan c16 = [y7] of {int};
  for(y12 : 0 .. (y7) - (1)) {
    for10: skip;
    run fun4(c16, y8, child_15);
    run receiver(child_15);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && ((((y7) * (y8)) - (1)) != (-(3))) ->
    for(y12 : 0 .. ((y7) * (y8)) - (1)) {
      for21: skip;
      c16?0;
      for21_end: skip;
    };
    for21_exit: skip;
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
  child_9!0;
}
proctype fun4(chan c18; int y19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  chan child_26 = [1] of {int};
  chan child_27 = [1] of {int};
  if
  :: ((0) != (-(2))) && (((y19) - (1)) != (-(3))) ->
    for(y23 : 0 .. (y19) - (1)) {
      for14: skip;
      run fun5(c18, child_26);
      child_26?0;
      for14_end: skip;
    };
    for14_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      run fun5(c18, child_27);
      child_27?0;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  stop_process: skip;
  child_20!0;
}
proctype fun5(chan c29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  if
  :: true ->

  :: true ->

  :: true ->

  :: true ->

  :: true ->

  :: true ->


  fi;
  if
  :: true ->
    c29!0;

  fi;
  stop_process: skip;
  child_30!0;
}
proctype receiver(chan c) {
  c?0;
}