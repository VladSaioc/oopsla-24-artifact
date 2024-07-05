// git_link=loc-528-ginger.pml
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
  chan child_7 = [1] of {int};
  run fun3(x1, child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun3(int y8; chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan c17 = [y8] of {int};
  for(y12 : 0 .. (y8) - (1)) {
    for10: skip;
    run fun4(c17, child_16);
    run receiver(child_16);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun6(c17, child_15);
  run receiver(child_15);
  stop_process: skip;
  child_9!0;
}
proctype fun4(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  chan child_25 = [1] of {int};
  chan child_26 = [1] of {int};
  if
  :: ((0) != (-(2))) && (((x2) - (1)) != (-(3))) ->
    for(y22 : 0 .. (x2) - (1)) {
      for14: skip;
      run fun5(c18, child_25);
      child_25?0;
      for14_end: skip;
    };
    for14_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      run fun5(c18, child_26);
      child_26?0;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  stop_process: skip;
  child_19!0;
}
proctype fun5(chan c28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
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
    c28!0;

  fi;
  stop_process: skip;
  child_29!0;
}
proctype fun6(chan c37; chan child_38) {
  bool y39 = false;
  bool y40 = false;
  int y41 = 0;
  bool y42 = true;
  int y43 = 0;
  stop_process: skip;
  child_38!0;
}
proctype receiver(chan c) {
  c?0;
}