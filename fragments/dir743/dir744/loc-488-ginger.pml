// git_link=loc-488-ginger.pml
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
  run fun3(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  int y13 = x2;
  int y14 = x1;
  chan c15 = [y14] of {int};
  for(y9 : 0 .. (y14) - (1)) {
    for10: skip;
    run fun4(c15, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y14) - (1)) != (-(3))) ->
    for(y9 : 0 .. (y14) - (1)) {
      for23: skip;
      c15?0;
      for23_end: skip;
    };
    for23_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c15?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run close(c15);
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(chan c17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  if
  :: true ->
    c17!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c17!0;
    goto stop_process;
  :: true ->


  fi;
  c17!0;
  stop_process: skip;
  child_18!0;
}
proctype receiver(chan c) {
  c?0;
}