// git_link=loc-504-ginger.pml
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
  run fun3(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = 0;
  int y15 = x2;
  chan c16 = [y14] of {int};
  for(y10 : 0 .. (y6) - (1)) {
    for20: skip;
    for(y10 : 0 .. (y15) - (1)) {
      for21: skip;
      run fun4(c16, child_13);
      run receiver(child_13);
      for21_end: skip;
    };
    for21_exit: skip;
    for20_end: skip;
  };
  for20_exit: skip;
  if
  :: ((0) != (-(2))) && (((y14) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y14) - (1)) {
      for31: skip;
      c16?0;
      for31_end: skip;
    };
    for31_exit: skip;
  :: else  ->
    do
    :: true ->
      for30: skip;
      c16?0;
      for30_end: skip;
    :: true ->
      break;

    od;
    for30_exit: skip;
  fi;
  run close(c16);
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  if
  :: true ->
    c18!0;
    goto stop_process;
  :: true ->


  fi;
  c18!0;
  stop_process: skip;
  child_19!0;
}
proctype receiver(chan c) {
  c?0;
}