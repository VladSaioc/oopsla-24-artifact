// git_link=loc-137-ginger.pml
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
  int y14 = x2;
  int y15 = 0;
  if
  :: true ->
    y15 = y6;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [0] of {int};
  for(y10 : 0 .. (y14) - (1)) {
    for30: skip;
    run fun4(c18, child_13);
    run receiver(child_13);
    for30_end: skip;
  };
  for30_exit: skip;
  if
  :: ((y14) - (1)) != (-(3)) ->
    for(y10 : 0 .. (y14) - (1)) {
      for40: skip;
      c18?0;
      for40_end: skip;
    };
    for40_exit: skip;
  :: else  ->
    do
    :: true ->
      for41: skip;
      c18?0;
      for41_end: skip;
    :: true ->
      break;

    od;
    for41_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(chan c20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  c20!0;
  stop_process: skip;
  child_21!0;
}
proctype receiver(chan c) {
  c?0;
}