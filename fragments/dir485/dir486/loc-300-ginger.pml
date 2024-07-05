// git_link=loc-300-ginger.pml
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
  chan child_14 = [1] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c16 = [0] of {int};
  for(y11 : 0 .. (y6) - (1)) {
    for10: skip;
    run fun4(c16, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y6) - (1)) != (-(3)) ->
    for(y11 : 0 .. (y6) - (1)) {
      for20: skip;
      c16?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c16?0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  c19!0;
  stop_process: skip;
  child_20!0;
}
proctype receiver(chan c) {
  c?0;
}