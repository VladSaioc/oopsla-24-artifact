// git_link=loc-220-ginger.pml
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
  chan c15 = [0] of {int};
  for(y11 : 0 .. (y7) - (1)) {
    for20: skip;
    run fun4(c15, child_14);
    run receiver(child_14);
    for20_end: skip;
  };
  for20_exit: skip;
  if
  :: ((y7) - (1)) != (-(3)) ->
    for(y11 : 0 .. (y7) - (1)) {
      for30: skip;
      c15?0;
      for30_end: skip;
    };
    for30_exit: skip;
  :: else  ->
    do
    :: true ->
      for31: skip;
      c15?0;
      for31_end: skip;
    :: true ->
      break;

    od;
    for31_exit: skip;
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
proctype fun4(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  c18!0;
  stop_process: skip;
  child_19!0;
}
proctype receiver(chan c) {
  c?0;
}