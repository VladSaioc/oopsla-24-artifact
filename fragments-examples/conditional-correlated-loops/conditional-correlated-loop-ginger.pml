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
  chan child_4 = [1] of {int};
  run fun2(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  int y12 = x1;
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
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c16 = [y12] of {int};
  for(y8 : 0 .. (y12) - (1)) {
    for10: skip;
    run fun3(c16, child_11);
    run receiver(child_11);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y12) - (1)) != (-(3))) ->
    for(y8 : 0 .. (y12) - (1)) {
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
  child_5!0;
}
proctype fun3(chan c18; chan child_19) {
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
