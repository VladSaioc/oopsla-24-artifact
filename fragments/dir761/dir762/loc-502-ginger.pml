// git_link=loc-502-ginger.pml
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
  int y13 = -(2);
  int y14 = -(2);
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
  chan c18 = [2] of {int};
  if
  :: ((0) != (-(2))) && (((2) - (1)) != (-(3))) ->
    for(y8 : 0 .. (2) - (1)) {
      for21: skip;
      c18!0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c18!0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  chan c20 = [y12] of {int};
  for(y8 : 0 .. (y12) - (1)) {
    for30: skip;
    run fun3(c18, c20, child_11);
    run receiver(child_11);
    for30_end: skip;
  };
  for30_exit: skip;
  if
  :: ((y12) - (1)) != (-(3)) ->
    for(y8 : 0 .. (y12) - (1)) {
      for40: skip;
      c20?0;
      for40_end: skip;
    };
    for40_exit: skip;
  :: else  ->
    do
    :: true ->
      for41: skip;
      c20?0;
      for41_end: skip;
    :: true ->
      break;

    od;
    for41_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c22; chan c23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  c22?0;
  c23!0;
  c22!0;
  stop_process: skip;
  child_24!0;
}
proctype receiver(chan c) {
  c?0;
}