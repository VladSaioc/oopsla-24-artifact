// git_link=loc-161-ginger.pml
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
  run fun3(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = x2;
  int y15 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c17 = [y15] of {int};
  for(y10 : 0 .. (y14) - (1)) {
    for10: skip;
    run fun4(c17, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y14) - (1)) != (-(3)) ->
    for(y10 : 0 .. (y14) - (1)) {
      for20: skip;
      c17?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c17?0;
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
  child_7!0;
}
proctype fun4(chan c20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  chan child_27 = [1] of {int};
  run fun5(c20, child_27);
  child_27?0;
  stop_process: skip;
  child_21!0;
}
proctype fun5(chan c28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
  if
  :: true ->
    c28!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c28!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c28!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c28!0;
    goto stop_process;
  :: true ->


  fi;
  c28!0;
  stop_process: skip;
  child_29!0;
}
proctype receiver(chan c) {
  c?0;
}