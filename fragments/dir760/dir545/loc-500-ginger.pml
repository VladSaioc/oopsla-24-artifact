// git_link=loc-500-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
  run fun4(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = x3;
  int y15 = x2;
  int y16 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [y16] of {int};
  chan c19 = [y15] of {int};
  for(y10 : 0 .. (y15) - (1)) {
    for10: skip;
    if
    :: true ->
      goto for10_end;
    :: true ->


    fi;
    run fun5(c18, c19, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y14) - (1)) != (-(3)) ->
    for(y10 : 0 .. (y14) - (1)) {
      for20: skip;
      c19?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c19?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun5(chan c23; chan c24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  c23!0;
  c24!0;
  c23?0;
  stop_process: skip;
  child_25!0;
}
proctype receiver(chan c) {
  c?0;
}