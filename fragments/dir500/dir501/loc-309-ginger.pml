// git_link=loc-309-ginger.pml
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
      if
      :: true ->
        if
        :: true ->
          goto for25_end;
        :: true ->
          if
          :: true ->
            goto for25_end;
          :: true ->
            goto stop_process;

          fi;

        fi;

      fi;
      for23_end: skip;
    };
    for23_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c15?0;
      if
      :: true ->
        if
        :: true ->
          goto for22_end;
        :: true ->
          if
          :: true ->
            goto for22_end;
          :: true ->
            goto stop_process;

          fi;

        fi;

      fi;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
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
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(chan c22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  c22!0;
  stop_process: skip;
  child_23!0;
}
proctype receiver(chan c) {
  c?0;
}