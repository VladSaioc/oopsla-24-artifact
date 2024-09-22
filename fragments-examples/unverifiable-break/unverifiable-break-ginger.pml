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
  run fun2(x1, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(int y5; chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
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
  chan c15 = [y5] of {int};
  for(y9 : 0 .. (y5) - (1)) {
    for10: skip;
    run fun3(c15, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y5) - (1)) != (-(3))) ->
    for(y9 : 0 .. (y5) - (1)) {
      for21: skip;
      c15?0;
      if
      :: true ->
        break;
      :: true ->


      fi;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c15?0;
      if
      :: true ->
        break;
      :: true ->


      fi;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  if
  :: true ->
    c18!0;
  :: true ->
    if
    :: true ->
      c18!0;
    :: true ->
      if
      :: true ->
        c18!0;
      :: true ->
        if
        :: true ->
          c18!0;
        :: true ->
          if
          :: true ->
            c18!0;
          :: true ->
            c18!0;

          fi;

        fi;

      fi;

    fi;

  fi;
  stop_process: skip;
  child_19!0;
}
proctype receiver(chan c) {
  c?0;
}
